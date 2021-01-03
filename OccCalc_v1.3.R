#================================== OccCalc v1.3 =====================================
# This program calculates occupancy of buses based on transit card data.
# <MODIFICATIONS>
# 노선번호 끝에 '번' 추가 (케플러에서 string 형태로 읽을 수 있도록)
# 시간대를 (케플러에서 시계열로 띄울 수 있도록) datetime 형태로 나타냄
# 버스 노선별 geojson 코드 도출
# 운행횟수, 평균배차간격 도출
# 시간대별 파일분할은 없앰(필요없을거같음)
#
# First made: 8 Mar 2018
# Last Modification (v1.3): 10 Dec 2020
#======================================================================================
library(data.table)
library(timeSeries)
library(dplyr)
library(imputeTS)
library(zoo)
library(here)
library(bit64)
library(lubridate)

# ========== R 스크립트 파일의 디렉토리를 working directory로 설정 ==========
setwd(here())

# ========== 스마트카드 데이터 불러오기 ==========
total_bus <- c() # 전체 데이터를 넣을 데이터프레임
for(day in c(16,17,18,19,22)){
  datapath <- "D:/01_data/" # 스마트카드 데이터가 들어있는 폴더의 경로를 여기에 넣을 것!
  busdata <- fread(paste0(datapath, "05", as.character(day), ".txt"))
  busdata <- subset(busdata, 교통수단코드<200 | 교통수단코드>=300) # 버스만 추출
  total_bus <- rbind(total_bus, busdata)
}
rm(busdata)
total_bus <- total_bus[,c("차량등록번호", "운행출발일시", "노선ID_정산사업자", "승차일시", "승차정류장ID_교통사업자", "하차일시", "하차정류장ID_정산사업자", "이용객수_다인승")]
names(total_bus) <- c("vehicle","departure","line","boardtime","boardstation","alighttime","alightstation","multiple")

# ========== 버스 노선 및 정류장 목록 불러오기 ==========
buslist<-read.csv("Buslist.csv")
stationlistAll<-read.csv("Stationlist.csv")

# ========== totalresult: 전처리 결과, basicstat: 기초통계량, geojson: geojson ==========
totalresult <- c()
basicstat <- c()
geojson <- c()

# ========== 노선별 반복 ==========
for(i in 1:nrow(buslist)){
  tryCatch({

    # ========== 대상 노선의 노선ID 및 노선번호 ==========
    line_ID=buslist[i,1] # 노선ID
    line_NO=as.character(buslist[i,3]) # 노선번호
    oneline <- subset(total_bus,total_bus$line == as.character(line_ID)) # Get bus number wanted
    oneline$departure <- as.POSIXct(as.character(oneline$departure),"%Y%m%d%H%M%S", tz="Asia/Seoul") # convert time to POSIXct format
    oneline$boardtime <- as.POSIXct(as.character(oneline$boardtime),"%Y%m%d%H%M%S", tz="Asia/Seoul") # convert time to POSIXct format. 시간 단위로 반올림
    oneline$alighttime <- as.POSIXct(as.character(oneline$alighttime),"%Y%m%d%H%M%S", tz="Asia/Seoul") # convert time to POSIXct format. 시간 단위로 반올림
    oneline <- transform(oneline, dispatch=as.numeric(factor(departure))) # grouping by dispatch order
    oneline$alight_tag <- oneline$alightstation!="~" # TRUE if the passenger tagged his/her card when alight
    
    # ========== 정류장 순서(몇 번째 정류장인지) 추가 ==========
    stationlist <- subset(stationlistAll, LineID==as.character(line_ID))
    stationlist <- unique(stationlist) # 공동배차 노선인 경우 중복이 있어서 그것을 제거함
    oneline$boardstation <- as.integer(oneline$boardstation)
    oneline <- merge(oneline, stationlist[,c("StationID","StationNumber")], by.x="boardstation", by.y="StationID", all.x=T)
    colnames(oneline)[which(names(oneline) == "StationNumber")] <- "boardorder"
    oneline$alightstation <- as.integer(oneline$alightstation)
    oneline <- merge(oneline, stationlist[,c("StationID","StationNumber")], by.x="alightstation", by.y="StationID", all.x=T)
    colnames(oneline)[which(names(oneline) == "StationNumber")] <- "alightorder"

    # ========== 스마트카드 데이터의 승차와 하차를 분리 ==========
    record_board <- oneline[,c("departure","dispatch","boardstation","boardorder","boardtime","multiple","alight_tag")]
    names(record_board) <- c("departure","dispatch","stnID","stnorder","time","board","alight_tag")
    record_alight <- oneline[,c("departure","dispatch","alightstation","alightorder","alighttime","multiple","alight_tag")]
    names(record_alight) <- c("departure","dispatch","stnID","stnorder","time","alight","alight_tag")
    
    record <- rbind(record_board, record_alight, fill=T)
    record$board[is.na(record$board)] <- 0
    record$alight[is.na(record$alight)] <- 0

    # ========== 재차인원 산정 ==========
    record$diff_tot <- record$board - record$alight
    record$diff_tag[record$alight_tag==T] <- record$diff_tot[record$alight_tag==T] # include non-alight-tag
    record$diff_tag[record$alight_tag==F] <- 0 # exclude non-alight-tag
    record <- record[order(record$dispatch, record$time),] # sort by dispatch and time
    record$occ_tot <- ave(record$diff_tot, record$dispatch, FUN=cumsum) # include non-alight-tag
    record$occ_tag <- ave(record$diff_tag, record$dispatch, FUN=cumsum) # exclude non-alight-tag
    record<-record[,c('departure','dispatch','stnID','stnorder','time','board','alight','diff_tot','diff_tag','alight_tag','occ_tot','occ_tag')]

    # ========== 시간을 시간대(timeslot)로 환산 ==========
    record$timeslot <- floor(((as.numeric(record$time) + 9*3600) %% 86400) / 3600)
    
    # ========== 각 운행회차(dispatch) 단위로 정류장별 재차인원 정리 ==========
    occ_all <- c()

    for(j in unique(record$dispatch)){
      
      # 해당 dispatch의 record 추출
      recordSS <- subset(record, dispatch==j)
      recordSS <- subset(recordSS, !is.na(stnorder))
      
      # 다음 정류장으로 떠나는 순간에 대한 record만 남김
      for(k in 1:(nrow(recordSS)-1)){
        recordSS$tonextstop[k] <- recordSS$stnorder[k] != recordSS$stnorder[k+1]
      }
      recordSS$tonextstop[nrow(recordSS)] <- FALSE # 마지막줄은 FALSE
      recordSS <- subset(recordSS, tonextstop == TRUE)
      occ_all <- rbind(occ_all, recordSS)
    }
    
    # 승하차인원은 시간대별로 합산(5일치 데이터라서 5로 나눔), 재차인원은 시간대별로 평균
    occ_hour <- occ_all %>% group_by(timeslot, stnorder) %>% summarise(board=sum(board)/5, alight=sum(alight)/5, occ = mean(occ_tot))
    
    # ========== 전처리결과(result 데이터프레임) 만들기 =========
    # 노선 링크목록 만들기
    linklist<-cbind(stationlist, rbind(stationlist[-1,-1],NA))
    colnames(linklist) <- c("LineID", "S.ID", "S.Number", "S.Name", "S.Distance", "S.CoordX", "S.CoordY", "E.ID", "E.Number", "E.Name", "E.Distance", "E.CoordX", "E.CoordY")
    linklist$LinkLength <- abs(linklist$S.Distance-linklist$E.Distance)
    linklist<-linklist[-nrow(linklist),]
    linklist<-linklist[,-c(5,11)]
    linklist$S.CoordX <- floor(linklist$S.CoordX/100)+((linklist$S.CoordX/100)-floor(linklist$S.CoordX/100))*100/60 #좌표 변환(deg 단위)
    linklist$S.CoordY <- floor(linklist$S.CoordY/100)+((linklist$S.CoordY/100)-floor(linklist$S.CoordY/100))*100/60 #좌표 변환(deg 단위)
    linklist$E.CoordX <- floor(linklist$E.CoordX/100)+((linklist$E.CoordX/100)-floor(linklist$E.CoordX/100))*100/60 #좌표 변환(deg 단위)
    linklist$E.CoordY <- floor(linklist$E.CoordY/100)+((linklist$E.CoordY/100)-floor(linklist$E.CoordY/100))*100/60 #좌표 변환(deg 단위)
    linklist$Geom <- paste0("LINESTRING(",linklist$S.CoordX," ",linklist$S.CoordY,",",linklist$E.CoordX," ",linklist$E.CoordY,")")
    linklist <- cbind(line_NO, linklist)
    linklist <- rename(linklist, "LineNo"="line_NO")
    
    # 0시부터 23시까지 링크목록 반복
    result <- c()
    for(hr in 0:23){
      linklist$timeslot <- hr
      result <- rbind(result, linklist)
    }
    
    # result 데이터프레임에 재차인원, 승하차인원 합치기
    result <- left_join(result, occ_hour, by=c("S.Number"="stnorder", "timeslot"))

    # 시간대별로 해당 시간대에 버스가 실제로 운행한 구간만 남기기(첫차, 막차 시간대 고려)
    result$passed <- TRUE
    for(ts in unique(result$timeslot)){
      temp <- subset(result, timeslot==ts)
      for(l in 1:nrow(temp)){
        if(is.na(temp$occ[l])){
          temp$passed[l] <- FALSE
        }else{
          break
        }
      }
      for(l in 1:nrow(temp)){
        if(is.na(temp$occ[nrow(temp)-l+1])){
          temp$passed[nrow(temp)-l+1] <- FALSE
        }else{
          break
        }
      }
      result[result$timeslot==ts,] <- temp
    }
    
    # board, alight, occupancy가 NA인 행에 값 채우기
    result <- subset(result, passed==TRUE)
    result$board[is.na(result$board)] <- 0
    result$alight[is.na(result$alight)] <- 0
    for(m in 2:nrow(result)){
      if(is.na(result$occ[m])) result$occ[m] <- result$occ[m-1] + result$board[m]-result$alight[m]
    }
    result$occ <- round(result$occ, 1)
    
    # timeslot을 시간 데이터형으로 환산
    result$timeslot <- paste0(sprintf("%02d", result$timeslot),":00:00")
    
    # 필요한 열만 남긴 뒤 전체 전처리결과(totalresult)에 합치기
    result <- result[,c("LineNo","S.Number","S.Name","E.Number","E.Name","Geom","timeslot","board","alight","occ")]
    names(result) <- c("노선번호", "정류장 순서 1", "정류장 이름 1", "정류장 순서 2", "정류장 이름 2", "Geom", "시간대", "시간당 승차인원", "시간당 하차인원", "평균 재차인원")
    totalresult <- rbind(totalresult, result)
    
    # ========== 기초통계량 산출 ==========
    basicstat <- rbind(basicstat, c(line_NO, max(oneline$dispatch)/5, sum(oneline$multiple)/5))
    
    # ========== geojson 만들기 ==========
    geojson_line <- '{"type":"Feature","geometry":{"type":"LineString","coordinates":['
    stationlist$CoordX <- floor(stationlist$CoordX/100)+((stationlist$CoordX/100)-floor(stationlist$CoordX/100))*100/60 #좌표 변환(deg 단위)
    stationlist$CoordY <- floor(stationlist$CoordY/100)+((stationlist$CoordY/100)-floor(stationlist$CoordY/100))*100/60 #좌표 변환(deg 단위)
    for(n in 1:nrow(stationlist)){
      timestamp <- 1564184363 + n*60
      geojson_line <- paste0(geojson_line,"[",stationlist$CoordX[n],",",stationlist$CoordY[n],",",0,",",timestamp,"]")
      if(n < nrow(stationlist)){
        geojson_line <- paste0(geojson_line, ",")
      }
    }
    geojson_line <- paste0(geojson_line, "]}}")
    geojson <- rbind(geojson, c(line_NO, geojson_line))
    
  }, error=function(e){print(paste0("error in ",line_ID," ",line_NO))})
}

# ========= 전처리 결과 저장 =========
write.csv(totalresult, 'Result_1.3/preprocessed_201210.csv', row.names=F)

# ========= 일일 운행횟수 및 이용객수 저장 =========
basicstat <- as.data.frame(basicstat)
names(basicstat) <- c("노선번호", "일 운행횟수", "일 이용객수")
write.csv(basicstat, 'Result_1.3/dailyfreqridership_201210.csv', row.names=F)

# ========= geojson 저장 =========
geojson <- as.data.frame(geojson)
names(geojson) <- c("노선번호", "geojson")
write.csv(geojson, 'Result_1.3/geojson_201210.csv', row.names=F)