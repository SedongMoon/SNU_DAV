# SNU_DAV
This is a repository for the "Data Analysis and Visualization" class term project of 2020 Fall Semester in Seoul National University Graduate School of Data Science.
이 repository는 2020년 2학기 서울대학교 데이터사이언스대학원 '데이터 분석과 시각화' 수업의 텀프로젝트를 위한 repository입니다.

# Spatio-Temporal Visualization of Bus Line’s Congestion Level in Seoul
## 서울시 버스 노선 혼잡도의 시공간적 시각화
by Team TRLAB

# 0. About the project
Visualization of spatiotemporal variations in crowding of buses in Seoul, South Korea
- Calculation of crowdedness and ridership by time and bus lines through data pre-processing
- Visualization of spatiotemporal variations in crowding of bus lines to observe recurrent crowded sections
- User convenience of information acquisition through interaction

# 1. Pre-Processing
Pre-processing is conducted by 01_preprocessing.R. This processes smartcard data into occupancy level and ridership by time, bus lines, and sections.

## Language
R

## Development Environment
- R >= 3.6.2
- RStudio >= 1.2.5033

## Packages
 - data.table
 - timeSeries
 - dplyr
 - imputeTS
 - zoo
 - here
 - bit64
 - lubridate

## Inputs
 - Smartcard data: Because the original data is large (>4 GB/day), only the sample data is uploaded here(smartcarddata_sample.csv).
 - List of buses: Buslist.csv
 - List of bus stops: Stationlist.csv
 
## Output
 - Pre-processed data: preprocessed.csv. Because the pre-processed data is large (~102 MB), it is compressed into preprocessed.zip. You can open the original file after unzipping it.

# 2. Visualization
The main visualization is conducted by 02_visualization.ipynb. This visualizes the pre-processed data.

## Language
Jupyter Notebook

## Development Environment
- conda >= 4.9.2
- Jupyter Notebook >= 6.1.5

## Packages
- keplergl >= 0.2.2
- pandas >= 1.1.5

## Input
 - Pre-processed data: preprocessed.csv. Because the pre-processed data is large (~102 MB), it is compressed into preprocessed.zip. You can open the original file after unzipping it.
 
## Output
 - The final result: CrowdingVisualization.html. Because the file is large (~155 MB), it is compressed into CrowdingVisualization.zip. You can open the original file after unzipping it.
