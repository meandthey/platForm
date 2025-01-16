library(dplyr)
library(readr)
library(rlang)
library(tidyr)
library(ggplot2)
library(hablar)
library(stringr)
library(openxlsx)
library(httr)
library(XML)
library(jsonlite)

BASE_url <- "apis.data.go.kr/B551184/openapi/service/SolarGhiService"
call_address <- "/getSolarGhiHrInfo"
call_url <- paste0(BASE_url, call_address)
  
serviceKey 
pageNo 
numOfRows 
date 
lat 
lon 




## ChatGPT ##
# API endpoint 및 파라미터 설정
#base_url <- "https://apis.data.go.kr/B551184/openapi/service/SolarGhiService/getSolarGhiHrInfo"
base_url <- "https://apis.data.go.kr/B551184/openapi/service/SolarGhiService/getSolarGhiHrInfo?serviceKey=6%2BC8dzngGzHDrDj6PTGb38rnq7vm2xC9ShMhDPA9JT1m352jgOvtCAzyyOqS9FgnV55SM9ZPPT8mkZ57N0YZmQ%3D%3D&pageNo=1&numOfRows=10&date=20211110&lat=33.202807954&lon=126.26336105&type=json&time=1300"
#serviceKey <- "6%2BC8dzngGzHDrDj6PTGb38rnq7vm2xC9ShMhDPA9JT1m352jgOvtCAzyyOqS9FgnV55SM9ZPPT8mkZ57N0YZmQ%3D%3D"
# date <- "20211201"
# lat <- "35.0"
# lon <- "128.0"
# #time <- "1200"
# numOfRows <- "10"
# pageNo <- "1"
# type <- "json"

# GET 요청 생성 및 실행
response <- GET(url = base_url, query = list(serviceKey = serviceKey, pageNo = pageNo, numOfRows = numOfRows, date = date, lat = lat, lon = lon))
response <- GET(url = base_url)

# 응답 확인
status <- status_code(response)
if (status == 200) {
  # 응답 내용을 텍스트로 변환 후 JSON으로 파싱
  content_text <- content(response, "text", encoding = "UTF-8")
  data <- fromJSON(content_text)
  print(data)
} else {
  print(paste("Request failed with status", status))
}


# Print raw response text
content_text <- content(response, "text", encoding = "UTF-8")
print(content_text)
