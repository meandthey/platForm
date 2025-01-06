library(dplyr)

## 지번별 전력사용량 데이터 (세움터 2020년 1월~12월)


rawData <- read.csv("C:/Users/Jeon/Desktop/allData/platform/나인와트/안양시sample/mtr_[별도건축물_전체_양식]지역추출-sigungu_cd_41171-Matched_2020_mtr.csv", header = T, sep = "|")

unique(rawData$ENGY_SPLY_KIK_CD)
rawData_elec <- rawData %>%
  filter(grepl("11", ENGY_SPLY_KIK_CD))

sum(rawData_elec$USE_QTY) / 10^(9)

rawData_gas <- rawData %>%
  filter(grepl("12", ENGY_SPLY_KIK_CD))

rawData_elec <- rawData %>%
  filter(grepl("11", ENGY_SPLY_KIK_CD))



rawData_GG <- rawData %>%
  filter(grepl("경기도", ENTIR_ADDR  ))






######## 세움터 2020년 06월 ########
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

rawData_SUT_2020 <- c()

for ( i in 1:length(months)) {
  
  filePath <- "C:/Users/Jeon/Desktop/allData/세움터/지번별 데이터/전기/2020/"
  fileName <- paste0('eais_elcty_2020', months[i], '.txt'  )
  fullPath <- paste0(filePath, fileName)
  rawData_SUT_2020_each <- read.csv(fullPath, header = F, fileEncoding = "EUC-KR", sep = "|")
 
  rawData_SUT_2020 <- rawData_SUT_2020 %>% bind_rows(rawData_SUT_2020_each) 
}


rawData_SUT_colNames <- c('사용년월',
                          "순번",
                          "시군구코드",
                          "법정동코드",
                          "시도명",
                          "시군구명",
                          "법정동명",
                          "대지구분코드",
                          "본번",
                          "부번",
                          "새주소대로로코드",
                          "새주소대로로명",
                          "새주소지상지하코드",
                          "새주소지상지하명",
                          "새주소본번",
                          "새주소부번",
                          "사용량")

colnames(rawData_SUT_2020) <- rawData_SUT_colNames

## 안양시 ##
rawData_SUT_2020_GG <- rawData_SUT_2020 %>%
  filter(grepl("경기도", 시도명)) %>%
  filter(순번 == 1)

sum(rawData_SUT_2020_GG$사용량) / 10^(9)


## 안양시 ##
rawData_SUT_2020_GG_Anyang <- rawData_SUT_2020 %>%
  filter(grepl("안양시", 시군구명)) %>%
  filter(순번 == 1)

sum(rawData_SUT_2020_GG_Anyang$사용량) / 10^(9)






# test_rawData <- rawData %>%
#   slice(1:1000)
# 
# testExample_rawData <- rawData %>%
#   filter(ENTIR_ADDR == "경기도 안양시 만안구 안양동 371번지")
# 
# write.csv(testExample_rawData, "testExample_rawData.csv")
# 
# 
# filter(row_number<=10)
# top_n(11)
# 
# 
# 
# 
# 
# rawData_SUT <- read.csv("../세움터/eais_elcty_202001.txt", header = F, fileEncoding = "EUC-KR", sep = "|")
# rawData_SUT_colNames <- c('사용년월',
#                           "순번",
#                           "시군구코드",
#                           "법정동코드",
#                           "시도명",
#                           "시군구명",
#                           "법정동명",
#                           "대지구분코드",
#                           "본번",
#                           "부번",
#                           "새주소대로로코드",
#                           "새주소대로로명",
#                           "새주소지상지하코드",
#                           "새주소지상지하명",
#                           "새주소본번",
#                           "새주소부번",
#                           "사용량")
# 
# colnames(rawData_SUT) <- rawData_SUT_colNames

