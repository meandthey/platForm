library(dplyr)

## 지번별 전력사용량 데이터 (세움터 2020년 1월~12월)


rawData <- read.csv("./나인와트/mtr_[별도건축물_전체_양식]지역추출-sigungu_cd_41171-Matched_2020_mtr.csv", header = T, sep = "|")

sum(rawData$USE_QTY) / 10^(6)

rawData_GG <- rawData %>%
  filter(grepl("경기도", ENTIR_ADDR  ))



test_rawData <- rawData %>%
  slice(1:1000)

testExample_rawData <- rawData %>%
  filter(ENTIR_ADDR == "경기도 안양시 만안구 안양동 371번지")

write.csv(testExample_rawData, "testExample_rawData.csv")


filter(row_number<=10)
top_n(11)





rawData_SUT <- read.csv("../세움터/eais_elcty_202409.txt", header = F, fileEncoding = "EUC-KR", sep = "|")
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

colnames(rawData_SUT) <- rawData_SUT_colNames


######## 세움터 2020년 06월 ########
rawData_2020_06_SUT <- read.csv("../세움터/지번별_전기_민간개방용구조_202006.txt", header = F, fileEncoding = "EUC-KR", sep = "|")
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

colnames(rawData_2020_06_SUT) <- rawData_SUT_colNames

rawData_2020_06_SUT_GG <- rawData_2020_06_SUT %>%
  filter(grepl("경기", 시도명))

sum(rawData_2020_06_SUT_GG$사용량) / 10^(6)








