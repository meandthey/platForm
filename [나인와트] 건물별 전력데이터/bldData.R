library(dplyr)

rawData <- read.csv("bld_elec_data.csv", header = T, sep = "|")

test_rawData <- rawData %>%
  slice(1:1000)

testExample_rawData <- rawData %>%
  filter(ENTIR_ADDR == "경기도 안양시 만안구 안양동 371번지")

write.csv(testExample_rawData, "testExample_rawData.csv")


  filter(row_number<=10)
  top_n(11)
