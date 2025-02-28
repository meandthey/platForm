library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(readr)

data_years <- c(2024:2018)
data_issues <- c(93:87)
data_colNames <- c("empty", "발전소명", "용량_kW", "발전량_MWh", "평균전력_kW", "최대전력_kW", "부하율_%", "이용률_%", "소내전력량_MWh", "소내전력률_%", "송전단전력량_MWh", "plantsName")
mapping_plants_fuel <- readxl::read_excel('../mappingData/mapping_발전소_연료.xlsx', sheet = '2023', col_names = T)

rawData_Byplants_SGG <- readxl::read_excel('../mappingData/3_소별발전_byGCAM-EML.xlsx', col_names = T, sheet = 'data')
Byplant_SGG <- rawData_Byplants_SGG %>%
  mutate(발전기번호 = case_when(
    
    is.na(발전기번호) ~ "",
    TRUE ~ 발전기번호
    
  )) %>%
  mutate(발전소명 = paste0(발전소명, 발전기번호)) %>%
  rename(시도 = 광역시도) %>%
  select(시도, 시군구, 발전소명) %>%
  distinct(.keep_all = T)

finalData <- c()
for ( i in 1:length(data_years)) {
  
  data_fullnames_each <- paste0("../",data_years[i], "년도판 한국전력통계(제", data_issues[i], "호).xlsx")
  
  rawData_Byplants_each <- readxl::read_excel(data_fullnames_each, sheet = '3.소별 발전실적', col_names = F)
  
  colnames(rawData_Byplants_each) <- data_colNames
  
  Byplant_gen_byFuel_each <- rawData_Byplants_each %>%
    left_join(mapping_plants_fuel, by = c("발전소명")) %>%
    filter(!is.na(연료)) %>%
    mutate(용량_kW = as.numeric(용량_kW),
           발전량_MWh = as.numeric(발전량_MWh)) %>%
    mutate(용량_kW = replace(용량_kW, is.na(용량_kW), 0),
           발전량_MWh = replace(발전량_MWh, is.na(발전량_MWh), 0)) %>%
    mutate(year = c(data_years[i]-1))
  

  
  finalData_each <- Byplant_gen_byFuel_each %>%
    left_join(Byplant_SGG, by = c('발전소명')) %>%
    select(year, 시도, 시군구, 발전소명, 연료, 발전방식, 용량_kW, 발전량_MWh)
  
  finalData <- finalData %>% bind_rows(finalData_each)
}

#unique(paste0(finalData_each$시도, finalData_each$시군구))
#unique(paste0(finalData$시도, finalData$시군구))





rawData_renewGen <- read.csv('../../../KEA/신재생에너지 보급통계/outputData/finalData.csv', header = T, fileEncoding = "EUC-KR")

data_renewGen <- rawData_renewGen %>%
  group_by(year, SIDO, SGG, technology) %>% summarize(value = sum(value)) %>% ungroup() %>%
  mutate(연료 = technology,
         발전방식 = technology,
         용량_kW = 0) %>%
  rename(시도 = SIDO,
         시군구 = SGG,
         발전소명 = technology,
         발전량_MWh = value) %>%
  select(year, 시도, 시군구, 발전소명, 연료, 발전방식, 용량_kW, 발전량_MWh)

  
  
finalData_wReGen <- finalData %>%
  bind_rows(data_renewGen) 
  

write.csv(finalData_wReGen, "../outputData/finalData_wReGen.csv", fileEncoding = "EUC-KR", row.names = F)

## just check ##
# finalData_wReGen %>%
#   group_by(year) %>% summarize(발전량_MWh = sum(발전량_MWh))
# 
# finalData_wReGen %>%
#   filter(시도 == '경기') %>%
#   group_by(year) %>% summarize(발전량_MWh = sum(발전량_MWh))
# 
# 
# finalData_wReGen %>%
#   filter(시도 == '경기')








 

#rawData_Byplants_full <- readxl::read_excel('2023년 한국전력통계(제93호).xlsx', sheet = '3.소별 발전실적', col_names = F)

 
#replace(rawData_Byplants_full$발전량_MWh, is.na(rawData_Byplants_full$발전량_MWh), 0)




 








