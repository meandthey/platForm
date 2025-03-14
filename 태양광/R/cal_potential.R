library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(readr)

km2_to_m2 <- 10^(6)
TWh_to_kWh <- 10^(9)
kor_area <- 100210 #km2

# 대한민국 영토 면적: 100,210 (km2)

rawData_irr_filePath <- "C:/Users/DESKTOP/Desktop/allData/공공데이터포털"
rawData_mapping_latlong_to_SGG <- readxl::read_excel(paste0(rawData_irr_filePath,'./한국에너지기술연구원_신재생자원지도데이터_태양자원_천리안1호_수평면전일사량_20191231_edit_bySGG.xlsx' ), sheet = "데이터")

# 2012~2019 # 8 years
rawData_irr_2012_to_2019 <- readr::read_csv(paste0(rawData_irr_filePath,'./한국에너지기술연구원_신재생자원지도데이터_태양자원_천리안1호_수평면전일사량_20191231.csv'), locale = locale(encoding = 'EUC-KR'))

irr_2012_to_2019 <- rawData_irr_2012_to_2019 %>%
  gather(-위도, -경도, key = YYYYMM, value = value) %>%
  

  
  mutate(YYYY = case_when(
    
    grepl('2012', YYYYMM) ~ '2012', 
    grepl('2013', YYYYMM) ~ '2013', 
    grepl('2014', YYYYMM) ~ '2014', 
    grepl('2015', YYYYMM) ~ '2015', 
    grepl('2016', YYYYMM) ~ '2016', 
    grepl('2017', YYYYMM) ~ '2017', 
    grepl('2018', YYYYMM) ~ '2018', 
    grepl('2019', YYYYMM) ~ '2019',
    TRUE ~ 'else'
    
  )) %>%
  
  mutate(value_per_month = case_when(
    
    grepl('-01', YYYYMM) ~ value * 31,
    grepl('-02', YYYYMM) ~ value * 28,
    grepl('-03', YYYYMM) ~ value * 31,
    grepl('-04', YYYYMM) ~ value * 30,
    grepl('-05', YYYYMM) ~ value * 31,
    grepl('-06', YYYYMM) ~ value * 30,
    grepl('-07', YYYYMM) ~ value * 31,
    grepl('-08', YYYYMM) ~ value * 31,
    grepl('-09', YYYYMM) ~ value * 30,
    grepl('-10', YYYYMM) ~ value * 31,
    grepl('-11', YYYYMM) ~ value * 30,
    grepl('-12', YYYYMM) ~ value * 31
    
  )) %>%
  
  left_join(rawData_mapping_latlong_to_SGG, by = c('위도', '경도')) %>%
  drop_na() 

## Area covered by a dot
unique_Dots <- irr_2012_to_2019 %>%
  distinct(위도, 경도, .keep_all = T)

area_coveredByDot <- kor_area / nrow(unique_Dots) # km2

irr_2012_to_2019_full <- irr_2012_to_2019 %>%
  mutate(value_per_month_per1.46km2_ = value_per_month * area_coveredByDot * km2_to_m2) %>%
  mutate(YYYYMM =  gsub("\\(", "", YYYYMM)) %>%
  mutate(YYYYMM =  gsub("\\)", "", YYYYMM)) %>%
  mutate(YYYYMM = gsub(" kWh per m2 per day", "", YYYYMM)) %>%
  select(-value) %>%
  rename(value = value_per_month_per1.46km2_) %>%
  select(위도, 경도, YYYYMM, YYYY, value, CTP_KOR_NM, SIG_KOR_NM)
  


## 잠재량 check
#### National #### unit: TWh/year

irr_2012_to_2019_byNat_byYear <- irr_2012_to_2019_full %>%
  group_by(YYYY) %>% summarize(value = sum(value)) %>% ungroup() %>%
  mutate(value = value / TWh_to_kWh)

#### Provincial #### unit: TWh/year
irr_2012_to_2019_byPrv_byYear <- irr_2012_to_2019_full %>%
  group_by(YYYY, CTP_KOR_NM) %>% summarize(value = sum(value / TWh_to_kWh)) %>% ungroup()


#### SGG #### unit: TWh/year
irr_2012_to_2019_bySGG_byYear <- irr_2012_to_2019_full %>%
  group_by(YYYY, CTP_KOR_NM, SIG_KOR_NM) %>% summarize(value = sum(value / TWh_to_kWh)) %>% ungroup()

irr_2012_to_2019_bySGG_byYear %>%
  distinct(CTP_KOR_NM, SIG_KOR_NM)

irr_2012_to_2019_full %>%
  filter(YYYY==2019,
         CTP_KOR_NM =='경기도')






  




irr_2012_to_2019_interr <- irr_2012_to_2019 %>%
  left_join(rawData_mapping_latlong_to_SGG, by = c('위도', '경도')) %>%
  drop_na()


  group_by(YYYY) %>% summarize(value = sum(value))
  

  


rawData_irr_2012_to_2019 <- read.csv(paste0(rawData_irr_filePath,'./한국에너지기술연구원_신재생자원지도데이터_태양자원_천리안1호_수평면전일사량_20191231.csv'))

readr::read_csv(paste0(rawData_irr_filePath,'./한국에너지기술연구원_신재생자원지도데이터_태양자원_천리안1호_수평면전일사량_20191231.csv'), locale = locale(encoding = 'EUC-KR'))

rawData_Sheets <- excel_sheets(paste0(rawData_filePath, '2019년 신재생에너지 보급통계_시군구.xlsx'))  # 어차피 다 같으니 2019년을 대표로 선택해서 sheet list 추출.


########## PV Data ##########
rawData_years <- c(2019, 2020, 2021, 2022)
rawData_filePath <- '../'
rawData_Sheets <- excel_sheets(paste0(rawData_filePath, '2019년 신재생에너지 보급통계_시군구.xlsx'))  # 어차피 다 같으니 2019년을 대표로 선택해서 sheet list 추출.
rawData_Sheets_SIDO <- c("서울", "부산", "대구", "인천", "광주", "대전", "울산", "세종", "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주")
rawData_Gen_Sheets <- rawData_Sheets[grepl("발전량_", rawData_Sheets)]


rawData_cellRange_bySGG <- readxl::read_excel('./cellRange_SGG.xlsx', sheet = "data")   
rawData_cellRange_byTech <- readxl::read_excel('./cellRange_Tech.xlsx', sheet = "data")   
