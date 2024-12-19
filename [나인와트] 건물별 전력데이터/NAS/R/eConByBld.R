library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(tidyverse)

## 2022년 경기도 부문별 전력소비량 (KTOE)
# KTOE to TWh = 0.01163
# total: 12,086 (140.56TWh)
# ind: 6,369 (74.07TWh)
# trn: 115 (1.33TWh)
# resid & commercial: 4,741 (55.14TWh)
# public & others: 861 (10.01 TWh)
# c(4741+861) * 0.01163 = 65.15 (TWh)
#12086*0.01163

Data_colnames <- c("사용_년월", "시도", 
                   "주소1", "주소2","주소3","주소4","주소5","주소6",
                   "주소full", "도로명_대지_위치", "시군구_코드", "법정동_코드", "대지_구분_코드", "번", "지", "새주소_일련번호", "새주소_도로_코드", "새주소_지상지하_코드", "새주소_본_번",
                   "새주소_부_번", "사용_량(kWh)")

getData <- function() {
  
  rawData_full <- c()
  filePath <- c("../data/경기도_지번별_에너지사용량_23년전체.xlsx")
  months <- formatC(seq(1, 12), width=2, flag = 0)
  sheetNames <- paste0("23.", months)
  

  
  for ( i in 1:length(months)) {
    
    rawData_eachMonth <- readxl::read_excel(filePath,
                                            sheet = sheetNames[i],
                                            col_names = T)
    
    rawData_full <- rawData_full %>% bind_rows(rawData_eachMonth)
    
    #colnames(rawData_full) <- Data_colnames
    
  }
  
  return(rawData_full)
}


rawData <- getData()
colnames(rawData) <- Data_colnames

rawData <- rawData %>%
  mutate(사용년월_주소full = paste0(사용_년월, 주소full))



# 중복되는 주소들 제거. #
rawData_temp_for_duplicated <- rawData %>%
  filter(duplicated(사용년월_주소full))

dupCases <- unique(rawData_temp_for_duplicated$사용년월_주소full)

unique_dupData <- rawData %>%
  filter(사용년월_주소full %in% dupCases) %>%
  distinct(., 사용년월_주소full, .keep_all = TRUE)


no_dupData <- rawData %>%
  filter(!사용년월_주소full %in% dupCases)

rawData_unq <- no_dupData %>%
  bind_rows(unique_dupData)

rawData_totalkWh <- sum(rawData_unq$`사용_량(kWh)`)

rawData_unq_byBJ <- rawData_unq %>%
  group_by(주소full) %>% summarize(`사용_량(kWh)` = sum(`사용_량(kWh)`)) %>% ungroup()

rawData_unq_byBJ_byCum <- rawData_unq_byBJ %>%
  arrange(desc(`사용_량(kWh)`)) %>%
  mutate(`누적사용_량(kWh)` = cumsum(`사용_량(kWh)`)) %>%
  mutate(`개별비중` = `사용_량(kWh)` / rawData_totalkWh,
         `누적비중` = `누적사용_량(kWh)` / rawData_totalkWh) %>%
  mutate(rowNum = row_number())

################################################################

ggplot(rawData_unq_byBJ_byCum, aes(y = `누적비중`, x = rowNum)) +
  geom_line() 








rawData_unq_byBJ_forGraph <- rawData_unq_byBJ %>%
  arrange(desc(`사용_량(kWh)`)) %>%
  mutate(rowNum = row_number()) %>%
  mutate(proportion = `사용_량(kWh)` / sum(`사용_량(kWh)`))



rawData_unq_byBJ_above1000 <- rawData_unq_byBJ %>%
  filter(rowNum >= 1000)

  

ggplot(rawData_unq_byBJ_above1000, aes(y = `사용_량(kWh)`, x = rowNum)) +
  geom_line() 







sum(rawData_unq_descOD$`사용_량(kWh)`) / 2
rawData_unq_descOD <- rawData_unq %>%
  arrange(desc(`사용_량(kWh)`)) %>%
  mutate(rowNum = row_number())



ggplot(rawData_unq_descOD, aes(y = `사용_량(kWh)`, x = rowNum)) +
  geom_line() + 
  facet_wrap(~사용_년월)


ggplot(rawData_unq_descOD, aes(y = `사용_량(kWh)`, x = rowNum)) +
  #geom_bar(aes(fill = energyType), position = 'stack', stat = 'identity') +
  geom_bar( position = 'stack', stat = 'identity')
  facet_wrap(~province) +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size = 50))














