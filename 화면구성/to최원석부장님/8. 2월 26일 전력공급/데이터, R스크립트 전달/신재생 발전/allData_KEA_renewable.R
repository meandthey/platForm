library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)




########## PV Data ##########
rawData_years <- c(2019, 2020, 2021, 2022)
rawData_filePath <- '../'
rawData_Sheets <- excel_sheets(paste0(rawData_filePath, '2019년 신재생에너지 보급통계_시군구.xlsx'))  # 어차피 다 같으니 2019년을 대표로 선택해서 sheet list 추출.
rawData_Sheets_SIDO <- c("서울", "부산", "대구", "인천", "광주", "대전", "울산", "세종", "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주")
rawData_Gen_Sheets <- rawData_Sheets[grepl("발전량_", rawData_Sheets)]


rawData_cellRange_bySGG <- readxl::read_excel('./cellRange_SGG.xlsx', sheet = "data")   
rawData_cellRange_byTech <- readxl::read_excel('./cellRange_Tech.xlsx', sheet = "data")   


cellRagne_bySGG <- rawData_cellRange_bySGG %>%
  mutate(range = paste0(col_first, row_SGG, ":",col_last, row_SGG))


#cellRange_byTech <- 

  
finalData <- c()

for ( i in 1:length(rawData_years)) {
  
  for ( j in 1:length(rawData_Sheets_SIDO)) {
    
    for ( k in 1:length(rawData_cellRange_byTech$technology)) {
      
      
      ## import each province's SGG (i.e. columnName)
      rawData_gen_colname_each <- readxl::read_excel(paste0(rawData_filePath, rawData_years[i],'년 신재생에너지 보급통계_시군구.xlsx'), 
                                                     sheet = rawData_Gen_Sheets[j], 
                                                     col_names = T, 
                                                     range = cellRagne_bySGG$range[j])
      
      
      ## import each province's data by tech (i.e. numeric data)
      cellRange_byTech <- paste0(cellRagne_bySGG$col_first[j], rawData_cellRange_byTech$row_first[k], ":", cellRagne_bySGG$col_last[j], rawData_cellRange_byTech$row_last[k])
        
        
  
      rawData_gen_value_each <-  readxl::read_excel(paste0(rawData_filePath, rawData_years[i],'년 신재생에너지 보급통계_시군구.xlsx'), 
                                                    sheet = rawData_Gen_Sheets[j], 
                                                    col_names = F, 
                                                    range = cellRange_byTech)
      
      colnames(rawData_gen_value_each) <- colnames(rawData_gen_colname_each)
      
      genData_bySIDO_byYear_byTech_each <- rawData_gen_value_each %>%
        mutate(technology = rawData_cellRange_byTech$technology[k],
               SIDO = rawData_Sheets_SIDO[j],
               year = rawData_years[i])
      
      if(rawData_cellRange_byTech$technology[k] %in% c("PV", "wind", "hydro", "bio", "waste", "fuelcell")) {
        
        finalData_each <- genData_bySIDO_byYear_byTech_each %>%
          mutate(purpose = c("전체", "사업용", "자가용"))
        
      } else {
        
        finalData_each <- genData_bySIDO_byYear_byTech_each %>%
          mutate(purpose = c("전체", "사업용"))
        
      }
      
      finalData_each <- finalData_each %>%
        gather(-year, -SIDO, -purpose, -technology, key = 'SGG', value = value) %>%
        select(year, SIDO, SGG, technology, purpose, value)
      
      
      
      finalData <- finalData %>% bind_rows(finalData_each)

    }
    
  }
  
}

finalData <- finalData %>%
  mutate(SGG = case_when(
    
    SGG == '세종' ~ '세종시',
    TRUE ~ SGG
    
  ))




write.csv(finalData, "../outputData/finalData.csv", fileEncoding = "EUC-KR", row.names = F)
