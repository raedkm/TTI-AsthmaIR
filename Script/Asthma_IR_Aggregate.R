#Created by Raed Alotaibi
#Creation Date: 3/21/19 
#Purpose: Analysing 2006-2010 childhod asthma incidence rates in the US by loading two data sets the BRFSS and ACBS

Sys.time()

# Houskeeping
ls()
rm(list=ls())
search()

#Check working directory
getwd()

#loading libraries
library(foreign)
library(dplyr)
library(xlsx)
library(tidyr)
library(readxl)


# Loading data set

load("Data/FIPS.R")
FIPS$FIPS <- as.double(FIPS$FIPS)


# Creating for loop to run the code for the years 2006-2010
for(i in 6:10){
  
    ifelse(i < 10, year <- paste0("0", i), year <- i)

    print(paste0(" ######## RUNNING ANALYSIS FOR THE YEAR 20", year ))
      

    # Assigning the ACBS and BRFSS file paths
    path_ACBS <- paste0("Data/ACBS_20", year, ".SAV")
    path_BRFSS <- paste0("Data/CDBRFS", year, ".XPT")
    
    
    # Reading the ACBS and BRFSS files
    ACBS <- read.spss(path_ACBS, use.value.labels = F, to.data.frame = T, trim.factor.names = T, trim_values = T)
    BRFSS <- read.xport(path_BRFSS)
    
  


# ACBS Analysis

ACBS_sample <- ACBS %>% 
  select("X._STATE", "INCIDNT") %>% 
  rename(FIPS =  X._STATE ) %>% 
  group_by(FIPS, INCIDNT) %>% 
  count(INCIDNT) %>%
  spread(INCIDNT, n) %>% 
  full_join(FIPS) %>% 
  arrange(FIPS)



ACBS_weighted <- ACBS %>% 
  select("X._STATE", "INCIDNT", "CHILDWT_F") %>% 
  rename(FIPS =  X._STATE ) %>% 
  group_by(FIPS, INCIDNT) %>% 
  summarise(count = sum(CHILDWT_F, na.rm = T)) %>% 
  spread(INCIDNT, count) %>% 
  full_join(FIPS) %>% 
  arrange(FIPS)



# BRFSS Analysis

BRFSS_sample <- BRFSS %>% 
  select("X_STATE", "CASTHDX2") %>% 
  rename(FIPS =  X_STATE ) %>% 
  group_by(FIPS, CASTHDX2) %>% 
  count(CASTHDX2) %>%
  spread(CASTHDX2, n) %>% 
  full_join(FIPS) %>% 
  arrange(FIPS)

BRFSS_weighted <- BRFSS %>% 
  select("X_STATE", "CASTHDX2", "X_CHILDWT") %>% 
  rename(FIPS =  X_STATE ) %>% 
  group_by(FIPS, CASTHDX2) %>% 
  summarise(count = sum(X_CHILDWT, na.rm = T)) %>% 
  spread(CASTHDX2, count) %>% 
  full_join(FIPS) %>% 
  arrange(FIPS) %>% 
  na_if(0) # The BRFSS had some values coded as zero when they should have been NA


# Joining ACBS with BRFSS

Asthma_sample <- ACBS_sample %>% 
  full_join(BRFSS_sample, by = "FIPS", suffix = c("_ACBS", "_BRFSS")) %>% 
  rename(State = State_BRFSS)
  
  
Asthma_weighted <- ACBS_weighted %>% 
  full_join(BRFSS_weighted, by = "FIPS", suffix = c("_ACBS", "_BRFSS")) %>% 
  rename(State = State_BRFSS)


# Incidence Rate Estimation

Asthma_IR <- Asthma_weighted %>% 
  mutate(At_risk = `1_ACBS` + `2_BRFSS`) %>% 
  mutate(IR_per1000 = `1_ACBS` / At_risk*1000) %>% 
  select(FIPS,State, `1_ACBS`, At_risk, IR_per1000) %>% 
  arrange(FIPS) %>% 
  rename(`<12_month` =  `1_ACBS`) 
  

# Prevalance Rate Estimation

Asthma_PRV <- Asthma_sample %>% 
  mutate(SAMPLE = `1_BRFSS` + `2_BRFSS`) %>% 
  mutate(PRV_per100 = `1_BRFSS`/ SAMPLE * 100) %>% 
  rename(EVER =  `1_BRFSS`) %>% 
  select(FIPS,State, EVER, SAMPLE, PRV_per100) %>% 
  arrange(FIPS)

# Converting to data frame to print

Asthma_IR <- as.data.frame(Asthma_IR)
Asthma_PRV <- as.data.frame(Asthma_PRV)
Asthma_sample <- as.data.frame(Asthma_sample)
Asthma_weighted <- as.data.frame(Asthma_weighted)


# Printing to Excel sheet

write.xlsx(Asthma_sample, "Results/Asthma_result.xlsx", sheetName = paste0("20", year, "_count"), showNA=F, append = T, row.names = F)
write.xlsx(Asthma_weighted, "Results/Asthma_result.xlsx", sheetName = paste0("20", year, "_weighted"), showNA=F, append = T,row.names = F)
write.xlsx(Asthma_IR, "Results/Asthma_IR.xlsx", sheetName = paste0("20", year), showNA=F, append = T, row.names = F)
write.xlsx(Asthma_PRV, "Results/Asthma_PRV.xlsx", sheetName = paste0("20", year), showNA=F, append = T, row.names = F)

  

}


# Asthma IR Aggregate -----------------------------------------------------



for(i in 2006:2010) {
  
  i <- as.character(i)
  print(i)
  

  Asthma_result <- read_excel("Results/Asthma_IR.xlsx", sheet = i) %>% 
    select(FIPS, State,`<12_month`, `At_risk`) %>% 
    na.omit()
  
  print(Asthma_result)
  
  # Binding data sets by row
  ifelse(i == "2006",   Asthma_agg <- Asthma_result, Asthma_agg <- Asthma_agg)
  
  Asthma_agg <- bind_rows(Asthma_agg, Asthma_result)
  
}
  

# Aggregating the IR by state for all years 
Asthma_agg_IR <- Asthma_agg %>% 
  group_by(FIPS, State) %>% 
  summarise_all(sum ) %>% 
  mutate(`IR per 1000` = `<12_month`/ At_risk*1000) %>% 
  as.data.frame()

range(Asthma_agg_IR$`IR per 1000`)



# # Aggregating the National IR for all years 
# Asthma_agg_IR <- Asthma_agg %>% 
#   summarise_all(sum ) %>% 
#   mutate(`IR per 1000` = `<12_month`/ At_risk*1000) %>% 
#   as.data.frame()
# 

# Printing Aggregated data to Excel
write.xlsx(Asthma_agg_IR, "Results/Asthma_IR.xlsx", sheetName = "Aggregate", showNA=F, append = T, row.names = F)




# Asthma PRV Aggregate -----------------------------------------------------


for(i in 2006:2010) {
  
  i <- as.character(i)
  i <- paste0(i, "_weighted")
  print(i)
  
  Asthma_result <- read_excel("Results/Asthma_result.xlsx", sheet = i) %>% 
    select(FIPS, State,`1_BRFSS`, `2_BRFSS`) %>% 
    na.omit()
  
  print(Asthma_result)
  
  # Binding data sets by row
  ifelse(i == "2006_weighted",   Asthma_agg <- Asthma_result, Asthma_agg <- Asthma_agg)
  
  Asthma_agg <- bind_rows(Asthma_agg, Asthma_result)
  
}

# Aggregating the PRV by state for all years 
Asthma_agg_PRV <- Asthma_agg %>% 
  group_by(FIPS, State) %>% 
  summarise_all(sum ) %>% 
  mutate(`PRV per 100` = `1_BRFSS`/ `2_BRFSS`*100) %>%
  rename(EVER = `1_BRFSS`, SAMPLE = `2_BRFSS`) %>% 
  as.data.frame()

range(Asthma_agg_PRV$`PRV per 100`)

# Printing Aggregated data to Excel
write.xlsx(Asthma_agg_PRV, "Results/Asthma_PRV.xlsx", sheetName = "Aggregate", showNA=F, append = T, row.names = F)





