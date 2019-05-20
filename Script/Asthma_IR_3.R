#---------------------------------------------#
#Project : State specific childhood Asthma incidence rate
#Part : (03) Estimating aggregate (2006-2010) astham PRV across states 
#Purpose: Conduct the burden modeling and estimate results by state
#Created by Raed Alotaibi
#Date Created: March-12-2019
#---------------------------------------------#



# Asthma PRV Aggregate -----------------------------------------------------


for(i in 2006:2010) {
  
  i <- as.character(i)
  j <- i
  i <- paste0(i, "_weighted")
  print(i)
  
  Asthma_result <- read_excel("Results/Tables/Asthma_result.xlsx", sheet = i) %>% 
    select(FIPS, State,`1_BRFSS`, `2_BRFSS`) %>% 
    mutate(Year = j) %>%
    mutate(SAMPLE = `1_BRFSS` + `2_BRFSS`) %>% 
    rename(EVER =  `1_BRFSS`) %>%
    select(FIPS, State, EVER, SAMPLE, Year) %>% 
    na.omit()
  
  print(Asthma_result)
  
  # Binding data sets by row
  ifelse(i == "2006_weighted",   Asthma_agg <- Asthma_result, Asthma_agg <- bind_rows(Asthma_agg, Asthma_result))
  
}

# Aggregating the PRV by state for all years 
Asthma_agg_PRV <- Asthma_agg %>%
  select(-Year) %>% 
  group_by(FIPS, State) %>% 
  summarise_all(sum ) %>% 
  mutate(`PRV per 100` = EVER / SAMPLE*100) %>%
  as.data.frame()

range(Asthma_agg_PRV$`PRV per 100`)



# Aggregating the National PRV by years
Asthma_agg_national_year_PRV <- Asthma_agg %>%
  group_by(Year) %>%
  summarise_at(c("EVER", "SAMPLE"), sum ) %>%
  mutate(`PRV per 100` = EVER / SAMPLE*100) %>%
  as.data.frame()



# Aggregating the National PRV 
Asthma_agg_national_PRV <- Asthma_agg_national_year_PRV %>%
  #group_by(Year) %>%
  summarize(EVER = sum(EVER)/5,
            SAMPLE = sum(SAMPLE)/5,
            `PRV per 100` = EVER / SAMPLE*100) %>%
  mutate(Year = "'National") %>% 
  as.data.frame()



# Printing Aggregated data to Excel
write.xlsx(Asthma_agg_PRV, "Results/Tables/Asthma_PRV.xlsx", sheetName = "Aggregate", showNA=F, append = T, row.names = F)



# Printing Tables 8
table_IR <- Asthma_agg_national_IR %>% 
  rbind(Asthma_agg_national_year_IR) 
  
table_PRV <- Asthma_agg_national_PRV %>% 
  rbind(Asthma_agg_national_year_PRV) 

names <- table_IR$Year

table_8 <- table_IR %>% 
  left_join(table_PRV, by = "Year") %>% 
  select(SAMPLE, EVER, `<12_month`, At_risk, `IR per 1000`, `PRV per 100`) %>% 
  t()

colnames(table_8) <- names 

write.xlsx(table_8, "Results/Tables/table_8.xlsx",  append = F, row.names = T)



# Printing Table 9

table_9 <- Asthma_agg_IR %>% 
  left_join(Asthma_agg_PRV, by = c("FIPS", "State")) %>% 
  select(State, SAMPLE, EVER, `<12_month`, At_risk, `IR per 1000`, `PRV per 100`)

write.xlsx(table_9, "Results/Tables/table_9.xlsx",  append = F, row.names = F)
