#---------------------------------------------#
#Project : State specific childhood Asthma incidence rate
#Part : (03) Estimating aggregate (2006-2010) astham PRV across states 
#Purpose: Conduct the burden modeling and estimate results by state/urban
#Created by Raed Alotaibi
#Date Created: March-12-2019
#---------------------------------------------#



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





