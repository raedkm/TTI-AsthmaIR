#---------------------------------------------#
#Project : State specific childhood Asthma incidence rate
#Part : (02) Estimating aggregate (2006-2010) astham IR across states 
#Purpose: Conduct the burden modeling and estimate results by state/urban
#Created by Raed Alotaibi
#Date Created: March-12-2019
#---------------------------------------------#


# Asthma IR Aggregate -----------------------------------------------------



for(i in 2006:2010) {
  
  i <- as.character(i)
  print(i)
  
  
  Asthma_result <- read_excel("Results/Asthma_IR.xlsx", sheet = i) %>% 
    select(FIPS, State,`<12_month`, `At_risk`) %>% 
    mutate(Year = i) %>% 
    na.omit()
  
  print(Asthma_result)
  
  # Binding data sets by row


  ifelse(i == "2006",   Asthma_agg <- Asthma_result, Asthma_agg <- bind_rows(Asthma_agg, Asthma_result))
  
  
}


# Aggregating the IR by state for all years 
Asthma_agg_IR <- Asthma_agg %>%
  select(-Year) %>% 
  group_by(FIPS, State) %>% 
  summarise_all(sum ) %>% 
  mutate(`IR per 1000` = `<12_month`/ At_risk*1000) %>% 
  as.data.frame()

range(Asthma_agg_IR$`IR per 1000`)



# Aggregating the National IR by year
 Asthma_agg_national_year_IR <- Asthma_agg %>%
  group_by(Year) %>%
  summarise_at(c("<12_month", "At_risk"), sum ) %>%
  mutate(`IR per 1000` = `<12_month`/ At_risk*1000) %>%
  as.data.frame()

 
 
 # Aggregating the National IR 
 Asthma_agg_national_IR <- Asthma_agg_national_year_IR %>%
   #group_by(Year) %>%
   summarize(`<12_month` = sum(`<12_month`)/5,
             At_risk = sum(At_risk)/5,
            `IR per 1000` = `<12_month`/ At_risk*1000) %>%
   mutate(Year = "'National") %>% 
   as.data.frame()
 
 
 # Printing Aggregated data to Excel
write.xlsx(Asthma_agg_IR, "Results/Asthma_IR.xlsx", sheetName = "Aggregate", showNA=F, append = T, row.names = F)

