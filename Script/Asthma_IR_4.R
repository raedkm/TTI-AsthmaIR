#---------------------------------------------#
#Project : State specific childhood Asthma incidence rate
#Part : (04) Estimating Counts for ACBS and BRFSS (2006-2010)  across states 
#Purpose: Conduct the burden modeling and estimate results by state
#Created by Raed Alotaibi
#Date Created: May-16-2019
#---------------------------------------------#


library(readxl)
library(dplyr)


Asthma_count <- read_excel("Results/Tables/Asthma_count.xlsx")
State_in <- read_excel("Results/Tables/Asthma_count_State.xlsx")
  



Year_count <- Asthma_count %>% 
  filter(State  %in% State_in$State_in ) %>% 
  group_by( Year) %>% 
  summarize(
    Sample_c = sum(BRFSS_1...7, BRFSS_1...8, na.rm = T), 
    Ever = sum(BRFSS_1...7, na.rm = T),
    Incident = sum(ACBS_1, na.rm = T),
    At_risk = sum(ACBS_1, BRFSS_1...8, na.rm = T))%>% 
  as.data.frame() %>% t()


State_count <- Asthma_count %>% 
  filter(State  %in% State_in$State_in ) %>% 
  group_by( State) %>% 
  summarize(
            Sample_c = sum(BRFSS_1...7, BRFSS_1...8, na.rm = T), 
            Ever = sum(BRFSS_1...7, na.rm = T),
            Incident = sum(ACBS_1, na.rm = T),
            At_risk = sum(ACBS_1, BRFSS_1...8, na.rm = T)) %>% 
  as.data.frame()




write.xlsx(Year_count, "Results/Asthma_count_year.xlsx",  append = F, row.names = T, col.names = F)
write.xlsx(State_count, "Results/Asthma_count_state.xlsx",  append = F, row.names = F)




############################


#############################

#---------------------------------------------#
#Project : State specific childhood Asthma incidence rate
#Part : (01) Preparing ACBD and BRFS data sets for 2006-2010, printing out results in excel
#Purpose: Conduct the burden modeling and estimate results by state
#Created by Raed Alotaibi
#Date Created: March-12-2019
#---------------------------------------------#


#loading libraries
library(foreign)
library(dplyr)
library(xlsx)
library(tidyr)
library(readxl)


# Loading data set

load("Data/FIPS.R")
FIPS$FIPS <- as.double(FIPS$FIPS) #To be used to add FIPS code and state names to data sets


# Creating for loop to run the code for the years 2006-2010
for(i in 6:10){
  
  ifelse(i < 10, year <- paste0("0", i), year <- i)
  
  print(paste0(" ######## RUNNING ANALYSIS FOR THE YEAR 20", year ))
  
  
  # Assigning the ACBS and BRFSS file paths
  path_ACBS <- paste0("Data/ACBS_20", year, ".SAV") 


    ACBS_i <- read.spss(path_ACBS, use.value.labels = F, to.data.frame = T, 
                      trim.factor.names = T, trim_values = T) %>% 
      select("X._STATE", "INCIDNT") %>% 
      mutate(Year = as.character(year))
    

if(i==6) {

  ACBS <- ACBS_i

  }else{

    ACBS <- ACBS %>% 
    bind_rows(ACBS_i) 
  
  }
 
}




State_count_2 <- ACBS %>% 
  group_by( X._STATE ) %>%
  rename(FIPS = X._STATE) %>% 
  count() %>% 
  left_join(FIPS) %>% 
  filter(State  %in% State_in$State_in ) %>% 
  select(State, n)

  
  