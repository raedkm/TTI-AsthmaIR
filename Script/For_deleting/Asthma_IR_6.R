#---------------------------------------------#
#Project : State specific childhood Asthma incidence rate
#Part : (06) Producing a table of the ACBS weighted samples estimate
#Purpose: Conduct the burden modeling and estimate results by state
#Created by Raed Alotaibi
#Date Created: June-19-2019
#---------------------------------------------#


# loading libraries
library(foreign)
library(dplyr)
library(plyr)
library(xlsx)
library(tidyr)
library(readxl)


Asthma_var <- c('State', 'IR_per1000')

# Loading the Asthma incidence rate tables produced from earlier scripts
Asthma_06 <- read_excel("Results/Tables/Asthma_IR.xlsx", sheet = "2006") %>% select(Asthma_var)
Asthma_07 <- read_excel("Results/Tables/Asthma_IR.xlsx", sheet = "2007") %>% select(Asthma_var)
Asthma_08 <- read_excel("Results/Tables/Asthma_IR.xlsx", sheet = "2008") %>% select(Asthma_var)
Asthma_09 <- read_excel("Results/Tables/Asthma_IR.xlsx", sheet = "2009") %>% select(Asthma_var)
Asthma_10 <- read_excel("Results/Tables/Asthma_IR.xlsx", sheet = "2010") %>% select(Asthma_var)


IR <- list(Asthma_06, Asthma_07, Asthma_08, 
           Asthma_09, Asthma_10) %>% 
  lapply(na.omit)


IR_available <- FIPS %>% 
  left_join(IR[[1]], by = 'State') %>% 
  na.omit() %>% 
  select(FIPS)

IR_available[[1]]


for(i in 1:5){

  ifelse(i < 5, year <- paste0("0", i+5), year <- i+5)
  
  print(paste0(" ######## RUNNING ANALYSIS FOR THE YEAR 20", year ))
  
 
  # Assigning the corrospanding year with available data from the IR list
  included_states <- FIPS %>% 
    left_join(IR[[i]], by = 'State') %>% 
    na.omit() %>% 
    select(FIPS, State) %>% 
    as.vector()
  

  # Assigning the ACBS  file paths
  path_ACBS <- paste0("Data/ACBS_20", year, ".SAV")

  
  # Reading the ACBS  files
  ACBS <- read.spss(path_ACBS, use.value.labels = F, to.data.frame = T, trim.factor.names = T, trim_values = T)


  
  #ACBS weighted samples estimates
  ACBS_sample_weight <- ACBS %>% 
    select("X._STATE", "INCIDNT", "CHILDWT_F") %>% 
    #rename(FIPS = `X._STATE` ) %>% 
    filter(X._STATE %in% included_states[[1]]) %>% 
    group_by(X._STATE, INCIDNT) %>% 
    summarise(count = sum(CHILDWT_F, na.rm = T)) %>% 
    spread(INCIDNT, count) %>% 
    left_join(X._STATE) %>% 
    arrange(X._STATE) %>% as.data.frame()

  write.xlsx(ACBS_sample_weight, "Results/Tables/ACBS_sample_weight.xlsx", sheetName = paste0("20", year), showNA=F, append = T,row.names = F)
  
  }  
  

  