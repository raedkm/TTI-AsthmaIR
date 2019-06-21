#---------------------------------------------#
#Project : State specific childhood Asthma incidence rate
#Part : (05) Producing a table of what states were/were not included in the data sets
#Purpose: Conduct the burden modeling and estimate results by state
#Created by Raed Alotaibi
#Date Created: June-18-2019
#---------------------------------------------#


#loading libraries
library(readxl)
library(plyr)
library(foreign)
library(dplyr)
library(xlsx)
library(tidyr)
library(readxl)


# loading States and FIPs code
load("Data/FIPS.R")
FIPS$FIPS <- as.double(FIPS$FIPS) #To be used to add FIPS code and state names to data sets




# Section 1 - Available years ---------------------------------------------

Asthma_var <- c('State', 'IR_per1000')


# Loading the Asthma incidence rate tables produced from earlier scripts
Asthma_06 <- read_excel("Results/Tables/Asthma_IR.xlsx", sheet = "2006") %>% select(Asthma_var)
Asthma_07 <- read_excel("Results/Tables/Asthma_IR.xlsx", sheet = "2007") %>% select(Asthma_var)
Asthma_08 <- read_excel("Results/Tables/Asthma_IR.xlsx", sheet = "2008") %>% select(Asthma_var)
Asthma_09 <- read_excel("Results/Tables/Asthma_IR.xlsx", sheet = "2009") %>% select(Asthma_var)
Asthma_10 <- read_excel("Results/Tables/Asthma_IR.xlsx", sheet = "2010") %>% select(Asthma_var)
Asthma_agg <- read_excel("Results/Tables/Asthma_IR.xlsx", sheet = "Aggregate") %>% select('State', `IR per 1000`)



# Table with IR by state and year -----------------------------------------


Asthma_year <-  FIPS %>% 
  left_join(Asthma_06, by = 'State') %>% 
  left_join(Asthma_07, by = 'State') %>%   
  left_join(Asthma_08, by = 'State') %>% 
  left_join(Asthma_09, by = 'State') %>% 
  left_join(Asthma_10, by = 'State') %>% 
  left_join(Asthma_agg) %>% 
  select(-FIPS)


names_asthma <- c("State", "2006", "2007", "2008", "2009", "2010", "Aggregate")

names(Asthma_year) <- names_asthma

Asthma_year



# Priting the table to excel
write.xlsx(Asthma_year, "Results/Tables/Asthma_years.xlsx",  showNA=F, append = F, row.names = F)


# Table with indicator (1) of participation by state and year -------------

# 
# # Making a list for the data frames then omitting states with missing IR
# 
# IR <- list(Asthma_06, Asthma_07, Asthma_08, 
#            Asthma_09, Asthma_10) %>% 
#       lapply(na.omit)
# 
# 
# # The for loop will insert a (1) as an indicator for each year of available IR for each state.  
# 
# IR_year <- list()
# for(i in 1:5){
#   
#   IR_year[[i]] <-  IR[[i]] %>% 
#     mutate(Year = 1) %>%
#     select(State, Year)
#   
#       }
# 
# 
# Asthma_year <-  FIPS %>% 
#   left_join(IR_year[[1]], by = 'State') %>% 
#   left_join(IR_year[[2]], by = 'State') %>%   
#   left_join(IR_year[[3]], by = 'State') %>% 
#   left_join(IR_year[[4]], by = 'State') %>% 
#   left_join(IR_year[[5]], by = 'State') %>% 
#   mutate(Total = rowSums(select(.,contains("Year")), na.rm = T)) %>% 
#   mutate(Available = ifelse(Total > 0, 1, 0)) %>% 
#   select(-FIPS)
# 
# names_asthma <- c("State", "06", "07", "08", "09", "10", "Total", "Available")
# 
# names(Asthma_year) <- names_asthma
# 
# Asthma_year


# 
# # Priting the table to excel
# write.xlsx(Asthma_year, "Results/Tables/Asthma_years.xlsx",  showNA=F, append = F, row.names = F)





