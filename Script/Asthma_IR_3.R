#---------------------------------------------#
#Project : State specific childhood Asthma incidence rate
#Part : (03) Producing a table of Asthma IR by state and year
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


