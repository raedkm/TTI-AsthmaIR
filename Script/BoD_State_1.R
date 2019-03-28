#---------------------------------------------#
#Project : State specific Burden of childhood asthma due to TRAP - 2019
#Part : (01) Preparing data sets 
#Purpose: Read in census data, pollutants conc, incidence rate (state-specifi), and prevelance rate (state, specific)
#         Followed by joining the data sets and replacing missing IR/PRV with weighted averages
#Created by Raed Alotaibi
#Date Created: March-12-2019
#---------------------------------------------#
 

# Loading packages
library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(stringr)
library(stringi)
library(DT)
library(htmltools)

# Loading Data Sets -------------------------------------------------------


## Loading the 2010 children census 
path_census <- "Data\\Census\\nhgis0034_ds172_2010_block.csv"

var_census <- c("GISJOIN", "STATE", "STATEA", "URBRURALA", "H7V001", 
         "H76003", "H76004", "H76005", "H76006", 
         "H76027", "H76028", "H76029", "H76030", 
         "PLACEA")


# Making new variable of total children count "CHILDREN" and renaming variables
census <- fread(file = path_census,data.table = F, stringsAsFactors = F, verbose = T, select = var_census,
                colClasses = list(factor = c("STATE", "URBRURALA"))) %>% 
  filter(H7V001 > 0) %>% 
  mutate(CHILDREN = H76003 + H76004 + H76005 +H76006 +
           H76027 + H76028 + H76029 + H76030) %>% 
  mutate(FIPS = stri_sub(GISJOIN, 2,3)) %>%
  mutate(PLACEA = str_pad(PLACEA, 5, pad = "0")) %>%
  mutate(PlaceFIPS = paste0(FIPS, PLACEA)) %>% 
  select(GISJOIN, FIPS, PlaceFIPS, PLACEA, STATE, URBRURALA, CHILDREN) %>% 
  rename(URBAN = URBRURALA) %>% 
  as_tibble()

included_blocks <- census$GISJOIN



## Loading NO2 concentration  
var_lur <- c("GISJOIN", "Y2010")

lur <- fread("Data\\Pollutant\\NO2_2010.csv", data.table = F, stringsAsFactors = F,  verbose = T, select = var_lur) %>% 
  filter(GISJOIN %in% included_blocks) %>% 
  mutate(NO2 = Y2010*1.88) %>% 
  select(GISJOIN, NO2) %>% 
  as_tibble()



## Loading state-specific incidence rate
path_inc <- "Results\\Asthma_IR.xlsx"

inc <- read_excel(path_inc, sheet = "Aggregate") %>% 
  mutate(IR = as.double(`IR per 1000`/1000)) %>% 
  mutate(FIPS = str_pad(FIPS, 2, pad = "0")) %>% 
  select(FIPS, IR)


# Estimating weighted IR
read_excel(path_inc, sheet = "Aggregate") %>%   
  summarise(IR = sum(`<12_month`)/sum(At_risk))
weighted_IR <- 0.0123



## Loading state-specific prevelance rate
path_prv <- "Results\\Asthma_PRV.xlsx"

prv <- read_excel(path_prv, sheet = "Aggregate") %>% 
  mutate(PRV = as.double(`PRV per 100`/100)) %>% 
  mutate(FIPS = str_pad(FIPS, 2, pad = "0")) %>% 
  select(FIPS, PRV)


# Estimating weighted PRV
read_excel(path_prv, sheet = "Aggregate") %>%   
  summarise(PRV = sum(EVER)/sum(SAMPLE))
weighted_PRV <- 0.151



# Joining Data Sets -------------------------------------------------------

# Joining census, lur, incidence and prevalance data sets
join <- census %>% 
            left_join(lur, by = "GISJOIN") %>% 
            left_join(inc, by = "FIPS") %>% 
            left_join(prv, by = "FIPS") %>% 
            replace_na(list(IR = weighted_IR, PRV = weighted_PRV))

  
  
  
  
  
