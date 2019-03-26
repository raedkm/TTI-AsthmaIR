#---------------------------------------------
#Project : Burden of childhood asthma due to TRAP project - 2018
#Part : (0) Installing and Loading required packages
#Purpose: Load required packages
#
#Created by Raed Alotaibi
#---------------------------------------------#

#note : This  needs to be run each time you open R

# Loading packages
library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(stringr)
#---------------------------------------------#
#Project : Burden of childhood asthma due to TRAP project - 2018
#Part : (01) Preparing census data 
#Purpose: Read in and join census data for year 2010
#
#Created by Raed Alotaibi
#---------------------------------------------#


# Loading the 2010 children census
path <- "S:\\BoD_Asthma\\R projects\\Asthma_IR\\Data\\Census\\nhgis0034_ds172_2010_block.csv"
path <- "C:\\Users\\R-Alotaibi\\Desktop\\Data\\nhgis0034_ds172_2010_block.csv"

var <- c("GISJOIN", "STATE", "STATEA", "URBRURALA",
         "H76003", "H76004", "H76005", "H76006", 
         "H76027", "H76028", "H76029", "H76030")

census <- fread(file = path,data.table = F, stringsAsFactors = T, verbose = T, select = var) %>%  
            mutate(CHILDREN = H76003 + H76004 + H76005 +H76006 +
                              H76027 + H76028 + H76029 + H76030) %>% 
            select(GISJOIN, STATEA, STATE, URBRURALA, CHILDREN) %>% 
            rename(FIPS = STATEA, URBAN = URBRURALA) 


# Loading NO2 concentration  
lur <- fread("C:\\Users\\R-Alotaibi\\Desktop\\Data\\NO2_2010.csv", data.table = F, stringsAsFactors = T) %>% 
       mutate(NO2 = Y2010*1.88) %>% 
       select(GISJOIN, NO2)

# Loading state-specific incidence rate
prev <- 0.137

path_inc <- "C:\\Users\\R-Alotaibi\\Desktop\\Data\\Asthma_IR.xlsx"

inc <- read_excel(path_inc, sheet = "Aggregate") %>% 
  mutate(IR = `IR per 1000`/1000) %>% 
  select(FIPS, State, IR) %>% 
  rename(STATE = State)

#---------------------------------------------
#Project : Burden of childhood asthma due to TRAP project - 2018
#Part : (3) Estimating asthma cases
#Purpose:  Estimate the number of asthma cases for year 2010 
#
#Created by Raed Alotaibi
#---------------------------------------------


### Defining variables ####
# The below functions estimates the adjusted and non adjusted total incident cases of the population using 
# the 12.5 per 1000 incidence rate and the 13.7% prevalance rate

prev <- 0.137

path_inc <- "C:\\Users\\R-Alotaibi\\Desktop\\Data\\Asthma_IR.xlsx"

inc <- read_excel(path_inc, sheet = "Aggregate") %>% 
  mutate(IR = `IR per 1000`/1000) %>% 
  select(FIPS, State, IR) %>% 
  rename(STATE = State)


incidence_adj <- function(x) {
  case <- round(sum((x - (x * prev)) * inc))
  return(case)
}

incidence_nonadj <- function(x) {
  case <- round(sum(x * prev))
  return(case)
}


### Incidence estimation ####

#total incident cases among children 
cenPolInc_2010 <- mutate(cenPol_2010, cases = (total_children - (total_children*prev))*inc)


#by age group and gender
apply(cenPol_2010[,c(4:7, 9:19)], 2, incidence_adj)

#by Urban
tapply(cenPol_2010$total_children, cenPol_2010$Urban, incidence_adj)

#Median income 
tapply(cenPol_2010$total_children, cenPol_2010$mincome_i, incidence_adj)








#---------------------------------------------
#Project : Burden of childhood asthma due to TRAP project - 2018
#Part : (4) Estimating Burden
#Sub  : (A)  NO2
#Purpose: Estimate the burden of disease, "safe level" scenarions, and sensitivity
#analysis for year 2010 with census data
#Created by Raed Alotaibi
#---------------------------------------------

#clearing Environment
rm(list = ls())

# loading joined census and pollutant data
load("Data files/cenPolInc_2010.RData")

### Defining variables ####
# The below functions estimates the adjusted and non adjusted total incident cases of the population using 
# the 12.5 per 1000 incidence rate and the 13.7% prevalance rate
prev <- 0.137
inc <- 0.0125
inc_l <- 0.0105
inc_u <- 0.0144

### NO2ug Burden ####
#Assigning values for CRF
crf <- 1.05
crf_l <- 1.02
crf_u <- 1.07
unit_inc <- 4
low_det <- 1.478

#estimate (RR of new exposure: RRnew, Attributable fraction; AF, Attributable cases; AC with lower and upper limits)
cenPolInc_2010 <- mutate(cenPolInc_2010, RRnew = exp((log(crf)/unit_inc)*NO2ug))
cenPolInc_2010 <- mutate(cenPolInc_2010, AF = (RRnew - 1)/(RRnew))
cenPolInc_2010 <- mutate(cenPolInc_2010, AC = AF*cases)
cenPolInc_2010 <- mutate(cenPolInc_2010, AClower = ((exp((log(crf_l)/unit_inc)*NO2ug)-1)/((exp((log(crf_l)/unit_inc)*NO2ug)-1)+1))*cases)
cenPolInc_2010 <- mutate(cenPolInc_2010, ACupper = ((exp((log(crf_u)/unit_inc)*NO2ug)-1)/((exp((log(crf_u)/unit_inc)*NO2ug)-1)+1))*cases)


# estimating the counts of cases attributable to NO2ug exposure and the lower/upper limits
round(apply(cenPolInc_2010[,c("cases","AC","AClower","ACupper")],2, sum))

#by urban status
round(tapply(cenPolInc_2010$AC, cenPolInc_2010$Urban, sum))

#by median income
round(tapply(cenPolInc_2010$AC, cenPolInc_2010$mincome_i, sum))