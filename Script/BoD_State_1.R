#---------------------------------------------#
#Project : State specific Burden of childhood asthma due to TRAP - 2019
#Sub     : Burden estimates
#Part    : (01) Preparing data sets 
#Purpose : Read in census data, income data, NO2 conc, incidence rate (state-specifi), and prevelance rate (state, specific)
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



# Loading census data -----------------------------------------------------


## Loading the 2010 children census 
path_census <- "Data\\Census\\nhgis0040_ds172_2010_block.csv"

var_census <- c("GISJOIN", "STATE", "STATEA", "URBRURALA", "H7V001", 
         "H7W001", "H7W002", "H7W003", "H7W004", "H7W005", "H7W006",      
         "H76003", "H76004", "H76005", "H76006", 
         "H76027", "H76028", "H76029", "H76030", 
         "PLACEA" )


# Making new variable of total children count "CHILDREN" and renaming variables
census <- fread(file = path_census,data.table = F, stringsAsFactors = F, verbose = T, select = var_census,
                colClasses = list(factor = c("STATE", "URBRURALA"))) %>% 
  filter(H7V001 > 0) %>% 
  mutate(CHILDREN = H76003 + H76004 + H76005 +H76006 +
           H76027 + H76028 + H76029 + H76030) %>% 
  mutate(FIPS = stri_sub(GISJOIN, 2,3)) %>%
  mutate(PLACEA = str_pad(PLACEA, 5, pad = "0")) %>%
  mutate(PlaceFIPS = paste0(FIPS, PLACEA)) %>% 
  mutate(URBAN = case_when(
    H7W003 > 0 ~ "Urbanized area", 
    H7W004 > 0 ~ "Urban cluster", 
    H7W005 > 0 ~ "Rural", 
    H7W006 > 0 ~ "Not defined",
    TRUE ~ "Not defined" 
  )) %>% 
  rename(TOTAL = H7V001) %>% 
  select(GISJOIN, FIPS, PlaceFIPS, PLACEA, STATE, URBRURALA, URBAN, TOTAL,  CHILDREN) %>% 
  as_tibble()

included_blocks <- census$GISJOIN


# Loading  median houshold income -------------------------------------

path_income <- "Data\\Census\\nhgis0040_ds176_20105_2010_blck_grp.csv"

var_income <- c("GISJOIN", "JOIE001", "JOIM001" )

income <- fread(file = path_income,data.table = F, stringsAsFactors = F, verbose = T, 
                select = var_income) %>% 
  mutate(INCOME = as.factor(case_when(
    JOIE001 < 20000 ~ "<20,000", 
    between(JOIE001, 20000, 34999) ~ "20,000 to <35,000", 
    between(JOIE001, 35000, 49999) ~ "35,000 to <50,000", 
    between(JOIE001, 50000, 74999) ~ "50,000 to <75,000", 
    JOIE001 >= 75000  ~ ">=75,000", 
    TRUE ~ "Not defined"))) %>% 
  mutate(INCOME = forcats::fct_relevel(INCOME, "<20,000", "20,000 to <35,000", "35,000 to <50,000", 
                               "50,000 to <75,000", ">=75,000")) %>% 
  rename(GISJOIN_i = GISJOIN) %>% 
  as_tibble()  
 


# Loading NO2 data --------------------------------------------------------

var_lur <- c("GISJOIN", "Y2010")

lur <- fread("Data\\Pollutant\\NO2_2010.csv", data.table = F, stringsAsFactors = F,  verbose = T, select = var_lur) %>% 
  filter(GISJOIN %in% included_blocks) %>% 
  mutate(NO2 = Y2010*1.88) %>% 
  select(GISJOIN, NO2) %>% 
  as_tibble()




# Loading state-specific asthma IR data -----------------------------------

path_inc <- "Results\\Asthma_IR.xlsx"

inc <- read_excel(path_inc, sheet = "Aggregate") %>% 
  mutate(IR = as.double(`IR per 1000`/1000)) %>% 
  mutate(FIPS = str_pad(FIPS, 2, pad = "0")) %>% 
  select(FIPS, IR)


# Estimating weighted IR
read_excel(path_inc, sheet = "Aggregate") %>%   
  summarise(IR = sum(`<12_month`)/sum(At_risk))
weighted_IR <- 0.0123



# Loading state-specific asthma  PRV rate ---------------------------------

path_prv <- "Results\\Asthma_PRV.xlsx"

prv <- read_excel(path_prv, sheet = "Aggregate") %>% 
  mutate(PRV = as.double(`PRV per 100`/100)) %>% 
  mutate(FIPS = str_pad(FIPS, 2, pad = "0")) %>% 
  select(FIPS, PRV)


# Estimating weighted PRV
read_excel(path_prv, sheet = "Aggregate") %>%   
  summarise(PRV = sum(EVER)/sum(SAMPLE))
weighted_PRV <- 0.150



# Joining Data Sets -------------------------------------------------------

# Joining census, income, NO2, asthma incidence and asthma prevalance data sets
join <- census %>% 
            mutate(GISJOIN_i = substr(GISJOIN, 1, 15)) %>% 
            left_join(lur, by = "GISJOIN") %>%
            left_join(income, by = "GISJOIN_i") %>% 
            left_join(inc, by = "FIPS") %>% 
            left_join(prv, by = "FIPS") %>% 
            replace_na(list(IR = weighted_IR, PRV = weighted_PRV)) %>% 
            select(-GISJOIN_i)



# Total Incidenct Cases ---------------------------------------------------

incident <- join %>%
  mutate(CASES = (CHILDREN - (CHILDREN * PRV)) * IR)



# Burden Modeling ---------------------------------------------------------

## Estimating (RR of new exposure: RRnew, Attributable fraction; AF, Attributable cases; AC with lower and upper limits)

crf <- 1.05
unit_inc <- 4

burden <- incident %>% 
  mutate(RRnew = exp((log(crf)/unit_inc)*NO2)) %>% 
  mutate(AF = (RRnew - 1)/(RRnew)) %>% 
  mutate(AC = AF*CASES) 


  
  
  
