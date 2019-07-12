#---------------------------------------------#
#Project : State specific Burden of childhood asthma due to TRAP - 2019
#Sub     : Comparing Burden
#Part    : (01) Preparing original BoD estimates 
#Purpose : Read in census data, income data, NO2 conc, incidence rate (original study), and prevelance rate (original study)
#         Followed by joining the data sets 
#Created by Raed Alotaibi
#Date Created: May-06-2019
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




# Joining Data Sets -------------------------------------------------------

# Joining census, income, NO2, and assigning asthma incidence and asthma prevalance rates from original study
join <- census %>% 
  mutate(GISJOIN_i = substr(GISJOIN, 1, 15)) %>% 
  left_join(lur, by = "GISJOIN") %>%
  left_join(income, by = "GISJOIN_i") %>% 
  mutate(IR = 0.0125, PRV = 0.137) %>% 
  select(-GISJOIN_i)



# Total Incidenct Cases ---------------------------------------------------

incident <- join %>%
  mutate(CASES = (CHILDREN - (CHILDREN * PRV)) * IR)



# Burden Modeling ---------------------------------------------------------

## Estimating (RR of new exposure: RRnew, Attributable fraction; AF, Attributable cases; AC with lower and upper limits)

crf <- 1.05
unit_inc <- 4

burden_c <- incident %>% 
  mutate(RRnew = exp((log(crf)/unit_inc)*NO2)) %>% 
  mutate(AF = (RRnew - 1)/(RRnew)) %>% 
  mutate(AC = AF*CASES) 





# Summary estimates -------------------------------------------------------


BLOCK <- burden_c %>%
  count()

BLOCK_U <- burden_c %>% 
  group_by(URBAN) %>% 
  count()

CHILDREN <- burden_c %>% 
  summarise(CHILDREN = sum(CHILDREN), 
            ASTHMA = sum(CASES) ,
            AC = sum(AC)) %>% 
  mutate(AF = round((AC/ASTHMA * 100), 1)) 


CHILDREN_URBAN <- burden_c %>% 
  group_by(URBAN) %>% 
  summarise(CHILDREN = sum(CHILDREN), 
            ASTHMA = sum(CASES) ,
            AC = sum(AC)) %>% 
  mutate(AF = round((AC/ASTHMA * 100), 1)) 


CHILDREN_INCOME <- burden_c %>% 
  group_by(INCOME) %>% 
  summarise(CHILDREN = sum(CHILDREN), 
            ASTHMA = sum(CASES) ,
            AC = sum(AC)) %>% 
  mutate(AF = round((AC/ASTHMA * 100), 1))



# Results tables ----------------------------------------------------------


table_a <- bind_rows(CHILDREN, CHILDREN_URBAN, CHILDREN_INCOME) %>% 
  select(URBAN, INCOME, CHILDREN, ASTHMA, AC, AF) %>% as.data.frame()

table_1 <- bind_rows(BLOCK, BLOCK_U, CHILDREN, CHILDREN_URBAN, CHILDREN_INCOME) %>% 
  select(URBAN, INCOME, n, CHILDREN) %>% as.data.frame()

table_2 <- bind_rows(CHILDREN, CHILDREN_URBAN, CHILDREN_INCOME) %>% 
  select(URBAN, INCOME,ASTHMA) %>% as.data.frame()

table_3 <- bind_rows(CHILDREN, CHILDREN_URBAN, CHILDREN_INCOME) %>% 
  select(URBAN, INCOME,AC, AF) %>% as.data.frame()


POLLUT <- burden_c %>% 
  select(NO2) %>% 
  summary() %>% as.data.frame() 



# Printing to excel -------------------------------------------------------
class(table_a)

xlsx::write.xlsx(table_a, "Results/Table_ac.xlsx", row.names = F)
xlsx::write.xlsx(table_2, "Results/Table_2c.xlsx", row.names = F)
xlsx::write.xlsx(table_3, "Results/Table_3c.xlsx", row.names = F)
xlsx::write.xlsx(POLLUT, "Results/Table_4c.xlsx", row.names = F)


# Result ----------------------------------------------------------------


# (1) Total

burden_c_1 <- burden_c %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, -2), AC = round(AC, -1), AF = round(AF, 1))

# (2) State 

burden_c_2 <- burden_c %>% 
  group_by(STATE) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, -2), AC = round(AC, -1), AF = round(AF, 1))


# (3) Urban status

burden_c_3 <- burden_c %>% 
  group_by(URBAN) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, -2), AC = round(AC, -1), AF = round(AF, 1))


# (4) Income status

burden_c_4 <- burden_c %>% 
  group_by(INCOME) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, -2), AC = round(AC, -1), AF = round(AF, 1))



# (5) State/Urban status

burden_c_5 <- burden_c %>% 
  group_by(STATE, URBAN) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, -2), AC = round(AC, -1), AF = round(AF, 1))



# (6) State/Income status

burden_c_6 <- burden_c %>% 
  group_by(STATE, INCOME) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, -2), AC = round(AC, -1), AF = round(AF, 1))


# (7) Urban/State status

burden_c_7 <- burden_c %>% 
  group_by(URBAN, STATE) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, -2), AC = round(AC, -1), AF = round(AF, 1))


# (8) Income/State status

burden_c_8 <- burden_c %>% 
  group_by(INCOME, STATE) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, -2), AC = round(AC, -1), AF = round(AF, 1))



# (9) Urban/Income status

burden_c_9 <- burden_c %>% 
  group_by(URBAN, INCOME ) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, -2), AC = round(AC, -1), AF = round(AF, 1))



# (10) Income/Urban status

burden_c_10 <- burden_c %>% 
  group_by(INCOME, URBAN ) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, -2), AC = round(AC, -1), AF = round(AF, 1))



# (11) State/Income/Urban status

burden_c_11 <- burden_c %>% 
  group_by(STATE, INCOME, URBAN) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, -2), AC = round(AC, -1), AF = round(AF, 1))



# (12) State/Urban/Income status

burden_c_12 <- burden_c %>% 
  group_by(STATE, URBAN, INCOME) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, -2), AC = round(AC, -1), AF = round(AF, 1))


# Printing ----------------------------------------------------------------

burden_c_list <- list(burden_c_1, burden_c_2, burden_c_3, burden_c_4, burden_c_5, burden_c_6, burden_c_7, burden_c_8, burden_c_9, burden_c_10, burden_c_11, burden_c_12 )
sheet <- c("Total", "By State", "By Urban", "By Income", 
           "By State|Urban", "By State|Income", "By Urban|State", "By Income|State", "By Urban|Income", "By Income|Urban", "By State|Income|Urban", "By State|Urban|Income")

for(i in 1:length(sheet)) {
  print(i)
  print(sheet[i])
  # Printing Aggregated data to Excel
  xlsx::write.xlsx(burden_c_list[i], "Results/burden_by_state_c.xlsx", sheetName = sheet[i], showNA=F, append = T, row.names = F)
  
}









