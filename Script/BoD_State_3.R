#---------------------------------------------#
#Project : State specific Burden of childhood asthma due to TRAP - 2019
#Part : (03) Printing Results tables
#Purpose: Conduct the burden modeling and estimate results by state/urban/income
#Created by Raed Alotaibi
#Date Created: March-12-2019
#---------------------------------------------#





# Results tables ----------------------------------------------------------


BLOCK <- burden %>%
  count()

BLOCK_U <- burden %>% 
  group_by(URBAN) %>% 
  count()

CHILDREN <- burden %>% 
  summarise(CHILDREN = sum(CHILDREN), 
            ASTHMA = sum(CASES) ,
            AC = sum(AC)) %>% 
  mutate(AF = round((AC/ASTHMA * 100), 1)) %>% 
  

CHILDREN_URBAN <- burden %>% 
  group_by(URBAN) %>% 
  summarise(CHILDREN = sum(CHILDREN), 
            ASTHMA = sum(CASES) ,
            AC = sum(AC)) %>% 
  mutate(AF = round((AC/ASTHMA * 100), 1)) 


CHILDREN_INCOME <- burden %>% 
  group_by(INCOME) %>% 
  summarise(CHILDREN = sum(CHILDREN), 
            ASTHMA = sum(CASES) ,
            AC = sum(AC)) %>% 
  mutate(AF = round((AC/ASTHMA * 100), 1))



table_a <- bind_rows(CHILDREN, CHILDREN_URBAN, CHILDREN_INCOME) %>% 
  select(URBAN, INCOME, CHILDREN, ASTHMA, AC, AF)

table_1 <- bind_rows(BLOCK, BLOCK_U, CHILDREN, CHILDREN_URBAN, CHILDREN_INCOME) %>% 
  select(URBAN, INCOME, n, CHILDREN) 
  
table_2 <- bind_rows(CHILDREN, CHILDREN_URBAN, CHILDREN_INCOME) %>% 
  select(URBAN, INCOME,ASTHMA) 

table_3 <- bind_rows(CHILDREN, CHILDREN_URBAN, CHILDREN_INCOME) %>% 
  select(URBAN, INCOME,AC, AF) 


POLLUT <- burden %>% 
  select(NO2) %>% 
  summary() %>% as.matrix() 
