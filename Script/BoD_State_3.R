#---------------------------------------------#
#Project : State specific Burden of childhood asthma due to TRAP - 2019
#Part : (03) Printing Results tables
#Purpose: Conduct the burden modeling and estimate results by state/urban/income
#Created by Raed Alotaibi
#Date Created: April-22-2019
#---------------------------------------------#






# Summary estimates -------------------------------------------------------


BLOCK <- burden %>%
  count()

BLOCK_U <- burden %>% 
  group_by(URBAN) %>% 
  count()

CHILDREN <- burden %>% 
  summarise(CHILDREN = sum(CHILDREN), 
            ASTHMA = sum(CASES) ,
            AC = sum(AC)) %>% 
  mutate(AF = round((AC/ASTHMA * 100), 1)) 
  

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



# Results tables ----------------------------------------------------------


table_a <- bind_rows(CHILDREN, CHILDREN_URBAN, CHILDREN_INCOME) %>% 
  select(URBAN, INCOME, CHILDREN, ASTHMA, AC, AF) %>% as.data.frame()

table_1 <- bind_rows(BLOCK, BLOCK_U, CHILDREN, CHILDREN_URBAN, CHILDREN_INCOME) %>% 
  select(URBAN, INCOME, n, CHILDREN) %>% as.data.frame()
  
table_2 <- bind_rows(CHILDREN, CHILDREN_URBAN, CHILDREN_INCOME) %>% 
  select(URBAN, INCOME,ASTHMA) %>% as.data.frame()

table_3 <- bind_rows(CHILDREN, CHILDREN_URBAN, CHILDREN_INCOME) %>% 
  select(URBAN, INCOME,AC, AF) %>% as.data.frame()


POLLUT <- burden %>% 
  select(NO2) %>% 
  summary() %>% as.data.frame() 



# Printing to excel -------------------------------------------------------
class(table_a)

xlsx::write.xlsx(table_a, "Results/Table_a.xlsx", row.names = F)
xlsx::write.xlsx(table_1, "Results/Table_1.xlsx", row.names = F)
xlsx::write.xlsx(table_2, "Results/Table_2.xlsx", row.names = F)
xlsx::write.xlsx(table_3, "Results/Table_3.xlsx", row.names = F)
xlsx::write.xlsx(POLLUT, "Results/Table_4.xlsx", row.names = F)


