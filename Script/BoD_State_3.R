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


POP <- burden %>% 
  summarise(POP = sum(TOTAL))


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


table_a <- bind_rows(POP, CHILDREN, CHILDREN_URBAN, CHILDREN_INCOME) %>% 
  select(URBAN, INCOME, POP, CHILDREN, ASTHMA, AC, AF) %>% as.data.frame()

table_1 <- bind_rows(BLOCK, BLOCK_U, POP, CHILDREN, CHILDREN_URBAN, CHILDREN_INCOME) %>% 
  select(URBAN, INCOME, n, POP, CHILDREN) %>% as.data.frame()
  
table_2 <- bind_rows(CHILDREN, CHILDREN_URBAN, CHILDREN_INCOME) %>% 
  select(URBAN, INCOME,ASTHMA) %>% as.data.frame()

table_3 <- bind_rows(CHILDREN, CHILDREN_URBAN, CHILDREN_INCOME) %>% 
  select(URBAN, INCOME,AC, AF) %>% as.data.frame()


table_p1 <- burden %>% 
  select(NO2) %>% 
  summarise( Mean = mean(NO2),
             Min = min(NO2), 
             first = quantile(NO2, .25),
             Median = median(NO2),
             third = quantile(NO2, .75),
             Max = max(NO2)) %>% as.data.frame() 


table_p2 <- burden %>% 
  group_by(URBAN) %>% 
  select(NO2) %>% 
  summarise( Mean = mean(NO2),
             Min = min(NO2), 
             first = quantile(NO2, .25),
             Median = median(NO2),
             third = quantile(NO2, .75),
             Max = max(NO2)) %>% as.data.frame() 


table_p3 <- burden %>% 
  group_by(INCOME) %>% 
  select(NO2) %>% 
  summarise( Mean = mean(NO2),
             Min = min(NO2), 
             first = quantile(NO2, .25),
             Median = median(NO2),
             third = quantile(NO2, .75),
             Max = max(NO2)) %>% as.data.frame() 

table_p <- table_p1 %>% 
  full_join(table_p2) %>% 
  full_join(table_p3) %>% 
  select(URBAN, INCOME, Mean, Min, first, Median, third, Max)

table_p_s <- burden %>% 
  group_by(STATE) %>% 
  select(NO2) %>% 
  summarise( Mean = mean(NO2),
             Min = min(NO2), 
             first = quantile(NO2, .25),
             Median = median(NO2),
             third = quantile(NO2, .75),
             Max = max(NO2)) %>% as.data.frame() 

# Printing to excel -------------------------------------------------------
class(table_a)

xlsx::write.xlsx(table_a, "Results/Tables/Table_a.xlsx", row.names = F)
xlsx::write.xlsx(table_1, "Results/Tables/Table_1.xlsx", row.names = F)
xlsx::write.xlsx(table_2, "Results/Tables/Table_2.xlsx", row.names = F)
xlsx::write.xlsx(table_3, "Results/Tables/Table_3.xlsx", row.names = F)
xlsx::write.xlsx(table_p, "Results/Tables/Table_4.xlsx", row.names = F)
xlsx::write.xlsx(table_p_s, "Results/Tables/Table_4b.xlsx", row.names = F)


