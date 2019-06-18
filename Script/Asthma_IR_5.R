library(readxl)
library(plyr)

Asthma_06 <- read_excel("Results/Tables/Asthma_IR.xlsx", 
                        sheet = "2006") %>% select('State', 'IR_per1000')
Asthma_07 <- read_excel("Results/Tables/Asthma_IR.xlsx", 
                        sheet = "2007") %>% select('State', 'IR_per1000')
Asthma_08 <- read_excel("Results/Tables/Asthma_IR.xlsx", 
                        sheet = "2008") %>% select('State', 'IR_per1000')
Asthma_09 <- read_excel("Results/Tables/Asthma_IR.xlsx", 
                        sheet = "2009")%>% select('State', 'IR_per1000')
Asthma_10 <- read_excel("Results/Tables/Asthma_IR.xlsx", 
                        sheet = "2010") %>% select('State', 'IR_per1000')
Asthma_agg <- read_excel("Results/Tables/Asthma_IR.xlsx", 
                         sheet = "Aggregate") %>% select('State', 'IR per 1000')

# Removing N/A
IR <- Asthma_agg %>% 
  left_join(Asthma_06)

IR <- list(Asthma_06,Asthma_07, Asthma_08, Asthma_09, Asthma_10)

IR_c <- lapply(IR, na.omit) 


IR_y <- list()

for(i in 1:5){
  
  IR_y[[i]] <-  IR_c[[i]] %>% 
    mutate(Year = "*") %>%
    select(State, Year)

  
      }


Asthma <-  FIPS %>% 
  left_join(IR_y[[1]], by = 'State') %>% 
  left_join(IR_y[[2]], by = 'State') %>%   
  left_join(IR_y[[3]], by = 'State') %>% 
  left_join(IR_y[[4]], by = 'State') %>% 
  left_join(IR_y[[5]], by = 'State') 

names_asthma <- c("FIPS", "State", "06", "07", "08", "09", "10")
names(Asthma) <- names_asthma
Asthma

load("Data/FIPS.R")
FIPS$FIPS <- as.double(FIPS$FIPS) #To be used to add FIPS code and state names to data sets

Asthma <-  FIPS %>% 
  left_join(IR_y[[1]], by = 'State') %>% 
  left_join(IR_y[[2]], by = 'State') %>%   
  left_join(IR_y[[3]], by = 'State') %>% 
  left_join(IR_y[[4]], by = 'State') %>% 
  left_join(IR_y[[5]], by = 'State') %>% 
  mutate(all = paste(Year.x, Year.y, Year.x.x, Year.y.y, Year)) %>% 
  select(State, all)
  







