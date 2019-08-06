#---------------------------------------------#
#Project : State specific Burden of childhood asthma due to TRAP - 2019
#Sub     : Creating Data Frames
#Part    : (08) Creating final data sets 
#Purpose : Creat finale data sets at multiple geographical levels
#Created by Raed Alotaibi
#Date Created : Aug-5-2019
#Last Modified: Aug-5-2019
#---------------------------------------------#


sample_n(burden, size = 20)

burden_county <- burden %>% 
  group_by(STATE, COUNTY) %>% 
  summarise(TOTAL = sum(TOTAL), 
            CHILDREN = sum(CHILDREN),
            NO2 = mean(NO2),
            CASES = sum(CASES),
            AC = sum(AC),
            AF = AC/CASES)

burden_place <- burden %>% 
  group_by(STATE, PLACEA) %>% 
  summarise(TOTAL = sum(TOTAL), 
            CHILDREN = sum(CHILDREN),
            NO2 = mean(NO2),
            CASES = sum(CASES),
            AC = sum(AC),
            AF = AC/CASES)


burden_zip <- burden %>% 
  group_by(STATE, ZCTA5A) %>% 
  summarise(TOTAL = sum(TOTAL), 
            CHILDREN = sum(CHILDREN),
            NO2 = mean(NO2),
            CASES = sum(CASES),
            AC = sum(AC),
            AF = AC/CASES)


burden_elementry <- burden %>% 
  filter(SDELMA != 99999) %>% 
  group_by(STATE, SDELMA) %>% 
  summarise(TOTAL = sum(TOTAL), 
            CHILDREN = sum(CHILDREN),
            NO2 = mean(NO2),
            CASES = sum(CASES),
            AC = sum(AC),
            AF = AC/CASES)

burden_secondary <- burden %>% 
  filter(SDSECA != 99999) %>%
  group_by(STATE, SDSECA) %>% 
  summarise(TOTAL = sum(TOTAL), 
            CHILDREN = sum(CHILDREN),
            NO2 = mean(NO2),
            CASES = sum(CASES),
            AC = sum(AC),
            AF = AC/CASES)

burden_unified <- burden %>% 
  filter(SDUNIA != 99999) %>%
  group_by(STATE, SDUNIA) %>% 
  summarise(TOTAL = sum(TOTAL), 
            CHILDREN = sum(CHILDREN),
            NO2 = mean(NO2),
            CASES = sum(CASES),
            AC = sum(AC),
            AF = AC/CASES)



sample_n(burden_school, size = 20)
