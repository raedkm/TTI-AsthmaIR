#---------------------------------------------
#Project : Burden of childhood asthma due to TRAP project - 2018
#Part : (10) 500-Cities Join Data
#Purpose: To Join the burden data with the 500-Cities data set
#
#Created by Raed Alotaibi
#---------------------------------------------#

# Loading the burden data set
load("C:/Users/R-Alotaibi/Desktop/BoD/Data files/burden_All_join.RData")


# Should I join each year seperatly?

# Joining the year 2000 data set
Cities_burden_2000 <- Cities_2000 %>% 
  left_join(burden_2000, by = c("GISJOIN", "YEAR" = "year"))

# Joining the year 2010 data set
burden_2010 <- burden_All_join %>% 
  filter(year == 2010) 

Cities_burden_2010 <-  Cities_2010 %>% 
  left_join(burden_2010, by = "GISJOIN")


# Merging the Cities 2000 and Cities 2010 by rows
bind_rows()


str(Cities)

unique(Cities_burden$PlaceName)
unique(Cities_burden$PlaceFIPS)
unique(Cities_burden$STATE)


summary(Cities_burden)
#Note: Missing values are form two sources, (1)missing income data for 2010, (2)missing PM2.5 and PM10 conc for 2000
#Note: We will use 
