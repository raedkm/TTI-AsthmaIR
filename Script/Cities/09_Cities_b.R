#---------------------------------------------#
#Project : Burden of childhood asthma due to TRAP project - 2018
#Part : (09b) 500-Cities census blocks
#Purpose: To add missing air pollution data to Cities census blocks
#
#Created by Raed Alotaibi
#---------------------------------------------#

#### Data set ####
# Loading Cities data set
load("Data files/Cities.RData")

#### Extracting pollutant values for missing census blocks ####

### NO2 ###

# Year 2000  #
  NO2_2000_a <- fread("Data files/raw_pollutant/NO2_2000_update.csv", data.table = F)
  
  NO2_2000 <- NO2_2000_a %>% 
    mutate(NO2 = Y2000*1.88) %>% 
    select(GISJOIN, NO2)

  
# Year 2010  #
  NO2_2010_a <- fread("Data files/raw_pollutant/NO2_2010.csv", data.table = F)
  
  NO2_2010 <- NO2_2010_a %>% 
    mutate(NO2 = Y2010*1.88) %>% 
    select(GISJOIN, NO2)
  
  
# Joining #
  pollut_NO2 <- bind_rows('2000' = NO2_2000, '2010' = NO2_2010, .id = "year")
  
  
  
### PM's ###
# Year 2000  #  
  load("Data files/raw_pollutant/CACES_P3v1_2000blocks.RData")

  PM_2000 <- preds.2000.00cw %>% 
    select(GJOIN2000, pm25.wght, pm10.wght) %>% 
    rename(GISJOIN = GJOIN2000, PM2.5 = pm25.wght,  PM10 = pm10.wght)



# Year 2000  #  
  load("Data files/raw_pollutant/CACES_P3v1_2010blocks.RData")

  PM_2010 <- preds.2010 %>% 
    select(GISJOIN, pm25, pm10) %>% 
    rename(PM2.5 = pm25,  PM10 = pm10)


# Joining #
  pollut_PM <- bind_rows('2000' = PM_2000, '2010' = PM_2010, .id = "year")
  

  
#### Joining the pollutants data sets by column (GISJOIN and Year)####
  pollut_long <- pollut_NO2 %>% 
      full_join( pollut_PM, by = c("GISJOIN", "year")) %>% 
      gather("Pollutant", "conc", c(NO2, PM2.5, PM10))
  
  # pollut_wide <- pollut_NO2 %>% 
  #   full_join( pollut_PM, by = c("GISJOIN", "year"))
  # 
  # 
  
  
#### Joining the Cities data set with pollutant data set by (GISJOIN, year and pollutant)####
Cities_long <- Cities %>% 
    left_join(pollut_long, by = c("GISJOIN", "year")) %>% 
    mutate_if(is.character, as.factor)

# Cities_wide <- Cities %>% 
#     left_join(pollut_wide, by = c("GISJOIN", "year")) %>% 
#     mutate_if(is.character, as.factor)
#   


#### Examinig the missing data  #### 
# For wide and long format data sets
Cities_long %>% 
  group_by(year, Pollutant) %>% 
  filter(is.na(conc)) %>% 
  summarize(missing_totalpop = sum(Total), missing_con = sum(is.na(conc)) )


# Cities_wide %>%
#   mutate(missing =  is.na(NO2 + PM2.5 + PM10)) %>%
#   group_by(year) %>%
#   filter(missing == TRUE) %>%
#   summarize(missing_totalpop = sum(Total), missing_NO2 = sum(is.na(NO2)), missing_PM2.5 = sum(is.na(PM2.5)),missing_PM10 = sum(is.na(PM10))  )
# 

  

# Saving the Cities_long data set
save(Cities_long, file = "Data files/Cities_long.RData")



