#---------------------------------------------
#Project : Burden of childhood asthma due to TRAP project - 2018
#Part : (09_backup) 500-Cities census blocks
#Purpose: To extract the census blocks included in the 500-Cities data base
#
#Created by Raed Alotaibi
#---------------------------------------------#

Cities_500 <- function(){ 
  
  # Loading children census based on year selection
  if (year == 2000){
    Place <- fread("Data files/raw_census/NHGIS_2000_Sum.csv", data.table = F)
  }else if(year == 2010){
    Place <- fread("Data files/raw_census/NHGIS_2010_Sum.csv", data.table = F)
  }
  
  
  # Selecting desired columns (PLACEA is the city code)
  
  
  # Loading the 500-cities 
  Cities_all <-  fread("Data files/Cities.csv", data.table = F)
  names(Cities_all)
  
  
  # Selecting desired columns from the 500-cities (state abbreviation, city name, city FIPS, city 2010 popultion count)
  Cities_1 <-  Cities_all %>% select(StateAbbr, PlaceName, PlaceFIPS) 
  head(Cities_1)
  
  
  # Creating a PlaceFIPS variable for joining 500-Cities with Place data set
  Cities_2 <- Cities_1 %>% mutate(PALCEA = stri_sub(  PlaceFIPS, -5, -1))
  head(Cities_2)
  
  
  # Padding the fips code with left hand zeros to complete 7 digit number
  Cities_2$PlaceFIPS <- str_pad(Cities_2$PlaceFIPS, 7, pad = "0")
  head(Cities_2)
  
  Place <- Place %>%  mutate(PlaceFIPS = str_c(stri_sub(GISJOIN, 2,3), str_pad(Place$PLACEA, 5, pad = "0")))
  head(Place)
  
  
  #Joining the two data sets
  Cities_3 <- inner_join(Cities_2, Place , by = "PlaceFIPS")
  
  
  # There are 8,175,133 total US population living within the 500-cities data set
  
  # Selecting desired columns
  # Loading children census based on year selection
  if (year == 2000){
    Cities <- Cities_3 %>% select(GISJOIN, STATE, COUNTY, StateAbbr, PlaceName, PlaceFIPS, FXS001)
  }else if(year == 2010){
    Cities <- Cities_3 %>% select(GISJOIN, STATE, COUNTY, StateAbbr, PlaceName, PlaceFIPS, H7V001)
  }
  
  bind_rows()
  
  if(year == 2000){
    Cities_2000 <<- Cities
    return(Cities_2000)
  }else if(year == 2010){
    Cities_2010 <<- Cities
    return(Cities_2010)  
  }
  
}


# Running the function for years 2000 and 2010

# Cities-500 year 2000
Cities_500(year = 2000)

# Cities-500 year 2010
Cities_500(year = 2010)

