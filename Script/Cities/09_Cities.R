#---------------------------------------------#
#Project : Burden of childhood asthma due to TRAP project - 2018
#Part : (09) 500-Cities census blocks
#Purpose: To extract the census blocks included in the 500-Cities data base
#
#Created by Raed Alotaibi
#---------------------------------------------#

#### Data set ####
# Loading children census based on year selection

Place_2000 <- fread("Data files/raw_census/NHGIS_2000_Sum.csv", data.table = F)
Place_2010 <- fread("Data files/raw_census/NHGIS_2010_Sum.csv", data.table = F)


# Selecting desired columns and renaming some columns
Place_2000 <-  Place_2000[-1,] %>% 
  select(GISJOIN, YEAR, STATE, COUNTY, PLACEA, FXS001) %>% 
  rename_at(c("FXS001", "YEAR"),~c("Total", "year"))  


Place_2010 <-  Place_2010 %>% 
  select(GISJOIN, YEAR, STATE, COUNTY, PLACEA, H7V001) %>% 
  rename_at(c("H7V001","YEAR"), ~c("Total", "year"))


# Binding the two data sets
Place <- rbind(Place_2000, Place_2010)


# Changin variable type to proper
Place$Total <- as.numeric(Place$Total)  

# Creating a PlaceFIPS code    
Place <- Place %>%  mutate(PlaceFIPS = str_c(stri_sub(GISJOIN, 2,3), str_pad(Place$PLACEA, 5, pad = "0")))
head(Place)  


# Loading the 500-cities 
Cities_all <-  fread("Data files/Cities.csv", data.table = F)
names(Cities_all)



# Selecting desired columns from the 500-cities (state abbreviation, city name, city FIPS, city 2010 popultion count)
Cities_1 <-  Cities_all %>% select(StateAbbr, PlaceName, PlaceFIPS) 
head(Cities_1)


# To create teh PlaceFIPS should be 7 digits long, to create strip the first 2 digits-(state  fips) then pad with (0)
Cities_2 <- Cities_1 %>% mutate(PALCEA = stri_sub(  PlaceFIPS, -5, -1))
head(Cities_2)


# Padding the fips code with left hand zeros to complete 7 digit number
Cities_2$PlaceFIPS <- str_pad(Cities_2$PlaceFIPS, 7, pad = "0")
head(Cities_2)




#Joining the two data sets
Cities <- inner_join(Cities_2, Place , by = "PlaceFIPS")
head(Cities)


# Examinig Cities that joined and did not join
Cities_anti <- anti_join(Cities_2, Place, by = "PlaceFIPS")
Cities_semi <- semi_join(Cities_2, Place, by = "PlaceFIPS")


# Examining the total population included in both years
Cities %>% 
  group_by(year) %>% 
  summarise(Total = sum(Total))

Place %>% 
  group_by(year) %>% 
  summarise(Total = sum(Total))

# Total US population living within the 500-cities data set = 
#  Year   Total
#1 2000   92,479,890
#2 2010  101,775,775 

# Total US population in the census data set
#  Year  Total
#1 2000  279,583,437
#2 2010  306,675,006

# Saving the Cities data set
save(Cities, file = "Data files/Cities.RData")

