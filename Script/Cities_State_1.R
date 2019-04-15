#---------------------------------------------#
#Project : State specific Burden of childhood asthma due to TRAP - 2019
#Part : (05) Preparing 500-Cities data set
#Purpose: 
#Created by Raed Alotaibi
#Date Created: March-27-2019
#---------------------------------------------#


# Loading the 500-Cities 
path_cities <- "Data\\Cities.csv"

var_cities <- c("StateAbbr", "PlaceName" ,"PlaceFIPS","Population2010" )

cities <-  fread(path_cities, data.table = F, verbose = T, select = var_cities) %>%
  mutate(PLACEA = stri_sub(PlaceFIPS, -5, -1)) %>%
  mutate(PlaceFIPS = str_pad(PlaceFIPS, 7, pad = "0")) %>%
  mutate(FIPS = stri_sub(PlaceFIPS, 1,2)) %>%
  as_tibble()


# Joining the 500-Cities data with the state specific burden

cities_burden <- burden %>% 
  right_join(cities, by ="PlaceFIPS") %>% 
  select(GISJOIN, PlaceFIPS, STATE, StateAbbr, PlaceName, CHILDREN, IR, PRV, CASES, AC, AF, NO2)
  


# creating mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



# Porducing table for 500 cities without filter
cities_table <- cities_burden %>% 
  group_by(PlaceFIPS) %>% 
  summarise(City = getmode(PlaceName),
            State = getmode(STATE),
            Total_Children = sum(CHILDREN), 
            Total_Cases = round(sum(CASES), digits = 0), 
            Total_AC = round(sum(AC), digits = 0),
            Fraction = round(Total_AC/Total_Cases, digits = 2),
            Mean_Con = round(mean(NO2, na.rm = T), digits = 2))



