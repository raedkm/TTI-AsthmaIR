#---------------------------------------------
#Project : Burden of childhood asthma due to TRAP project - 2018
#Part : (11) 500-Cities and Burden Analysis
#Purpose: To Analys the burden data with the 500-Cities data set
#
#Created by Raed Alotaibi
#---------------------------------------------#

#Loading libraries
library(DT)

# Loading the Cities burden data set
load("Data files/Cities_burden.RData")

# creating mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}



# Porducing table for 500 cities without filter
Cities_pollutant_year <- Cities_burden %>% 
  group_by( Pollutant, year, PlaceFIPS) %>% 
  summarise(City = getmode(PlaceName),
            County = getmode(COUNTY),
            State = getmode(STATE),
            Total_Population = sum(Total), 
            Total_Children = sum(total_children), 
            Total_Cases = round(sum(cases), digits = 0), 
            Total_AC = round(sum(AC), digits = 0),
            Fraction = round(Total_AC/Total_Cases, digits = 2),
            Mean_Con = round(mean(conc, na.rm = T), digits = 2), 
            Weight_Con  = round(sum((conc * Total)/sum(Total), na.rm = T), digits = 2))






# Interactive table
table_500 <- datatable(Cities_pollutant_year, filter = 'top', options = list(
  pageLength = 50, autoWidth = TRUE))



# Producing wide table
Cities_burden_w <- Cities_pollutant_year %>%
  gather(variable, value, -(Pollutant:State)) %>%
  unite(temp, variable, year) %>%
  spread(temp, value)



# # Producing tables for 500 sities summaries for year 2000 and 2010 separatly
# Cities_2010 <- Cities_burden_c %>% 
#   filter(Pollutant == 'NO2', year == 2010) %>% 
#   group_by( PlaceName) %>% 
#   summarise(Total_population = sum(Total), 
#             Total_children = sum(total_children), 
#             Total_cases = round(sum(cases), digits = 0), 
#             Total_AC = round(sum(AC), digits = 0),
#             Fraction = Total_AC/Total_cases,
#             Mean_con = mean(conc, na.rm = T), 
#             conc_pop1  = sum((conc * Total)/sum(Total), na.rm = T))
# 
# Cities_2000 <- Cities_burden_c %>% 
#   filter(Pollutant == 'NO2', year == 2000) %>% 
#   group_by( PlaceName) %>% 
#   summarise(Total_Population = sum(Total), 
#             Total_Children = sum(total_children), 
#             Total_Cases = round(sum(cases), digits = 0), 
#             Total_AC = round(sum(AC), digits = 0),
#             Fraction = Total_AC/Total_Cases,
#             Mean_con = mean(conc, na.rm = T), 
#             conc_pop1  = sum((conc * Total)/sum(Total), na.rm = T))
# 





# # Interactive table function (by year)
# cities_table  <- function(pollutant) {
# 
#   Cities <- Cities_burden_c %>% 
#   filter(Pollutant == pollutant) %>% 
#   group_by( year, PlaceName) %>% 
#   summarise(Total_Population = sum(Total), 
#             Total_Children = sum(total_children), 
#             Total_Cases = round(sum(cases), digits = 0), 
#             Total_AC = round(sum(AC), digits = 0),
#             Fraction = Total_AC/Total_cases,
#             Mean_Con = mean(conc, na.rm = T), 
#             Weight_Con  = sum((conc * Total)/sum(Total), na.rm = T))
# 
#   datatable(Cities, options = list(pageLength = 50))
# }
# 
# 
# # Interactive table function
# cities_year_compare  <- function(pollutant) {
#   
#   Cities <- Cities_burden_c %>% 
#     filter(Pollutant == pollutant) %>% 
#     group_by(PlaceName) %>% 
#     summarise(Total_population = sum(Total), 
#               Total_children = sum(total_children), 
#               Total_cases = round(sum(cases), digits = 0), 
#               Total_AC = round(sum(AC), digits = 0),
#               Fraction = Total_AC/Total_cases,
#               Mean_con = mean(conc, na.rm = T), 
#               conc_pop1  = sum((conc * Total)/sum(Total), na.rm = T))
#   
#   datatable(Cities, options = list(pageLength = 50))
# }

