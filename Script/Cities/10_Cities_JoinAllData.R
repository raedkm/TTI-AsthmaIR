#---------------------------------------------
#Project : Burden of childhood asthma due to TRAP project - 2018
#Part : (10) 500-Cities and Burden join data set
#Purpose: To Join the burden data with the 500-Cities data set and examin charactaristics of missing data
#
#Created by Raed Alotaibi
#---------------------------------------------#

# Loading the burden and Cities_long data set
load("C:/Users/R-Alotaibi/Desktop/BoD/Data files/burden_All_join.RData")
load("Data files/Cities_long.RData")

# Converting characters to factors
burden_All_join_a <- burden_All_join %>% 
  mutate_if(is.character, as.factor)


Cities_burden <- burden_All_join_a %>% 
  select(-conc) %>% 
  right_join(Cities_long, by = c("GISJOIN", "year", "Pollutant")) %>%
  mutate(total_children = if_else(is.na(total_children), 0, total_children),
         cases = if_else(is.na(cases), 0, cases),
         Urban = as.factor(if_else(is.na(Urban), "Nopop", as.character(Urban))),
         AC = if_else(is.na(AC), 0, AC)) 



# # Examinig missing data
# Cities_burden %>% 
#   group_by(year, Pollutant) %>% 
#   filter(is.na(conc)) %>% 
#   summarize(missing_totalpop = sum(Total),missing_children = sum(total_children),  missing_con = sum(is.na(conc)) )
# 


# # Examinig blocks that joined and did not join
# Cities_burden_anti <- anti_join(Cities_burden, burden_All_join_a, by = c("GISJOIN", "year"))
# Cities_burden_semi <- semi_join(Cities_burden, burden_All_join_a, by = c("GISJOIN", "year"))


# summary(Cities_burden_semi) 
# # All children have been accounted for, the missing counts of children among blocks should have a value of zero 
# 







# Saving Cities_Burden
save(Cities_burden, file = "Data files/Cities_burden.RData")


# Todo list:
#Update Nov - 20 (Last step trying to replace missing values with zeros) [complete]
#Update Nov - 27 (Fix county names by removing extra "county") [pending]



