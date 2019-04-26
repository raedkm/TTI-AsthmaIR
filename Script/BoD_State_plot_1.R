#---------------------------------------------#
#Project : State specific Burden of childhood asthma due to TRAP - 2019
#Sub     : Plots
#Part    : (01) Preparing plots 
#Purpose : Read in census data, income data, NO2 conc, incidence rate (state-specifi), and prevelance rate (state, specific)
#         Followed by joining the data sets and replacing missing IR/PRV with weighted averages
#Created by Raed Alotaibi
#Date Created: April-26-2019
#---------------------------------------------#



library(ggplot2)




# Loading Burden data -----------------------------------------------------

# Missing code here

# Preventing scientific notations
options(scipen=10000)

# Sampling data set
set.seed(1)
index <- sample(1:nrow(burden), 1000000)
burden_s <- burden[index, ]

# Plotting ----------------------------------------------------------------

# AF boxplot 
burden_s %>%
  ggplot(aes(x = URBAN, y = AF*100)) +
  geom_boxplot() + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1)) +
  labs( y = "AF", x = "") +
  scale_fill_discrete(name = "Year") +
  facet_grid( ~ INCOME) 
  #ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
  #coord_flip() 
  
