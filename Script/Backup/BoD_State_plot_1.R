#---------------------------------------------#
#Project : State specific Burden of childhood asthma due to TRAP - 2019
#Sub     : Plots
#Part    : (01) Preparing plots 
#Purpose : Read in census data, income data, NO2 conc, incidence rate (state-specifi), and prevelance rate (state, specific)
#         Followed by joining the data sets and replacing missing IR/PRV with weighted averages
#Created by Raed Alotaibi
#Date Created: April-26-2019
#---------------------------------------------#

#Note: Run "BoD_State_1.R" first to obtain dataset.

library(ggplot2)




# Loading Burden data -----------------------------------------------------

# Missing code here

# Preventing scientific notations
options(scipen=10000)

# Sampling data set
set.seed(1)
index <- sample(1:nrow(burden), 100000)
burden_s <- burden[index, ]

# Plotting ----------------------------------------------------------------



# 2.	NO2 concentration 

burden_s %>%
  ggplot(aes(y = NO2)) +
  geom_boxplot() + 
  theme_bw()
  #theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1)) 
  #labs( y = "AF", x = "") +
  #scale_fill_discrete(name = "Year") 
#  gtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
#coord_flip() 



# 3.	NO2 concentration by living location
burden_s %>%
  ggplot(aes(x = URBAN, y = NO2)) +
  geom_boxplot() + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1)) 
#ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
#coord_flip() 



# 4.	NO2 concentration by median income grp
burden_s %>%
  ggplot(aes(x = INCOME, y = NO2)) +
  geom_boxplot() + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1)) 
#ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
#coord_flip() 



# 5.	NO2 concentration by living location stratified into median income grp
burden_s %>%
  ggplot(aes(x = URBAN, y = NO2)) +
  facet_grid( ~ INCOME) +
  geom_boxplot() + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1)) 
#ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
#coord_flip() 



# 6.	NO2 concentration by median income grp stratified into living location
burden_s %>%
  ggplot(aes(x = INCOME, y = NO2)) +
  facet_grid( ~ URBAN) +
  geom_boxplot() + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1)) 
#ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
#coord_flip() 


# 7.	NO2 concentration by state
burden_s %>%
  ggplot(aes(x= STATE, y = NO2)) +
  geom_boxplot() + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1)) 
#ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
#coord_flip() 

# 8.	NO2 concentration by state and living location
burden_s %>%
  ggplot(aes(x= INCOME, y = NO2)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot() + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=1)) +
#ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
coord_flip() 



# 9.	NO2 concentration by state and median income grp
burden_s %>%
  ggplot(aes(x= URBAN, y = NO2)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot() + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=1)) +
#ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
coord_flip() 



# 10.	Attributable Fraction (AF)
burden_s %>%
  ggplot(aes(y = AF)) +
  geom_boxplot() + 
  theme_bw()
#theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1)) 
#labs( y = "AF", x = "") +
#scale_fill_discrete(name = "Year") 
#  gtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
#coord_flip() 


# 11.	AF concentration by living location
burden_s %>%
  ggplot(aes(x = URBAN, y = AF)) +
  geom_boxplot() + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1)) 
#ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
#coord_flip() 



# 12.	AF concentration by median income grp
burden_s %>%
  ggplot(aes(x = INCOME, y = AF)) +
  geom_boxplot() + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1)) 
#ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
#coord_flip() 



# 13.	AF concentration by living location stratified into median income grp
burden_s %>%
  ggplot(aes(x = URBAN, y = AF)) +
  facet_grid( ~ INCOME) +
  geom_boxplot() + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1)) 
#ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
#coord_flip() 



# 14.	AF concentration by median income grp stratified into living location
burden_s %>%
  ggplot(aes(x = INCOME, y = AF)) +
  facet_grid( ~ URBAN) +
  geom_boxplot() + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1)) 
#ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
#coord_flip() 



# 15.	AF by state 
burden_s %>%
  ggplot(aes(x= STATE, y = AF)) +
  geom_boxplot() + 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1)) 
#ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
#coord_flip() 



# 16.	AF by state and median income grp
burden_s %>%
  ggplot(aes(x= INCOME, y = AF)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot() + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=1)) +
  #ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
  coord_flip() 



# 17.	AF by state and living location
burden_s %>%
  ggplot(aes(x= URBAN, y = AF)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot() + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=1)) +
  #ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
  coord_flip() 



# Scatter plot ------------------------------------------------------------
burden_s %>% 
ggplot(aes(x = NO2, y = TOTAL))+
  geom_point() +
 coord_flip()
