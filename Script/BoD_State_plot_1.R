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
library(scales)
library(forcats)



# Loading Burden data -----------------------------------------------------


# Preventing scientific notations
options(scipen=10000)

# Sampling data set & renming levels of income
set.seed(1)
index <- sample(1:nrow(burden), 10000)
burden_s <- burden[index, ]  

burden_s %>% 
  mutate(INCOME = recode(burden[index,]$INCOME,  
                         "<20,000" = "<$20,000" , 
                         "20,000 to <35,000" = "$20,000 to <$35,000", 
                         "35,000 to <50,000" = "$35,000 to <$50,000", 
                         "50,000 to <75,000" = "$50,000 to <$75,000" , 
                         ">=75,000" = ">=$75,000"))

levels(burden$INCOME)


# Assiging layers ---------------------------------------------------------

theme_text <-  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1),
                     axis.title.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.ticks.x=element_blank())

theme_text2 <- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=1),
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank(),
                    axis.ticks.x=element_blank())
  

breaks_m <- seq(0,0.5,0.05)
scale_y_bod <-   scale_y_continuous(breaks  = breaks_m,  labels = percent_format(accuracy = 1))


# Plotting ----------------------------------------------------------------


# 2.	NO2 concentration by living location
 burden_s %>%
  ggplot(aes(x = URBAN, y = NO2)) +
  geom_boxplot() + 
  theme_bw()+
  theme_text +
  ggsave("p2.png" ,path = "Plots",  width = 9.5, height = 6,  dpi = 920, pointsize=12 )

                  

# 3.	NO2 concentration by median income grp
 burden_s %>%
  ggplot(aes(x = INCOME, y = NO2)) +
  geom_boxplot() + 
  theme_bw()+
  theme_text +
  ggsave("p3.png" ,path = "Plots",  width = 9.5, height = 6,  dpi = 920, pointsize=12)



# 4.	NO2 concentration by living location stratified into median income grp
burden_s %>%
  ggplot(aes(x = URBAN, y = NO2)) +
  facet_grid( ~ INCOME) +
  geom_boxplot() + 
  theme_bw()+
  theme_text +
  ggsave("p4.png" ,path = "Plots", width = 9.5, height = 6,  dpi = 920, pointsize=12)



# 5.	NO2 concentration by median income grp stratified into living location
 burden_s %>%
  ggplot(aes(x = INCOME, y = NO2)) +
  facet_grid( ~ URBAN) +
  geom_boxplot() + 
  theme_bw()+
  theme_text +
  ggsave("p5.png" ,path = "Plots", width = 9.5, height = 6,  dpi = 920, pointsize=12)


# 6.	NO2 concentration by state
burden_s %>%
  ggplot(aes(x= reorder(STATE, NO2, FUN = median), y = NO2)) +
  geom_boxplot() + 
  theme_bw()+
  theme_text2 +
  ggsave("p6.png" ,path = "Plots", width = 9.5, height = 6,  dpi = 920, pointsize=12)



# 7.	NO2 concentration by state and living location
burden_s %>%
  ggplot(aes(x= INCOME, y = NO2)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot() + 
  theme_bw() +
  theme_text2 +
  ggsave("p7.png" ,path = "Plots", width = 9.5, height = 12,  dpi = 920, pointsize=12)



# 8.	NO2 concentration by state and median income grp
 burden_s %>%
  ggplot(aes(x= URBAN, y = NO2)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot() + 
  theme_bw() +
  theme_text2 +
  ggsave("p8.png" ,path = "Plots", width = 9.5, height = 12,  dpi = 920, pointsize=12)



# 10.	AF concentration by living location
burden_s %>%
  ggplot(aes(x = URBAN, y = AF)) +
  geom_boxplot() + 
  theme_bw()+
  theme_text2 +
  scale_y_bod +
  ggsave("p10.png" ,path = "Plots" , width = 9.5, height = 12,  dpi = 920, pointsize=12)



# 11.	AF concentration by median income grp
burden_s %>%
  ggplot(aes(x = INCOME, y = AF)) +
  geom_boxplot() + 
  theme_bw()+
  theme_text2 +
  scale_y_bod +
  ggsave("p11.png" ,path = "Plots", width = 9.5, height = 12,  dpi = 920, pointsize=12)



# 12.	AF concentration by living location stratified into median income grp
burden_s %>%
  ggplot(aes(x = URBAN, y = AF)) +
  facet_grid( ~ INCOME) +
  geom_boxplot() + 
  theme_bw()+
  theme_text2 +
  scale_y_bod +
  ggsave("p12.png" ,path = "Plots", width = 9.5, height = 12,  dpi = 920, pointsize=12)



# 13.	AF concentration by median income grp stratified into living location
burden_s %>%
  ggplot(aes(x = INCOME, y = AF)) +
  facet_grid( ~ URBAN) +
  geom_boxplot() + 
  theme_bw()+
  theme_text2 +
  scale_y_bod +
   ggsave("p13.png" ,path = "Plots", width = 9.5, height = 12,  dpi = 920, pointsize=12)
 


# 14.	AF by state 
 burden_s %>%
   ggplot(aes(x= reorder(STATE, AF, FUN = median), y = AF)) +
   geom_boxplot() + 
  theme_bw()+
   theme_text2 +
   scale_y_bod +
   ggsave("p14.png" ,path = "Plots", width = 9.5, height = 12,  dpi = 920, pointsize=12)
 
 



# 15.	AF by state and median income grp
burden_s %>%
  ggplot(aes(x= INCOME, y = AF)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot() + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=1)) +
  #ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
  coord_flip() +
  ggsave("p15.png" ,path = "Plots")



# 16.	AF by state and living location
burden_s %>%
  ggplot(aes(x= URBAN, y = AF)) +
  facet_wrap( ~ STATE, nrow =  7 ) +
  geom_boxplot() + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=1)) +
  #ggtitle("Change in the percentage of all asthma incident cases due to TRAP between 2000 and 2010") 
  coord_flip() +
  ggsave("p16.png" ,path = "Plots")




