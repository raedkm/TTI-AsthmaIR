#Created by Raed Alotaibi
#Creation Date: 3/11/19 
#Purpose: Analysing 2008 childhod asthma incidence rates in the US by loading two data sets the BRFSS and ACBS

Sys.time()

# Houskeeping
ls()
rm(list=ls())
search()

#Check working directory
getwd()

#loading libraries
library(foreign)
library(dplyr)
library(questionr)
library(xlsx)
library(tidyr)

# Loading data set
load("Data/FIPS")
ACBS_a <- read.spss("Data/ACBS_2008.SAV", 
                    use.value.labels = F,
                    to.data.frame = T, 
                    trim.factor.names = T, 
                    trim_values = T)

BRFSS_a <- read.xport("Data/CDBRFS08.XPT")


# ACBS "Child" --------------------------------------------------------------

# Selecting variables
ACBS_b <- select(ACBS_a, "SEQNO","X._STATE", "INCIDNT", "CHILDWT_F")


# Sampel count Estimates ---------------------------------------------------------
ltabs(~INCIDNT, ACBS_b)
ltabs(~X._STATE+INCIDNT, ACBS_b)

ACBS_sample <- as.data.frame.matrix(ltabs(~X._STATE+INCIDNT, ACBS_b))
ACBS_sample <- tibble::rownames_to_column(ACBS_sample, "FIPS")
ACBS_sample <- full_join(FIPS, ACBS_sample, by = "FIPS")



# Weighted estimates ------------------------------------------------------
ACBS_c <- wtd.table(ACBS_b$INCIDNT, y = ACBS_b$X._STATE, weights = ACBS_b$CHILDWT_F, normwt = F, na.show = F)
(ACBS_d <- as.data.frame.matrix(t(ACBS_c))) # converting to data frame


# Adding FIPS(state code) from rownames -------------------------------------------------------------
ACBS_e <- tibble::rownames_to_column(ACBS_d, "FIPS")
ACBS_e <- full_join(FIPS, ACBS_e, by = "FIPS")




# BRFSS "Child" ----------------------------------------------------------


# Selecting variables
BRFSS_b <- select(BRFSS_a, "X_STATE","CASTHDX2","X_CHILDWT")


# Sample count estimates ---------------------------------------------------------
ltabs(~CASTHDX2, BRFSS_b)
ltabs(~X_STATE+CASTHDX2, BRFSS_b)

BRFSS_sample <- as.data.frame.matrix(ltabs(~X_STATE+CASTHDX2, BRFSS_b))
BRFSS_sample <- tibble::rownames_to_column(BRFSS_sample, "FIPS")
BRFSS_sample <- full_join(FIPS, BRFSS_sample, by = "FIPS")



# Weighted estimates ------------------------------------------------------
BRFSS_c <- wtd.table(BRFSS_b$CASTHDX2, y = BRFSS_b$X_STATE, weights = BRFSS_b$X_CHILDWT, normwt = F, na.show = F)
(BRFSS_d <- as.data.frame.matrix(t(BRFSS_c))) # converting to data frame


# Adding FIPS(state code) from rownames -------------------------------------------------------------
BRFSS_e <- tibble::rownames_to_column(BRFSS_d, "FIPS")
BRFSS_e <- full_join(FIPS, BRFSS_e, by = "FIPS")




# Joining "ACBS" and "BRFSS"  ----------------------------------------------------------------


# Sample Counts
Asthma_sample <- full_join(ACBS_sample, BRFSS_sample, by = "FIPS", suffix = c("_ACBS", "_BRFSS"))
Asthma_sample <- Asthma_sample %>% arrange(as.numeric(FIPS))


# Weighted estimates
Asthma_weighted <- full_join(ACBS_e, BRFSS_e, by = "FIPS", suffix = c("_ACBS", "_BRFSS"))
Asthma_weighted <- Asthma_weighted %>% arrange(as.numeric(FIPS))


# Estimating Incidence Rate -----------------------------------------------


# Asthma Incidence rate
Asthma_IR <- Asthma_weighted %>% mutate(IR_per1000 = `1_ACBS` / (`1_ACBS` + `2_BRFSS`)*1000) %>% select(FIPS, IR_per1000)



# Printing to Excel -------------------------------------------------------

write.xlsx(Asthma_sample, "Asthma_result.xlsx", sheetName = "2008_count", showNA=F, append = T)
write.xlsx(Asthma_weighted, "Asthma_result.xlsx", sheetName = "2008_weighted", showNA=F, append = T)
write.xlsx(Asthma_IR, "Asthma_IR.xlsx", sheetName = "2008", showNA=F, append = T)

