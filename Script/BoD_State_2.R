#---------------------------------------------#
#Project : State specific Burden of childhood asthma due to TRAP - 2019
#Part : (02) Preparing census data 
#Purpose: Conduct the burden modeling and estimate results by state/urban
#Created by Raed Alotaibi
#Date Created: March-12-2019
#---------------------------------------------#



# Total Incidenct Cases ---------------------------------------------------

incident <- join %>%
  mutate(CASES = (CHILDREN - (CHILDREN * PRV)) * IR)

        



# Burden Modeling ---------------------------------------------------------

## Estimating (RR of new exposure: RRnew, Attributable fraction; AF, Attributable cases; AC with lower and upper limits)

crf <- 1.05
unit_inc <- 4

burden <- incident %>% 
  mutate(RRnew = exp((log(crf)/unit_inc)*NO2)) %>% 
  mutate(AF = (RRnew - 1)/(RRnew)) %>% 
  mutate(AC = AF*CASES)
  



# Result ----------------------------------------------------------------


# Total

burden_1 <- burden %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, -2), AC = round(AC, -1), AF = round(AF, 1))

# State 

burden_2 <- burden %>% 
  group_by(STATE) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, -2), AC = round(AC, -1), AF = round(AF, 1))


# Urban status

burden_3 <- burden %>% 
  group_by(URBAN) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, -2), AC = round(AC, -1), AF = round(AF, 1))



# State/Urban status

burden_4 <- burden %>% 
  group_by(STATE, URBAN) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, -2), AC = round(AC, -1), AF = round(AF, 1))


# Urbane/State status

burden_5 <- burden %>% 
  group_by(URBAN, STATE) %>% 
  summarise(CHILDREN = sum(CHILDREN), CASES = sum(CASES), AC = sum(AC))%>% 
  mutate(AF = AC/CASES * 100) %>% 
  mutate(CHILDREN = round(CHILDREN), CASES = round(CASES, -2), AC = round(AC, -1), AF = round(AF, 1))



# Printing ----------------------------------------------------------------

burden_list <- list(burden_1, burden_2, burden_3, burden_4, burden_5)
sheet <- c("Total","By State", "By Urban", "By State|Urban", "By Urban|State")

for(i in 1:length(sheet)) {
  print(i)
  print(sheet[i])
  # Printing Aggregated data to Excel
  write.xlsx(burden_list[i], "Results/Burden_by_state.xlsx", sheetName = sheet[i], showNA=F, append = T, row.names = F)
  
  }



