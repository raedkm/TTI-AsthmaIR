# Loading NO2 data --------------------------------------------------------

var_lur <- c("GISJOIN", "Y2010")

MD_NO2 <- fread("Data\\Pollutant\\NO2_2010.csv", data.table = F, stringsAsFactors = F,  verbose = T, select = var_lur) %>% 
  mutate(MD = str_detect(GISJOIN, "G24")) %>% 
  mutate(NO2 = Y2010*1.88) %>% 
  filter(MD == TRUE) %>% 
  select(GISJOIN, NO2) %>% 
  as_tibble()

MD_pred <- pred %>%
  mutate(MD = str_detect(GISJOIN, "G24")) %>% 
  filter(MD == TRUE) %>% 
  select(-no2, -MD) %>% 
  as_tibble()
 
MD <- left_join(MD_NO2, MD_pred, by = "GISJOIN") %>% 
  filter(NO2 > 0) %>% 
  select(-block_fip)


xlsx::write.xlsx(MD, "Results/MD_conc.xlsx",  showNA=F, append = T, row.names = F)

write.csv(MD, "Results/MD_conc.csv", row.names = F)
