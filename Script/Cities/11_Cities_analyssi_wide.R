



Cities_burden_w <- Cities_pollutant_year %>%
  select(-County) %>% 
  group_by( year, PlaceFIPS) %>% 
  mutate(id=1:n()) %>%
  melt(id = c("id", "Pollutant","year","PlaceFIPS","City", "State")) %>% 
  dcast(... ~  variable + year , value.var="value") 
  

datatable(Cities_burden_w, filter = 'top', options = list(
  pageLength = 50, autoWidth = TRUE))


Cities_burden_w_2 <- Cities_burden_w %>% 
  group_by(Pollutant, PlaceFIPS) %>%
  mutate(id=1:n()) %>%
  melt(id=c("id", "Pollutant", "PlaceFIPS","City", "State", "Total_Population_2000",
            "Total_Population_2010", "Total_Children_2000",  "Total_Children_2010",
            "Total_Cases_2000", "Total_Cases_2010")) %>%
  dcast(... ~ Pollutant + variable    , value.var="value") %>% 
  select(-id, PlaceFIPS)



options(DT.options = list(pageLength = 5))

# Table Container
sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'FIPS'),
      th(rowspan = 2, 'City'),
      th(rowspan = 2, 'State'),
      th(colspan = 2, 'Total Population'),
      th(colspan = 2, 'Total Children'),
      th(colspan = 2, 'Total Cases'),
      th(colspan = 2, 'NO2  Attributable Cases'),
      th(colspan = 2, 'NO2  Fraction'),
      th(colspan = 2, 'NO2  Concentration'),
      th(colspan = 2, 'NO2  Weighted Concentration'),
      th(colspan = 2, 'PM10  Attributable Cases'),
      th(colspan = 2, 'PM10  Fraction'),
      th(colspan = 2, 'PM10  Concentration'),
      th(colspan = 2, 'PM10  Weighted Concentration'),
      th(colspan = 2, 'PM2.5  Attributable Cases'),
      th(colspan = 2, 'PM2.5  Fraction'),
      th(colspan = 2, 'PM2.5  Concentration'),
      th(colspan = 2, 'PM2.5  Weighted Concentration')
    ),
    tr(
      lapply(rep(c('2000', '2010'), 15), th)
    )
  )
))
print(sketch)


datatable(Cities_burden_w_2, filter = 'top', 
          options = list(pageLength = 50, autoWidth = TRUE), 
          class = 'cell-border stripe',
          editable = F,
          colnames = c('Total Population 2000' = 4),
          container = sketch,
          rownames = FALSE) 
     


df = data.frame(id = 1:15,
x = seq(from = 4, to = 33, by =2),
y = seq(from = 5, to = 33, by =2),
w = seq(from = 6, to = 35, by =2),
z = seq(from = 7, to = 35, by =2)
)


df1 = df

for(i in 3:ncol(df1))  df1[,i] = df[,i]-df[,(i-1)]

df1

delta <- function(col1, col2){
  change = round(( col2 - col1 )/col1, digit = 2)
  return(change)
}

df %>% 
  mutate(
    diff_1 = delta(df[,2],df[,3]))

####### 
Cities_burden_d <- Cities_burden_w_2 %>% 
  mutate(diff_1 = delta(Cities_burden_w_2[,4],Cities_burden_w_2[,5]),
         diff_2 = delta(Cities_burden_w_2[,6],Cities_burden_w_2[,7]),
         diff_3 = delta(Cities_burden_w_2[,8],Cities_burden_w_2[,9]),
         diff_4 = delta(Cities_burden_w_2[,10],Cities_burden_w_2[,11]),
         diff_5 = delta(Cities_burden_w_2[,12],Cities_burden_w_2[,13]),
         diff_6 = delta(Cities_burden_w_2[,14],Cities_burden_w_2[,15]),
         diff_7 = delta(Cities_burden_w_2[,16],Cities_burden_w_2[,17]),
         diff_8 = delta(Cities_burden_w_2[,18],Cities_burden_w_2[,19]),
         diff_9 = delta(Cities_burden_w_2[,20],Cities_burden_w_2[,21]),
         diff_10 = delta(Cities_burden_w_2[,22],Cities_burden_w_2[,23]),
         diff_11 = delta(Cities_burden_w_2[,24],Cities_burden_w_2[,25]),
         diff_12 = delta(Cities_burden_w_2[,26],Cities_burden_w_2[,27]),
         diff_13 = delta(Cities_burden_w_2[,28],Cities_burden_w_2[,29]),
         diff_14 = delta(Cities_burden_w_2[,30],Cities_burden_w_2[,31]),
         diff_15 = delta(Cities_burden_w_2[,32],Cities_burden_w_2[,33]),
         )

datatable(Cities_burden_d, filter = 'top', options = list(
  pageLength = 50, autoWidth = TRUE))



Cities_burden_b <- cbind(Cities_burden_w_2[4:33], Cities_burden_w_2[seq(5,33, by =2)])
Cities_burden_b <- Cities_burden_b[, sort(names(Cities_burden_b))]
Cities_burden_b[, seq(3, 45,by=3)] <- NA

## Note ( Add the mutate function above here !!!!)

dim(Cities_burden_b[, seq(3, 45,by=3)])
dim(Cities_burden_d[34:48])

df <- cbind(dat, dat[seq(2,4, by =2)])
df <- df[, sort(names(df))]
df[, seq(3, 6,by=3)] <- NA


dat <- data.frame(A = rnorm(5), B = rnorm(5), C = rnorm(5), D = rnorm(5))

df <- cbind(dat, dat[seq(2,4, by =2)])
df <- df[, sort(names(df))]
df[, seq(3, 6,by=3)] <- NA

seq(2, 8,by=2)


datatable(Cities_burden_b, filter = 'top', options = list(
  pageLength = 50, autoWidth = TRUE))



