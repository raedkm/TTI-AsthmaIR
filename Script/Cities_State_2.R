#---------------------------------------------#
#Project : State specific Burden of childhood asthma due to TRAP - 2019
#Part : (05) Preparing 500-Cities data set
#Purpose: 
#Created by Raed Alotaibi
#Date Created: March-27-2019
#---------------------------------------------#



# Interactive table
table_1 <- datatable(cities_table, filter = 'top', options = list(
  pageLength = 50, autoWidth = TRUE))



# Table Container
sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'FIPS'),
      th(rowspan = 2, 'City'),
      th(rowspan = 2, 'State'),
      th('Total Children'),
      th('Total Cases'),
      th('NO2  Attributable Cases'),
      th('NO2  Fraction'),
      th('NO2  Concentration')
    ),
    tr(
      lapply(rep(c('2010'), 5), th)
    )
  )
))
print(sketch)


# Table with container
datatable(cities_table, filter = 'top', options = list(
  pageLength = 50, autoWidth = TRUE))

cities_state_IT <- datatable(cities_table, filter = 'top', 
          options = list(pageLength = 50, autoWidth = TRUE), 
          class = 'cell-border stripe',
          editable = F,
          colnames = c('Total Population 2000' = 4),
          container = sketch,
          rownames = FALSE) 


# Saving file

htmltools::save_html(cities_state_IT, file = "500_cities_stateIR.html")
