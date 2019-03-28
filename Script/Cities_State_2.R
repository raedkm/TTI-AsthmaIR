#---------------------------------------------#
#Project : State specific Burden of childhood asthma due to TRAP - 2019
#Part : (05) Preparing 500-Cities data set
#Purpose: 
#Created by Raed Alotaibi
#Date Created: March-27-2019
#---------------------------------------------#


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
table_IR <- datatable(cities_table, filter = 'top', 
          options = list(pageLength = 50, autoWidth = TRUE), 
          class = 'cell-border stripe',
          editable = F,
          colnames = c('Total Population 2000' = 4),
          container = sketch,
          rownames = FALSE) 


saveWidget(table_IR, file = "cities_state_IR.html")
