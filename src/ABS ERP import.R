library(data.table)
library(readxl)
library(plyr)
library(dplyr)

load_SA2_ERP <- function(filename = 'c:/local datasets/shapefiles/ABS Statistical Areas/ERP by age and SA2.xlsx')
{
# SA2_ERPs <-
#   as.data.table(read_excel('c:/local datasets/shapefiles/ABS Statistical Areas/32180ds0006_2016-17.xls', 
#                            sheet = 'Table 2',
#                            col_types = c('skip', 'skip', 'skip', 'skip',
#                                          'numeric', 'text', 'numeric', 'text', 'numeric', 'text', 
#                                          'numeric', 'numeric',
#                                          'skip', 'skip', 'skip', 'skip', 'skip', 'skip', 'skip', 'skip',
#                                          'numeric', 'skip'),
#                            n_max = 463,
#                            skip = 8,
#                            col_names = c('SA4 code', 'SA4 name', 'SA3 code', 'SA3 name', 'SA2 code', 'SA2 name', '2016', '2017', 'Area km')))

  SA2_ERPs_age <- 
    as.data.table(read_excel(filename, 
                             skip = 7)) %>%
    rename('SA2_Name' = '..6') %>%
    select(-starts_with('..')) %>%
    select(-Age) %>%
    filter(!is.na(SA2_Name)) %>%
    melt(variable.name = 'Age', id.vars = 'SA2_Name', value.name = 'ERP') %>%
    filter(!is.na(ERP) & Age != 'All ages')
  return(SA2_ERPs_age)
}