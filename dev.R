devtools::document()

devtools::use_package('magrittr')

con = pgr::pg_con(mdb1252, driver = PostgreSQL)

library(tidyverse)

pg_show(con,'codigos','localidad')
