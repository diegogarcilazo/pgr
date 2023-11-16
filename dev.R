devtools::document()
usethis::use_testthat()
devtools::test()
usethis::use_package('readr')
usethis::use_test()
con = pgr::pg_con(mdb1252, driver = Postgres)

con <- PgCon$new('mdb1252')

con$export(tibble::tibble(a = 1:4), table_name = "prueba")

con$import('C:/Users/Diego/Dropbox/SQL/my_views/PruebaDeptos_er.sql')

con$describeTable("mort80")

con$addValue_client_encoding <- "UTF8"

con$client_encoding

pgr::pg_tbl(con$pg_con, "deptos_er")

rm(con)

con$disconnect()

DBI::dbListConnections(drv = RPostgreSQL::PostgreSQL())

DBI::dbDisconnect()

gc()

library(here)
library(stringr)

query <- readr::read_file(
  here("C:/Users/Diego/Dropbox/SQL/my_views/PruebaDeptos_er.sql")
  )

str_detect("C:/Users/Diego/Dropbox/SQL/my_views/PruebaDeptos_er.sql", "\\/")

con$import(query)

x <- "SELECT * FROM public.prueba"

if(grepl("(SELECT)(?:.+)(FROM)", toupper(x))){T}

grepl("(SELECT)(?:.+)(FROM)", toupper("SELECT * FROM mortalidad.db"))
grepl("\\/", toupper("C:/Users/"))
grepl("^[A-Z]*\\.[A-Z]*$", toupper("public.prueba"))


