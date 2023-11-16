devtools::document()
usethis::use_testthat()
devtools::test()
devtools::use_package('R6')
usethis::use_test()
con = pgr::pg_con(mdb1252, driver = Postgres)

con <- PgCon$new('mdb1252')

con$export(tibble::tibble(a = 1:4), table_name = "prueba")

con$import('codigos.deptos_er')

con$describeTable("mort80")

con$addValue_client_encoding <- "UTF8"

con$client_encoding

pgr::pg_tbl(con$pg_con, "deptos_er")

rm(con)

con$disconnect()

DBI::dbListConnections(drv = RPostgreSQL::PostgreSQL())

DBI::dbDisconnect()

gc()

