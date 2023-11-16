devtools::document()
usethis::use_testthat()
devtools::test()
devtools::use_package('R6')
usethis::use_test()
con = pgr::pg_con(mdb1252, driver = Postgres)

con <- PgCon$new('mdb1252')

con$export(tibble::tibble(a = 1:4), table_name = "prueba")

con$import('codigos.deptos_er')

con$addValue_client_encoding <- "UTF8"


tbl_loc <- purrr::set_names(
  x = strsplit('codigos.deptos_er', "\\.")[[1]],
  nm = c("schema", "table"))

a <- paste0(tbl_loc, collapse = ".")

DBI::dbReadTable(con$pg_con, RPostgres::Id(tbl_loc))



rm(con)

con$disconnect()

DBI::dbListConnections(drv = RPostgreSQL::PostgreSQL())

DBI::dbDisconnect()

gc()

dbplyr::copy_to(con,
                   tibble::tibble(a = 1:5),
                   dbplyr::in_schema("public", "sales"))
