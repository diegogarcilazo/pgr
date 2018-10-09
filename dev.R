devtools::document()
devtools::use_testthat()
devtools::use_package('R6')

con = pgr::pg_con(mdb1252, driver = Postgres)

con <- PgCon$new('mdb1252')

con$import('codigos.deptos_er')

con$disconnect()
