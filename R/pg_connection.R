pg_con_ = function(dbname = NULL, user = 'postgres', host = 'localhost', driver = 'Postgres'){
  if(dbname == 'NULL') stop('No database name');
  switch (driver,
    'Postgres' = {driver <- RPostgres::Postgres()},
    'PostgreSQL' = {driver <- RPostgreSQL::PostgreSQL()}
  );
  password = rstudioapi::askForPassword(paste('Password for user', user));
  DBI::dbConnect(drv = driver,
                         dbname = dbname,
                         user = user,
                         host = host,
                         password = password)
}


#' Connection to postgresql via RPostgreSQL. The password is entered by .rs.askForPassword().
#' @param dbname: database name.
#' @param user: user name. Default(postgres).
#' @param host: host. Default(localhost).
#' @param driver: switch from Postgres to PostgreSQL

pg_con = function(dbname = NULL, user = postgres, host = localhost, driver = Postgres){
  dbname = deparse(substitute(dbname));
  user = deparse(substitute(postgres));
  host = deparse(substitute(localhost));
  driver = deparse(substitute(driver));
  pg_con_(dbname = dbname,
          user = user,
          host = host,
          driver = driver)
}

#' Disconnect all connections available
#'
pg_disconnect_all <- function()
{
  all_cons <- DBI::dbListConnections(RPostgre::Postgre())
  walk(all_cons, DBI::dbDisconnect)

}
