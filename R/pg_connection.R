pg_con_ = function(dbname = NULL, user = 'postgres', host = 'localhost', port = '5432', driver = 'Postgres'){

  if(dbname == 'NULL') stop('No database name');

  switch (driver,
    'Postgres' = {driver <- RPostgres::Postgres()},
    'PostgreSQL' = {driver <- RPostgreSQL::PostgreSQL()}
  );

  password = rstudioapi::askForPassword(paste('Password for user', user));

  con <- DBI::dbConnect(drv = driver,
                         dbname = dbname,
                         user = user,
                         host = host,
                         port = port,
                         password = password)
}


#' Connection to postgresql via RPostgreSQL. The password is entered by .rs.askForPassword().
#' @param dbname: database name.
#' @param user: user name. Default(postgres).
#' @param host: host. Default(localhost).
#' @param driver: switch from Postgres to PostgreSQL

pg_con = function(dbname = NULL, user = postgres, host = 'localhost', port = '5432', driver = Postgres){
  dbname = deparse(substitute(dbname));
  user = deparse(substitute(postgres));
  driver = deparse(substitute(driver));
  pg_con_(dbname = dbname,
          user = user,
          host = host,
          port = port,
          driver = driver)
}


pg_con(fibrosisquistica, host = '192.168.0.204')

#' Disconnect all connections available
#'
pg_disconnect_all <- function()
{
  all_cons <- DBI::dbListConnections(RPostgre::Postgre())
  walk(all_cons, DBI::dbDisconnect)

}
