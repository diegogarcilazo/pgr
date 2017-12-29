pg_con_ = function(dbname = NULL, user = 'postgres', host = 'localhost'){
  if(dbname == 'NULL') stop('No database name');
  password = rstudioapi::askForPassword(paste('Password for user', user));
  DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                         dbname = dbname,
                         user = user,
                         host = host,
                         password = password)
}


#' Connection to postgresql via RPostgreSQL. The password is entered by .rs.askForPassword().
#' @param dbname: database name.
#' @param user: user name. Default(postgres).
#' @param host: host. Default(localhost).

pg_con = function(dbname = NULL, user = postgres, host = localhost){
  dbname = deparse(substitute(dbname));
  user = deparse(substitute(postgres));
  host = deparse(substitute(localhost));
  pg_con_(dbname = dbname,
          user = user,
          host = host)
}

#' Disconnect all connections available
#'
pg_disconnect_all <- function()
{
  all_cons <- DBI::dbListConnections(RPostgreSQL::PostgreSQL())
  walk(all_cons, DBI::dbDisconnect)

}
