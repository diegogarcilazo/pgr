#' @importFrom magrittr %>%
pg_con_ = function(dbname = NULL, user = 'postgres', host = 'localhost'){
  if(dbname == 'NULL') stop('No database name');
  password = .rs.askForPassword(paste('Password for user', user));
  RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
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

pg_src_ <- function(
  dbname = NULL, host = 'localhost', port = 5432, user = 'postgres') {
  password = .rs.askForPassword(paste('Password for user', user));
  if(dbname == 'NULL') stop('No database name');
  dplyr::src_postgres(dbname = dbname,
                      host = host,
                      port = port,
                      user = user,
                      password = password)
}

#' Connection to postgresql via dplyr. The password is entered by .rs.askForPassword().
#' @param dbname: database name.
#' @param user: user name. Default(postgres).
#' @param host: host. Default(localhost).

pg_src <- function(
  dbname = NULL, host = localhost, port = 5432, user = postgres) {
  dbname = deparse(substitute(dbname));
  host = deparse(substitute(host));
  user = deparse(substitute(user));
  pg_src_(dbname = dbname,
          host = host,
          port = port,
          user = user)
}


pg_dplyr_ <- function(pg_src, schema = 'public', table){
  query <- paste('select * from', paste(schema, table, sep = '.'));
  query_4dplyr <- dplyr::sql(query);
  return(dplyr::tbl(query_4dplyr, src = pg_src));
}

pg_dplyr <- function(pg_src, schema, table){
  schema = deparse(substitute(schema));
  table = deparse(substitute(table));
  return(pg_dplyr_(pg_src, schema, table));
}


pg_dplyr_tbl_ <- function(
  dbname = NULL,
  schema = 'public', table = NULL,
  host = 'localhost',
  port = 5432, user = 'postgres',
  password = .rs.askForPassword(paste('Password for user', user))){
  if(dbname == 'NULL') stop('No database name');
  if(table == 'NULL') stop('No table name');
  conexion <- pg_src_(dbname = dbname, host = host, port = port, user = user, password = password);
  return(pg_dplyr_(conexion, schema, table));
}

#' Read table from postgresql via dplyr. The password is entered by .rs.askForPassword().
#' @param dbname: database name.
#' @param user: user name. Default(postgres).
#' @param host: host. Default(localhost).
#' @param port: port. Default(5432).
#' @param password: password. Default(.rs.askForPassword()).
#' @param schema: schema. Default(public).
#' @param table: table. Default(NULL)

pg_dplyr_tbl <- function(
  dbname = NULL,
  schema = public, table = NULL,
  host = localhost,
  port = 5432, user = postgres,
  password = .rs.askForPassword(paste('Password for user', user))){
  dbname = deparse(substitute(dbname));
  schema = deparse(substitute(schema));
  table = deparse(substitute(table));
  host = deparse(substitute(host));
  user = deparse(substitute(user));
  if(dbname == 'NULL') stop('No database name');
  if(table == 'NULL') stop('No table name');
  return(pg_dplyr_tbl_(dbname = dbname, schema = schema, table = table, host = host, port = port, user = user, password = password));
}
