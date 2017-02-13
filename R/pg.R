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

#' Export query to CSV with delimiter tabular and header.
#' @param con: connection.
#' @param query: (chr) SQL query.
#' @param dir: (chr) directory. Default(file.choose()).
#' @param encoding: (chr) SET client_encoding to. Default 'WIN1252'.
#' @param delimiter: (chr) Set delimiter. Default tabular.

pg_export = function(con,query,dir = file.choose(new = T), encoding = 'WIN1252', delimiter = '\t'){
  RPostgreSQL::dbSendQuery(con,
                           paste0("SET client_encoding TO '", 
                                  encoding,
                                  "'; COPY (",
                                  query,
                                  ") TO '",
                                  dir ,
                                  "' WITH DELIMITER E'",
                                  delimiter,
                                  "'CSV HEADER;"))}

pg_col4pg = function(x){
  if(inherits(x, "data.frame")){cols <- tolower(gsub('[^A-z | ^0-9]','',colnames(x))); print(cols)}else{warning('El objeto no es de clase: data.frame')}}#DEVUELVE UN VECTOR CON LOS NOMBRES de las colmnas DEL DATA.FRAME EN MINUSCULAS Y QUITANDO TODO LO QUE NO SEA LETRA O NMERO

pg_nomcols = function(con, a){
  nomcols<-pg_sql(con, paste0("SELECT column_name, data_type FROM information_schema.columns WHERE table_name ='",a,"'"));
  return(nomcols);
  }#NOMBRE DE LAS COLUMNAS Y TIPO DE DATO a(character): Nombre de la tabla

pg_tbl_ = function(con, a) {
  tbl <- pg_sql(con, paste0("SELECT a.attnum, a.attname AS CAMPO, t.typname AS tipo, a.attlen AS length, a.atttypmod AS lengthvar, a.attnotnull AS notnull FROM pg_class c, pg_attribute a, pg_type t WHERE c.relname = '", a, "' and a.attnum > 0 and a.attrelid = c.oid and a.atttypid = t.oid ORDER BY a.attnum;"));
  return(tbl)}#DESCRIBE LA TABLA a(character): nombre de la tabla

pg_tbl <- function(con, a){
  pg_tbl_(con, deparse(substitute(a)));
}


pg_read_ <- function(con, schema, table_name){
RPostgreSQL::dbReadTable(con, c(schema,table_name))
  }

pg_read <- function(con, schema, table_name){
  schema = deparse(substitute(schema));
  table_name = deparse(substitute(table_name));
  pg_read_(con, schema, table_name)
  }

pg_sql = function(con, a){
  RPostgreSQL::dbGetQuery(con, a, stringsAsFactors = F)}

tradu_colnames <- function(datos){
  cols <- tolower(gsub('[^a-z | ^0-9 | ^A-Z]','',colnames(datos)))
  names(datos) <- cols;
  return(datos);
}


pg_importxl_ <- function(
  con,
  file = file.choose(),
  sheet = 'Hoja 1',
  schema,
  table,
  col_types = NULL){
  datos <- readxl::read_excel(file, sheet = sheet, col_types = col_types);
  datos <- as.data.frame(datos);
  new_colnames <- pg_col4pg(datos);
  colnames(datos) <- new_colnames;
  valor <- pg_save_(datos, con, schema = schema, table_name = table);
  return(ifelse(valor == T, 'Loaded. OK.','Error'))
  }

#' Read tabular data in excel file (xls or xlsx) and write it in PostgreSQL.
#' @param con: Pg connection.
#' @param file: path excel file.
#' @param sheet: excel sheet where is the table.
#' @param schema: name of schema to write.
#' @param table: table name. 
#' @param col_types: chr Either NULL to guess from the spreadsheet or a character vector containing "blank", "numeric", "date" or "text".

pg_importxl <- function(
  con,
  file = file.choose(),
  sheet = 'Hoja 1',
  schema,
  table,
  col_types = NULL){
  pg_importxl_(con, file, sheet, deparse(substitute(schema)), deparse(substitute(table)), col_types = col_types)
}


pg_importdbf_ <- function(file = file.choose(), con, schema = 'public', table_name){
  data <- foreign::read.dbf(file, as.is = T)
  new_colnames <- pg_col4pg(data);
  colnames(data) <- new_colnames;
  pg_answer <- pg_save_(data, con, schema, table_name);
  if(pg_answer == T){str(tradu_colnames(data))}else{'Import Error'}}


#' Read tabular data in dbf file and write it in PostgreSQL.
#' @param file: path excel file.
#' @param con: Pg connection.
#' @param schema: name of schema to write.
#' @param table_name: table name.
#' 
pg_importdbf <- function(file = file.choose(), con, schema = 'public', table_name){
  pg_importdbf_(file, con, deparse(substitute(schema)), deparse(substitute(table_name)))}



pg_schemas <- function(con){
  schemas <- pg_sql(con,
'select schemaname, count(*) n_tables  from pg_tables group by 1 order by 1');
  return(invisible(schemas));
}

pg_tables_ <- function(con, schema){
  tables <- pg_sql(con, paste0("select tablename  from pg_tables where schemaname = '",schema,"' order by 1"));
  return(tables);
}

pg_tables <- function(con, schema){
  pg_tables_(con, deparse(substitute(schema)));
}

pg_views_ <- function(con, schema){
views <-pg_sql(con, paste0("select table_name from INFORMATION_SCHEMA.views WHERE table_schema = '", schema, "' ORDER BY 1"));
return(views);
}

pg_views <- function(con, schema){
  pg_views_(con, deparse(substitute(schema)))
}

pg_view_def <- function(con, view){
  def <-pg_sql(con, paste0("select view_definition from INFORMATION_SCHEMA.views WHERE table_name = '", view, "' ORDER BY 1"));
  return(def);
}


#' Save data.frame into Postgresql
#' @param con: pg connection.
#' @param schema: schema name.
#' @param table_name: name that will have table.
#' @param df: data.frame to save.

pg_save <- function(df, con, schema, table_name, overwrite = FALSE, append = FALSE){
  schema = deparse(substitute(schema));
  table_name = deparse(substitute(table_name));
  pg_save_(df, con, schema, table_name, overwrite, append);
}

pg_save_ <- function(df, con, schema = 'public', table_name, overwrite = FALSE, append = FALSE){
  RPostgreSQL::dbWriteTable(con, 
                            c(schema, table_name), 
                            value = df ,row.names = F, overwrite = overwrite, append = append);
}

pg_schema_old <-function(con, schema, first = NULL, second = NULL, to_clip = F){
  conn <- con;
  tables <-pg_tables_(con,schema);
  views <- pg_views_(con,schema);
  l <- list(tables = tables, views = views);
  if(!is.null(first) & !is.null(second) & to_clip == T){
    selectedTable <-l[[first]][second,];
    writeClipboard(selectedTable);
  }
  if(!is.null(first) & !is.null(second) & to_clip == F){
    selectedTable <-l[[first]][second,];
    return(pg_table_(con, schema, selectedTable));
  }
  if(is.null(first) & is.null(second)){
    cat(
      paste(
        "The schema",
        schema,
        "have:\n Tables:\n"));
    print(tables);
    cat(
      paste(
        "Views:\n"));
    print(views);
    return(invisible(list(selectedTable = selectedTable, connection = conn, schema = schema)));
  }
}

pg_schema_ <-function(con, schema, first = NULL, second = NULL){
  conn <- con;
  tables <-pg_tables_(con,schema);
  views <- pg_views_(con,schema);
  l <- list(tables = tables, views = views);
  if(!is.null(first) & !is.null(second)){
    selectedTable <-l[[first]][second,];
    list_1 <- list(selectedTable = selectedTable, connection = conn, schema = schema);
    class(list_1) <- 'schema_list';
    message(paste('Selected table:',selectedTable,'from',schema))
    return(invisible(list_1))}
  if(is.null(first) | is.null(second)){
    cat(
      paste(
        "The schema",
        schema,
        "have:\n Tables:\n"));
    print(tables);
    cat(
      paste(
        "Views:\n"));
    print(views);
  }
}

pg_action <- function(x, ...) UseMethod('pg_action',x)

pg_action_ <- function(pg_schema_list, action = 'view'){
  con <- pg_schema_list[['connection']];
  schema <- pg_schema_list[['schema']];
  selectedTable <- pg_schema_list[['selectedTable']];
  
  switch(action,
         'view' = {
           tbl<-pg_tbl_(con, selectedTable)
           return(tbl);
         },
         'clip' = {
           message('Schema and table name copied to clipboard');
           writeClipboard(paste0(schema,'.',selectedTable));  
         },
         'read' = {
           return(pg_read_(con, schema, selectedTable));  
         },
         'first' = {
    return(pg_sql(con, paste0("SELECT * FROM ",schema, ".",selectedTable, " LIMIT 10")))  
  })
}
#'Summary of schema. Additionaly if use first and second arguments read table or view.
#'
#'@param con: pg connection.
#'@param schema: schema.
#'@param first: int 1 = Tables. 2 = Views.
#'@param second: int with table or view position.
#'@param to_clip: logical if TRUE copies to clipboard 
#' 
pg_schema <-function(con, schema, first = NULL, second = NULL){
  pg_schema_(con, deparse(substitute(schema)), first, second);
}

pg_action.schema_list <- function(pg_schema_list, action = view){
  pg_action_(pg_schema_list, deparse(substitute(action)))
}


pg_action.pg_table <- function(pg_table, action = view){
  pg_action2_(pg_table, action = deparse(substitute(view)))
}

pg_table <- function(con, schema, table){
 schema = deparse(substitute(schema))
 table = deparse(substitute(table))
 
 table <- list(con, schema, table);
 class(table) <- 'pg_table'
  return(
  invisible(table)
)
}

pg_action2_ <- function(pg_table, action = 'view'){

  con <- pg_table[[1]];
  schema <- pg_table[[2]];
  selectedTable <- pg_table[[3]];
  switch(action,
         'view' = {
           tbl<-pg_tbl_(con, selectedTable)
           return(tbl);
         },
         'clip' = {
           message('Schema and table name copied to clipboard');
           writeClipboard(paste0(schema,'.',selectedTable));  
         },
         'read' = {
           return(pg_read_(con, schema, selectedTable));  
         },
         'first' = {
           return(pg_sql(con, paste0("SELECT * FROM ",schema, ".",selectedTable, " LIMIT 10")))  
         })
}