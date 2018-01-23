#' Export query to CSV with delimiter tabular and header.
#' @param con: connection.
#' @param query: (chr) SQL query.
#' @param dir: (chr) directory. Default(file.choose()).
#' @param encoding: (chr) SET client_encoding to. Default 'WIN1252'.
#' @param delimiter: (chr) Set delimiter. Default tabular.


pg_export = function(con,query,dir = file.choose(new = T), encoding = 'WIN1252', delimiter = '\t'){
  DBI::dbSendQuery(con,
                           paste0("SET client_encoding TO '",
                                  encoding,
                                  "'; COPY (",
                                  query,
                                  ") TO '",
                                  dir ,
                                  "' WITH DELIMITER E'",
                                  delimiter,
                                  "'CSV HEADER;"))}


pg_read_ <- function(con, schema, table_name){
  DBI::dbReadTable(con, c(schema,table_name))
}

pg_read <- function(con, schema, table_name){
  schema = deparse(substitute(schema));
  table_name = deparse(substitute(table_name));
  pg_read_(con, schema, table_name)
}


#' Connect to several tables from schema of db using dplyr
#' @param db chr database name
#' @param schema chr schema name. Default 'public'.
#' @param tbls chr or vector of chrs with table names. Default all tables.
#' @param host chr. Defaulta 'localhost'.
#'

pg_readtbls <- function(db = NULL, schema = 'public', tbls = NULL, host = 'localhost')
  {

  stopifnot(!is.null(db));

  con <- pgr::pg_con_(db, host = host);

  all_tbls <- DBI::dbGetQuery(con,"SELECT table_schema, table_name, table_type
FROM information_schema.tables
                WHERE NOT table_schema IN ('pg_catalog','information_schema')")

  if(is.null(tbls)){tbls <- subset(all_tbls, table_schema==schema);} else {

    tbls <- subset(all_tbls, table_schema==schema & table_name%in%tbls);}

  listOfTbls <- purrr::map2(tbls$table_schema, tbls$table_name,
                            ~ dplyr::tbl(con, dbplyr::in_schema(.x, .y)));

  names(listOfTbls) <- paste(tbls$table_schema, tbls$table_name, sep = '.')

  return(listOfTbls)
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
  return(ifelse(valor == T, 'Writed on postgresql: OK.','Writed on postgresql: Error'))
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
pg_importdbf <- function(file = file.choose(), con, schema = 'public', table_name)
{
  pg_importdbf_(file, con, deparse(substitute(schema)),
                deparse(substitute(table_name)))}



#' Save data.frame into Postgresql
#' @param con: pg connection.
#' @param schema: schema name.
#' @param table_name: name that will have table.
#' @param df: data.frame to save.
#' @param overwrite: Overwrite table.
#' @param append: Append table
#' @param temporary: Create temporary

pg_save <- function(df, con, schema, table_name, overwrite = FALSE, append = FALSE, temporary = FALSE){
  schema = deparse(substitute(schema));
  table_name = deparse(substitute(table_name));
  pg_save_(df, con, schema, table_name, overwrite, append, temporary);
}

pg_save_ <- function(df, con, schema = 'public', table_name, overwrite = FALSE, append = FALSE, temporary = FALSE){
  DBI::dbWriteTable(con,
                            c(schema, table_name),
                            value = df ,row.names = F, overwrite = overwrite, append = append, temporary = temporary);
}

