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


pg_sql = function(con, a){
  RPostgreSQL::dbGetQuery(con, a, stringsAsFactors = F)}
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

