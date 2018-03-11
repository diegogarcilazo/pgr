#' column name and data type from tbl.
#' @param con: Conection.
#' @param tbl: chr table name.

pg_nomcols = function(con, tbl){
  pgr::pg_sql(con,
                  glue::glue_sql(
                    "SELECT column_name, data_type
                       FROM information_schema.columns
                        WHERE table_name = {tbl}", .con = con));
  }


#' Summarise information about a tbl
#' @param con: DBI connection.
#' @param tbl: chr. tbl name.
#'
pg_tbl = function(con, tbl) {

  atts <- pgr::pg_sql(con,
               glue::glue_sql("SELECT relname, reltuples, (relpages * 8) / 2^10 mb
                                FROM pg_class
                                  WHERE relname = {tbl}", .con = con))

  cat('Table name:', tbl,'\tsize:', round(atts$mb), 'mb', '\trows:',atts$reltuples)
  cat('\n')
  pgr::pg_sql(con,
      glue::glue_sql("SELECT a.attnum, a.attname AS CAMPO, t.typname AS tipo,
                              a.attlen AS length, a.atttypmod AS lengthvar,
                               a.attnotnull AS notnull
                        FROM pg_class c, pg_attribute a, pg_type t
                         WHERE c.relname = {tbl} AND a.attnum > 0
                                  AND a.attrelid = c.oid AND a.atttypid = t.oid
                          ORDER BY a.attnum;", .con = con))
    }



#' Send query and return data.frame.
#' @param con: conection.
#' @param query: sql query.

pg_sql <- function(con, query){
    DBI::dbGetQuery(con, query, stringsAsFactors = F)
  }


#' Show schemas information from database
#' @param con: DBI connection.
#'

pg_schemas <- function(con){

dt <- "SELECT n.nspname as schema, reltuples,relpages,
c.relname as name,
  CASE c.relkind WHEN 'r' THEN 'table' WHEN 'v' THEN 'view' WHEN 'i' THEN 'index' WHEN 'S' THEN 'sequence' WHEN 's' THEN 'special' END as type,
  pg_catalog.pg_get_userbyid(c.relowner) as owner
  FROM pg_catalog.pg_class c
  LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
  WHERE c.relkind IN ('r','','v')
  AND n.nspname <> 'pg_catalog'
  AND n.nspname <> 'information_schema'
  AND n.nspname !~ '^pg_toast'
  ORDER BY 1,2;"

tbls_info <- pgr::pg_sql(con, dt)

schemas <- tbls_info %>%
               dplyr::mutate(
                  size_Tbl_MBytes = round((relpages * 8) / 2^10, 1)
                  ) %>%
  dplyr::group_by(schema) %>%
  dplyr::summarise(n_tables = sum(type == 'table'),
                             n_views = sum(type == 'view'),
                             tbls_size_MBytes = round(sum(size_Tbl_MBytes)))

cat('Database: \n')
print(con)
cat('\n')
as.data.frame(schemas);

  }

#' Table information from schema
#' @param con: DBI conection.
#' @param schema: chr. schema name.
#'

pg_tables <- function(con, schema){

  dt <- glue::glue("SELECT n.nspname as schema, reltuples,relpages,
c.relname as name,
  CASE c.relkind WHEN 'r' THEN 'table' WHEN 'v' THEN 'view' WHEN 'i' THEN 'index' WHEN 'S' THEN 'sequence' WHEN 's' THEN 'special' END as type,
  pg_catalog.pg_get_userbyid(c.relowner) as owner
  FROM pg_catalog.pg_class c
  LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
  WHERE n.nspname = '{schema}'
  ORDER BY 1,2;")

pgr::pg_sql(con, dt) %>%
  dplyr::transmute(
      type,
      name,
      owner,
      rows = reltuples,
      size_mb = round((relpages * 8) / 2^10)
    ) %>% dplyr::arrange(type, name) %>% dplyr::filter(type %in% c('table','view'))
}


pg_view_def <- function(con, view){
  pgr::pg_sql(con,
      glue::glue_sql("SELECT view_definition
                        FROM INFORMATION_SCHEMA.views
                          WHERE table_name = {view}
                            ORDER BY 1", .con = con))
}


#' Show information about, schema and table depend on arguments presents.
#' @param con: DBI connection.
#' @param schema: chr. Schema name.
#' @param table: chr. Table name.
#'

pg_show <- function(con, schema = NULL, table = NULL){

  if(is.null(schema) & is.null(table)){return(pg_schemas(con))}
  if(!is.null(schema) & is.null(table)){return(pg_tables(con, schema))}
  if(!is.null(schema) & !is.null(table)){return(pg_tbl(con, table))}
}

