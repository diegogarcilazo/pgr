PgCon <- R6::R6Class("PgCon",
                     public = list(
                       name = NULL,
                       user = NULL,
                       host = NULL,
                       port = NULL,
                       driver = NULL,
                       pg_con = NULL,
                       client_encoding = "UTF8",
                       initialize = function(name, user = 'postgres', host = 'localhost', port = '5432'){
                         self$name <- name
                         self$user <- user
                         self$host <- host
                         self$port <- port
                         self$driver <- RPostgreSQL::PostgreSQL()
                         self$pg_con <- DBI::dbConnect(drv = self$driver,
                                                       dbname = self$name,
                                                       user = self$user,
                                                       host = self$host,
                                                       port = self$port,
                                                       password = rstudioapi::askForPassword(paste('Password for user', self$user)))

                         DBI::dbSendQuery(self$pg_con, glue::glue("SET client_encoding TO {self$client_encoding};"))

                         print(self)

                       },
                       print = function(...){
                         print(
                           glue::glue("<{self$name}>
                                          user: {self$user}
                                          host: {self$host}
                                          port: {self$port}
                                          client encoding: {self$encoding}"))
                       },
                       import = function(query){
                         if(grepl('(SELECT|INSERT|UPDATE)(?:.+)(FROM)', toupper(query))){
                           tibble::as_tibble(DBI::dbGetQuery(self$pg_con, query, stringsAsFactors = F))
                         }else{
                           tibble::as_tibble(DBI::dbReadTable(self$pg_con, strsplit(query,"\\.")[[1]]))}
                       },

                       export = function(df, schema = 'public', table_name, overwrite = FALSE, append = FALSE, temporary = FALSE){
                         DBI::dbWriteTable(conn = self$pg_con, name = c(schema, table_name),
                                           value = df ,row.names = F, overwrite = overwrite, append = append, temporary = temporary);
                       }


                     )
)

