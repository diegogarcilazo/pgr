#' R6 Class PyCon postgresql connection.

PgCon <- R6::R6Class("PgCon",
                     public = list(
                       name = NULL,
                       user = NULL,
                       host = NULL,
                       port = NULL,
                       driver = NULL,
                       pg_con = NULL,
                       client_encoding = "UTF8",
                       initialize = function(name, user = 'postgres', host = 'localhost', port = '5432', driver = RPostgreSQL::PostgreSQL()){
                         self$name <- name
                         self$user <- user
                         self$host <- host
                         self$port <- port
                         self$driver <- driver
                         self$pg_con <- DBI::dbConnect(drv = self$driver,
                                                       dbname = self$name,
                                                       user = self$user,
                                                       host = self$host,
                                                       port = self$port,
                                                       password = rstudioapi::askForPassword(paste('Password for user', self$user)))

                         DBI::dbSendQuery(self$pg_con, glue::glue("SET client_encoding TO {self$client_encoding};"))
                         cat(print(self))
                       },
                       print = function(){
                         glue::glue("<{self$name}>
                                          user: {self$user}
                                          host: {self$host}
                                          port: {self$port}
                                          client encoding: {self$client_encoding}
                                    ")
                       },

                       import = function(query){
                         start <- Sys.time()
                         if(grepl('(SELECT|INSERT|UPDATE)(?:.+)(FROM)', toupper(query))){
                           .tibble <- tibble::as_tibble(DBI::dbGetQuery(self$pg_con, query, stringsAsFactors = F))
                         }else{
                           .tibble <- tibble::as_tibble(DBI::dbReadTable(self$pg_con, strsplit(query,"\\.")[[1]]))}
                         end <- Sys.time()
                         cat(paste('\nImport time:', round(end-start,2), "sec\n"))
                         return(.tibble)
                       },

                       export = function(df, schema = 'public', table_name, overwrite = FALSE, append = FALSE, temporary = FALSE){
                         DBI::dbWriteTable(conn = self$pg_con, name = c(schema, table_name),
                                           value = df ,row.names = F, overwrite = overwrite, append = append, temporary = temporary);
                       },

                       disconnect = function() {DBI::dbDisconnect(self$pg_con)}

                     )
)
