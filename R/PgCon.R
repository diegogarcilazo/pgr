#' R6 Class PyCon postgresql connection.

PgCon <- R6::R6Class("PgCon",


                     active = list(

                       addValue_client_encoding = function(value){

                         if(missing(value)){
                           private$set_client_encoding()}
                         else{
                             self$client_encoding <- value
                             private$set_client_encoding()
                           }
                       }
                     ),

                     public = list(
                       name = NULL,
                       user = NULL,
                       host = NULL,
                       port = NULL,
                       driver = NULL,
                       pg_con = NULL,
                       client_encoding = NULL,

                       initialize = function(name, user = 'postgres', host = 'localhost', port = '5432',
                                             driver = RPostgreSQL::PostgreSQL(), client_encoding = "UTF8"){
                         self$name <- name
                         self$user <- user
                         self$host <- host
                         self$port <- port
                         self$driver <- driver
                         self$client_encoding <- client_encoding
                         self$pg_con <- DBI::dbConnect(drv = self$driver,
                                                       dbname = self$name,
                                                       user = self$user,
                                                       host = self$host,
                                                       port = self$port,
                                                       password = rstudioapi::askForPassword(paste('Password for user', self$user)))

                         private$set_client_encoding()
                         print(self$print())
                       },

                       finalize = function() {

                         message("Cleaning up connection")
                         self$disconnect()

                         },

                       print = function(){
                         invisible(

                           glue::glue("<{self$name}>
                                          user: {self$user}
                                          host: {self$host}
                                          port: {self$port}
                                          client encoding: {self$client_encoding}
                                    ")

                         )


                       },


                       import = function(sql_obj){
                         start <- Sys.time()
                         if(grepl('(SELECT)(?:.+)(FROM)', toupper(sql_obj))){
                           obj <- "SQL query:"
                           .tibble <- tibble::as_tibble(DBI::dbGetQuery(self$pg_con, sql_obj, stringsAsFactors = F))
                         }else{
                           obj <- "Table:"
                           .tibble <- tibble::as_tibble(DBI::dbReadTable(self$pg_con, strsplit(sql_obj,"\\.")[[1]]))}
                         end <- Sys.time()
                         cat(glue::glue('Import {obj} {sql_obj} \ntime: {round(end-start,2)} sec\n\n'))
                         return(.tibble)
                       },

                       export = function(df, schema = 'public', table_name, overwrite = FALSE, append = FALSE, temporary = FALSE){
                         DBI::dbWriteTable(conn = self$pg_con, name = c(schema, table_name),
                                           value = df ,row.names = F, overwrite = overwrite, append = append, temporary = temporary);
                       },

                       disconnect = function() {DBI::dbDisconnect(self$pg_con)}

                     ),

                    private = list(
                      set_client_encoding = function(){
                        DBI::dbSendQuery(self$pg_con, glue::glue("SET client_encoding TO {self$client_encoding};"))
                      }
                    )
)
