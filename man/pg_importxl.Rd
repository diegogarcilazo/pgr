% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_export_funs.R
\name{pg_importxl}
\alias{pg_importxl}
\title{Read tabular data in excel file (xls or xlsx) and write it in PostgreSQL.}
\usage{
pg_importxl(
  con,
  file = file.choose(),
  sheet = "Hoja 1",
  schema,
  table,
  col_types = NULL
)
}
\arguments{
\item{con:}{Pg connection.}

\item{file:}{path excel file.}

\item{sheet:}{excel sheet where is the table.}

\item{schema:}{name of schema to write.}

\item{table:}{table name.}

\item{col_types:}{chr Either NULL to guess from the spreadsheet or a character vector containing "blank", "numeric", "date" or "text".}
}
\description{
Read tabular data in excel file (xls or xlsx) and write it in PostgreSQL.
}
