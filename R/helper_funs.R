pg_col4pg = function(x){
  if(inherits(x, "data.frame")){cols <- tolower(gsub('[^A-z | ^0-9]','',colnames(x))); print(cols)}else{warning('El objeto no es de clase: data.frame')}}#DEVUELVE UN VECTOR CON LOS NOMBRES de las colmnas DEL DATA.FRAME EN MINUSCULAS Y QUITANDO TODO LO QUE NO SEA LETRA O NMERO

tradu_colnames <- function(datos){
  cols <- tolower(gsub('[^a-z | ^0-9 | ^A-Z]','',colnames(datos)))
  names(datos) <- cols;
  return(datos);
}


library(myutilities)
