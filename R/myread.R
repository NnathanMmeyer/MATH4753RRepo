#' Allows R to read in a CSV file in the current directory
#'
#' Insert the CSV file as a function argument, make sure CSV file is in the current working directory
#'
#' @param csv CSV file
#'
#' @return data of CSV file
#'
#'
#' @export

myread=function(csv){
  fl=paste(paste(getwd(), "/", sep = ""),csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}
