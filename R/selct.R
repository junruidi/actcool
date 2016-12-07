#' selct
#'
#' This function selects a daily activity record given ID and day
#' @param data dataframe containing subject-day level activity count
#' @param id id of the subject
#' @param day day number of the subject
#' @return a vector containing daily activity record
#' @keywords selct
#' @export
#' @examples
#' data(act)
#' selct(data = act ,id = 21034,day = 1)
#'
#'
#


selct = function(data,id,day){
  x = as.vector(t(data[which(data$ID == id & data$Day == day),-c(1:2)]))
  return(x)
}



