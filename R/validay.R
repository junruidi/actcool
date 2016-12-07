#' validay
#'
#' This function keeps subject-days that are valid
#' @param data a data frame where each row is one subject day and
#'             column 1 and 2 are id and day, and fowllowing colums
#'             are minute level activity count
#' @param wear a data frame of similar strucutre to previous one containing
#'             wear/nonwear flag
#' @param hour daily wearing time, default hour=10
#' @return data frame of subject-day data
#' @keywords valid
#' @export
#' @examples
#' data(act)
#' validay(data=act,wear=flag,hour=10)
#'
#'

validay = function(data,wear,hour = 10){
  nonwear = rowSums(flag[, 3:1442], na.rm = TRUE) < hour*60
  valid_act = act[!(nonwear),]
  valid_flag = wear[!(nonwear),]

  result = list("valid_act" = valid_act,"valid_flag" = valid_flag)
  return(result)
}


