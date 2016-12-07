#' accelsummary_data
#'
#' This function generates daily activity summaries.
#' @param data a data frame where each row is one subject day and
#'             column 1 and 2 are id and day, and fowllowing colums
#'             are minute level activity count
#' @param wear a data frame of similar strucutre to previous one containing
#'             wear/nonwear flag
#' @param h threshold to define active and sedenday, default h=100
#' @return data frame of subject-day level summaries
#' @keywords accelsummary_data
#' @export
#' @examples
#' data(act)
#' accelsummary_data(data=act,wear=flag,h=100)
#'
#'
accelsummary_data = function(data,wear,h = 100){

  out =as.data.frame(matrix(NA, ncol = 12, nrow = nrow(data)))
  names(out) = c("ID","Day","TLAC","sedtime","acttime", "weartime",
                  "lambr","lamba","gr","ga","hr","ha")
  out[,1:2] = data[,1:2]

  dmat = as.matrix(data[,-c(1:2)])
  wmat = as.matrix(wear[,-c(1:2)])
  newd = cbind(dmat,wmat)

  result = apply(newd,1,function(x){
    accelsummary(x[1:1440],x[1441:2880],h=h)
  })

  aa = unlist(result)
  out[,3] = aa[seq(1,length(aa),10)]
  out[,4] = aa[seq(2,length(aa),10)]
  out[,5] = aa[seq(3,length(aa),10)]
  out[,6] = aa[seq(4,length(aa),10)]
  out[,7] = aa[seq(5,length(aa),10)]
  out[,8] = aa[seq(6,length(aa),10)]
  out[,9] = aa[seq(7,length(aa),10)]
  out[,10] = aa[seq(8,length(aa),10)]
  out[,11] = aa[seq(9,length(aa),10)]
  out[,12] = aa[seq(10,length(aa),10)]

  return(out)


}
