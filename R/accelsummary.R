#' accelsummary
#'
#' This function generates daily activity summaries.
#' @param x a vector of activity counts
#' @param w a vector of wear flags same length of x
#' @param h threshold to define active and sedenday, default h=100
#' @return list of summaries
#' @keywords accelsummary
#' @export
#' @examples
#' accelsummary(x=xi,w=wi,h=100)

accelsummary = function(x,w,h = 100){
  library(accelerometry)
  library(flux)
  library(survival)
  library(ineq)

  x = as.numeric(na.omit(x))
  w = as.numeric(na.omit(w))

  x = as.integer(x)
  w[which(w == 0)] = NA

  y = accel.bouts(counts = x, thresh.lower = h, bout.length = 1)
  y = y * w
  mat = rle2(y)
  mat = mat[which(!is.na(mat[,1])),]
  rest = as.matrix(mat[mat[,1] == 0,])[,2]
  act = as.matrix(mat[mat[,1] == 1,])[,2]


  tlac = sum(log(as.numeric(na.omit(x*w))+1))

  sedtime = sum(rest)
  acttime = sum(act)

  lambr = length(rest)/sum(rest)
  lamba = length(act)/sum(act)

  gr = Gini(rest,corr = T)
  ga = Gini(act, corr = T)

  if(length(rest) > 0){
    fitr = survfit(Surv(rest,rep(1,length(rest)))~1)
    hr = mean(fitr$n.event/fitr$n.risk)
  }
  if(length(act) > 0){
  fita = survfit(Surv(act,rep(1,length(act)))~1)
  ha = mean(fita$n.event/fita$n.risk)
  }

  if(length(rest) == 0){
    hr = NaN
  }
  if(length(act) == 0){
    ha = NaN
  }

  result = list(
    "TLAC" = tlac,
    "sedtime" = sedtime,
    "acttime" = acttime,
    "weartime" = sedtime+acttime,
    "lambr" = lambr,
    "lamba" = lamba,
    "gr" = gr,
    "ga" = ga,
    "hr" = hr,
    "ha" = ha
  )

  return(result)

}


