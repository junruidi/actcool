#' actsum_avebyday
#'
#' This function average summaries across days for each subject
#' @param act_sum dataframe containing subject-day summaries
#' @return a dataframe ontaining subjects'summaries
#' @keywords actsum_avebyday
#' @export
#' @examples
#' data(act)
#' actsum_avebyday(data = act_sum)
#'
#'
#'
actsum_avebyday = function(act_sum){
  library(dplyr)
  g = group_by(act_sum,ID)
  ave_actsum = summarise(g,TLAC = mean(TLAC,na.rm=T),sedtime = mean(sedtime,na.rm = T),
                         acttime=mean(acttime,na.rm = T),weartime = mean(weartime,na.rm = T),
                         lambr = mean(lambr,na.rm = T),lambra = mean(lamba,na.rm = T),
                         gr = mean(gr,na.rm = T),ga= mean(ga,na.rm = T),
                         hr = mean(hr,na.rm = T),ha= mean(ha,na.rm = T))
  return(ave_actsum)
}
