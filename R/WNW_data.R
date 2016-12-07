#' WNW_data
#'
#' This function generates wear/nonwera flags
#' @param data dataframe containing subject-day level activity count
#' @param window window interval scanning, default is 90
#' @param tolerance number of maximum allowance in the window, default is 2
#' @param cpmmax tolerated maximum activity counts, default is 99
#' @return a vector contating same length as x and its elements are either 1 or 0
#' @keywords WNW_data
#' @export
#' @examples
#' data(act)
#' WNW_data(data = act, window = 90, tolerance = 2, cpmmax = 99)
#'
WNW_data = function(data, window = 90, tolerance = 2, cpmmax = 99){
  activity_data = as.matrix(data[,3:1442])
  WMX = matrix(NA,nrow = nrow(activity_data), ncol = ncol(activity_data))

  pb = txtProgressBar(min = 1, max = nrow(activity_data), style = 3)

  for (i in 1 : nrow(activity_data)){

    activity_data_i = activity_data[i,]
    wearMark = WNW(activity_data_i,window = 90, tolerance = 2, cpmmax = 99)
    WMX[i,] = wearMark
    setTxtProgressBar(pb, i)
  }

  out = cbind(data[,1:2], WMX)
  names(out) = names(data)

  return(out)

}
