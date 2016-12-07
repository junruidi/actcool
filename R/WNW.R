#' WNW
#'
#' This function generates wear/nonwera flags
#' @param x a vector of activity counts
#' @param window window interval scanning, default is 90
#' @param tolerance number of maximum allowance in the window, default is 2
#' @param cpmmax tolerated maximum activity counts, default is 99
#' @return a vector contating same length as x and its elements are either 1 or 0
#' @keywords WNW
#' @export
#' @examples
#' data(onedaydata)
#' WNW(x = xi,window = 90, tolerance = 2, cpmmax = 99)
#'

WNW = function(x,window = 90, tolerance = 2, cpmmax = 99){
  library(accelerometry)
  w = accel.weartime(x,window = window,
                 tol = tolerance, tol.upper = cpmmax)
  return(w)
}


