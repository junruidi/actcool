#' activity_profile
#'
#' This function plots daily activity profile.
#' @param data a data frame where each row is one subject day and
#'             column 1 and 2 are id and day, and fowllowing colums
#'             are minute level activity count
#' @param wear a data frame of similar strucutre to previous one containing
#'             wear/nonwear flag
#' @param id id of the subject
#' @param day day number of the subject
#' @param h threshold determining sedentary and active, default is 100
#' @return a plot
#' @keywords activity_profile
#' @export
#' @examples
#' data(act)
#' activity_profile(data=act,wear=flag,id=21062,day=1)
#'
#'
activity_profile = function(data,  wear, id, day, h = 100){
  x = selct(data, id, day)
  w = selct(wear, id, day)
  x2 = ifelse(x >= h,1,0)

  breaks = c(0,360,720,1080,1440)
  lab = c("00:00 am","6:00 am","12:00 pm","6:00 pm","11:59 pm")

  w2 = w
  w2[which(w2 == 0)] = NA

  ind.nonwear = which(w == 0)
  ind.wear = which(w == 1)


  title = paste("Subject",id,"Day",day)
  #setwd(dir)
  #tiff(file = paste0(title,".tiff"), width = 12, height = 5,units="in",res=300)
  par(oma = c(2,2,2,2))
  par(mar = c(1,1,1,1))
  acti = x*w2
  cls = x2
  plot(acti,type = "h",col=c("black","black")[as.factor(cls)],yaxt="n",xaxt="n",xlab="",ylab="",
       cex.lab= 1, cex.main= 1, cex.sub= 2,axes = F,main = title)
  abline(v = ind.wear, col = rgb(0, 191, 255, 30, maxColorValue = 255))
  abline(v = ind.nonwear, col = rgb(200, 34, 34, 10, maxColorValue = 255))
  axis(side=1,at=breaks,labels = lab,cex.axis=1.3)
  axis(side=2)
  abline(h = h,lty=3)

  #dev.off()
}
