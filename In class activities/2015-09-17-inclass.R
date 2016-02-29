# Jingjing Yang
# ICA

#Task1
library(mosaicData)
nbins <- 10

#Task2
min <- min(Galton$height)
max <- max(Galton$height)
evenly_spaced <- seq(min, max, by = (max-min)/(nbins))

#Task3
bin_counts<- table(cut(Galton$height, breaks = nbins))

#Task4
hist_basics <- data.frame(xL = evenly_spaced[-11], xR = evenly_spaced[-1], count = as.numeric(bin_counts))

#Task5
make_one_bar <- function(point){
  xLeft <- point$xL
  xRight <- point$xR
  height <- point$count
  res <- data.frame(x=c(xLeft, xLeft, xRight, xRight, NA), y = c(0,height,height,0,NA))
  res
}

one_to_five <- function(hist_data){
  bars <- NULL
  for (k in 1:nrow(hist_data)){
    new_bar <- make_one_bar(hist_data[k, ])
    bars <- rbind(bars, new_bar)
  }
  bars
}

My_bars <- data.frame(one_to_five(hist_basics))

plot(My_bars, type = "n")
lines(My_bars)
polygon(My_bars)



# Writing functions 
# draw_an_empty_frame <- function(width, height, bg){
#   plot(1,type="n",xlim = c(0,width),ylim = c(0,height))
# }
# 
# plot(1, xlim = c(0,50), ylim = c(0,50))
# 
# draw_a_circle <- function(r=1, x=0,y=0){
#   angles <- seq(0,2*pi,length = 100)
#   pts_x <- x + r*cos(angles)
#   pts_y <- y + r*sin(angles)
#   polygon(pts_x, pts_y)
# }
# 
# College$Yield <- with(College, Enroll/Accept)
# mod1 <- lm(Yield~Accept, data = College)
# mod2 <- lm(Yield~Accept+Private, data = College)

#test
require(scoreActivity, quietly = TRUE )
score253(day = 5)
