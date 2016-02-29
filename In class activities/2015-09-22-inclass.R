# Jingjing Yang
# Sept 22, 2015

#Task1
library(mosaicData)

myHistogram <- function(col = "tomato"){
  nbins <- 10
  min <- min(Galton$height)
  max <- max(Galton$height)
  evenly_spaced <- seq(min, max, by = (max-min)/(nbins))
  bin_counts<- table(cut(Galton$height, breaks = nbins))
  hist_basics <- data.frame(xL = evenly_spaced[-11], xR = evenly_spaced[-1], count = as.numeric(bin_counts))
  bars <- NULL
  for ( k in 1:nrow(hist_basics)){
    xLeft <- hist_basics[k, ]$xL
    xRight <- hist_basics[k, ]$xR
    height <- hist_basics[k, ]$count
    res <- data.frame(x=c(xLeft, xLeft, xRight, xRight, NA), y = c(0,height,height,0,NA))
    bars <- rbind(bars, res)
  }
  My_bars <- data.frame(bars)
  plot(My_bars, type = "n")
  lines(My_bars)
  polygon(My_bars, col = col)
}


#Task2
v <- rnorm(10, mean = 100, sd = 1)
bw <- diff(range(v)) * 2/length(v)
kernel <- function(v,x){
  dnorm(v, mean = x, sd = bw)
}
x <- seq(min(v) - 5*bw, max(v) + 5*bw, length = 200)
Dvals <- outer(v,x, FUN = kernel)
density <- colSums(Dvals)/length(v)
den <- data.frame(x,density)
plot(den)


#Task3

plotdensity <- function(v, xlim = NULL){
  bw <- diff(range(v)) / sqrt(length(v))
  x <- seq(min(v) - 5*bw, max(v) + 5*bw, length = 200)
  Dvals <- outer(v,x, FUN = function(v,x) dnorm(v, mean = x, sd = bw))
  density <- colSums(Dvals)/length(v)
  den <- data.frame(x,density)
  plot(x, density, xlim = xlim, type="l")
  invisible(den)
}

#test
require(scoreActivity, quietly = TRUE )
score253(day = 6)