bootstrap <- function(data, statistics = median, k = 400){
  res <- numeric(k)
  for (i in 1:k){
    #create a resampling
    resample <- sample(1:length(data), replace = TRUE)
    #apply the statistics to that resampling
    val <- statistics(resample)
    #keep track of the results
    res[i] <- val
    #give back the results
  }
  return(res)
}

