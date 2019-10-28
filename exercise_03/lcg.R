#linear congruential random number generator
rrandu <- function(n){
  x0 <- 5
  M <- 2^21
  a <- 65539
  x <- x0
  for(i in c(1:n)){
    x[i+1] <- (a*x[i]) %% M
  }
  return (x/(M+1))
}


#histo
x <- rrandu(10000)
idx <- c(1:9998)
hist(x, main="", freq=FALSE)
#calculate mean and variance
cat("mean value is",mean(x))
cat("\nvariance is",var(x))

#scatterplot 3 dim.
#points not uniformly distributed

library(scatterplot3d)
scatterplot3d(x=x[idx], y=x[idx+1], z=x[idx+2], angle=20,
              xlab="x", ylab="y", zlab="z")
