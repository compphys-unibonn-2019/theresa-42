Means <- c()
Vars <- c()
i <- 1
for (n in c(1:25)){
  X <- array (runif(10000*n), dim=c(n,10000))
  #def Z_n, Z_n is a function
  myZ <- function(x,n){
    (sum(x)-n/2)*sqrt(12/n)
  }
  #apply is like a for loop
  #apply(array,margin (how it read the array), fun (function to be applied))
  myN <- apply (X,2,myZ,n=n)
  #mean is a generic function
  Means[i] <- mean(myN)
  Vars[i] <- var(myN)
  i <- i+1
}

plot (x=c(1:25), y=Means)
plot (x=c(1:25), y=Vars)
