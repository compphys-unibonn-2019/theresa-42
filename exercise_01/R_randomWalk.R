#random walk in 2 dim.
# 100 steps?
# x=[0],y=[0] initial condition
x<-c(0)
y<-c(0)
for(i in c(1:100)){
  #random number in 0 to 1 , random uniform
  d<-runif(1)
  #rounding od numbers, s is 0 or 1
  s<-round(runif(1))
  #four cases in equal probability
  if(d<=0.5){
    x<- c(x, x[i]+(-1)^s)
    y<- c(y, y[i])
  }

  else{
    x<-c(x, x[i])
    y<-c(y, y[i]+(-1)^s)
  }
}
str(x)
str(y)
plot(x,y)

