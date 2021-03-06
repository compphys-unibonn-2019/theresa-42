---
  
  title: "HW3"

output: pdf_document

---
  
  
  
  ```{r setup, include=FALSE}

knitr::opts_chunk$set(echo = TRUE)

library("scatterplot3d") # load

```





## Qustion 1 : Code



```{r}

f_X<-function(x){1/(pi*sqrt(x*(1-x)))
  
}



inverse_F_X<-function(y){
  
  #inverse of (2/pi)*arcsin(sqrt(x))
  
  sin((pi/2)*y)^2
  
}



F_X<-function(x){
  
  (2/pi)*asin(sqrt(x))
  
}



N<-50000



x<-runif(N,0,1)



```





## Question 1 : test inverse



```{r}

# t e s t  inverse

pdf( "task_inversefunction.pdf" ,   width =5 , height =5)

plot( x=x , y=F_X(x) ,
      
      main=" testing F_X^-1(F_X(x))" ,
      
      ylab= 'y' ,
      
      xlab= 'x' ,
      
      pch= '.' , cex =2 ,col="red" )

points( x=x , y=inverse_F_X(F_X( x ) ) , pch= '.' , cex =2)

legend( "topleft" , pch= '.' ,col=c( "red" , "black" ) ,
        
        legend=c( " y=F_X( x ) "," y=inverse_F_X(F_X(x))" ) ,
        
        pt.cex =5 , bty= ' n ' )



y<-inverse_F_X( x )

ytest<-f_X( x )

vNmax<-N/( 2 ^ rev( 0 : 4 ) )



for(Nmax in vNmax){
  
  h<-hist( y [ 0 : Nmax] ,   breaks=seq( 0 , 1 ,by= 0.05 ) ,plot=FALSE)
  
  plot( x=h$mids ,   y=h$density, col="black", type= 'h',
        
        main=paste0( "N=" ,Nmax),
        
        ylab="f_X(x)",
        
        xlab="x",
        
        ylim=c( 0, 2.5 )
        
  )
  
  points( x=x , y=ytest , pch= '.' , cex =2)
  
}

dev.off() 



```



## Question 2





```{r}

a<-c(7^5, 19, 65539)

m<-c(2^31-1,6788793,2^31)

X<-1



for(i in 10000){
  
  X<-c(X, a*X[i] %% m)
  
  x_axis<-c(x_axis,X[i]/m)
  
  y_axis<-c(y_axis,X[i+1]/m)
  
  z_axis<-c(z_axis,X[i+2]/m)
  
}

iris<-data.frame(x_axis,y_axis,z_axis)

scatterplot3d(iris[,1:3],
              
              main="3D Scatter Plot",
              
              xlim = c(0,1))

```