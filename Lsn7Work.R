
density<-function(x,y){
  result<-6*x^2*y
  return(result)
}

x.old<-.3
y.old<-.5
x.vec<-c()
y.vec<-c()

#Here I'm doing what's called a Metropolis Algorithm to simulate from problem 5.32
for(l in 1:100000){
  x.new<-runif(1,0,min(y.old,2-y.old))
  numer.x<-density(x.new,y.old)
  denom.x<-density(x.old,y.old)
  accep<-min(1,numer.x/denom.x)
  if(runif(1)<accep){
    x.old<-x.new
  }
  y.new<-runif(1,x.old,2-x.old)
  numer.y<-density(x.old,y.new)
  denom.y<-density(x.old,y.old)
  accep<-min(1,numer.y/denom.y)
  if(runif(1)<accep){
    y.old<-y.new
  }
  x.vec[l]<-x.old
  y.vec[l]<-y.old
}

#We can plot the joint density to see what it looks like
library(plot3D)

x_c <- cut(x.vec, 100)
y_c <- cut(y.vec, 100)

z <- table(x_c, y_c)
#Joint PDF
image2D(z=z, border="black")

#Marginals
hist(x.vec)

hist(y.vec)

#Compare to Actual
hist(rbeta(10000,3,2))


#Simulate Conditional
sub.y<-y.vec[x.vec>.59&x.vec<.62]

length(sub.y[sub.y<1.1])
