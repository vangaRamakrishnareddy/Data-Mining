df<-read.csv("HR.csv")

options(max.print = 15000)
print(df)
sapply(Filter(is.numeric,df),mean)
sapply(Filter(is.numeric,df),median)

mode<- function(a){
  
  for(j in 1:a){
    b<- (-2*mean(df[,j])-3*median(df[,j]))
    print(b)
  }
}
mode(8)
#sapply(Filter(is.numeric,df),mode(8))
variance<- function(b){
  
  for(j in 1:b){
    c<- var(df[ ,j])
    print(c)
  }
}
variance(8)
standard_d<- function(b){
  
  for(j in 1:b){
    c<- sqrt(var(df[ ,j]))
    print(c)
  }
}
standard_d(8)

#mode<- -2*mean(df$satisfaction_level)-3*median(df$satisfaction_level)

x<-df[ ,1:5]
result<-cor(x)
library(corrplot)

y <-corrplot(cor(df[,1:8]))

write.csv(y,"corre.csv")
z<-cov(df[,1:8])
write.csv(z,"cov.csv")