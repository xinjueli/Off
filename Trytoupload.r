#Hello 

setwd("C:/Teaching")
library(fBasics) 

# Daily returns of Apple stock: 2000 to 2009 (simple returns)
x=read.table("d-aapl0009.txt",header=T)
dim(x)
x[1,]
y=ts(x[,2],frequency=252,start=c(2000,1))
plot(y,type='l',xlab='year',ylab='rtn')
title(main='Daily returns of Apple stock: 2000 to 2009')
par(mfcol=c(2,1)) 
hist(y,nclass=50)
d1=density(y)
plot(d1$x,d1$y,xlab='returns',ylab='den',type='l')
    
# Monthly SP index: index vs returns
x=read.table("m-sp2609.txt",header=T)
dim(x)
tdx=c(1:1008)/12+1926
par(mfcol=c(2,1))
plot(tdx,x[,3],xlab='year',ylab='index',type='l')
plot(tdx,x[,2],xlab='year',ylab='return',type='l')
    
# Monthly U.S. TB3ms and TB6ms: level vs spread
x=read.table("m-tb3ms.txt",header=T)
y=read.table("m-tb6ms.txt",header=T)
dim(x)
dim(y)
914-615
x[300,]
y[1,]
int=cbind(x[300:914,4],y[,4])
tdx=(c(1:615)+11)/12+1959
par(mfcol=c(2,1))
max(int)
plot(tdx,int[,1],type='l',ylim=c(0,16.5))
lines(tdx,int[,2],lty=2)
plot(tdx,int[,2]-int[,1],type='l')
abline(h=c(0))

# Quarterly earnings of Coca Cola (1983-2009): Seasonality
x=read.table("q-ko-earns8309.txt",header=T)
dim(x)
x[1,]
y=ts(x[,3],frequency=4,start=c(1983,1))
plot(y)
points(y,pch=c('1','2','3','4'))
# Alternatively
tdx=c(1:107)/4+1983
plot(tdx,x[,3],xlab='year',ylab='earnings',type='l')
title(main='EPS of Coca Cola: 1983-2009')
points(tdx,x[,3])

# Daily FX USD vs Euro: level plot
y=read.table("d-useu.txt",header=T)
dim(y)
y[1,]
tdx=c(1:2807)/252+1999
plot(tdx,y[,4],xlab='year',ylab='eu',type='l')
title(main='Dollars per Euro')
r=diff(log(y[,4]))
plot(tdx[2:2807],r,xlab='year',ylab='rtn',type='l')
title(main='ln-rtn: US-EU')
hist(r,nclass=50)
