#3.2
#pick kernel function and a bandwidth
#use kernel density estimator -> estimate density
setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 3")
source("functions_Ch4.txt")
sample32=scan("sample32.txt")

par(mfrow=c(1,2))
hist(sample32, breaks=15, main='Histogram: sample (br=15)', xlab='sample', col='lightblue')
hist(sample32, breaks=20, main='Histogram: sample (br=20)', xlab='sample', col='lightblue')
#by plotting the hist, now we have an idea of the shape of the sample
#plot(density(sample32))


#systematical approach: 
#1) choose a kernel
#2) systematical choose bandwidth from an array of bandwidth
#3) compute optimal bandwidth using:
  #-Silverman's Rule
  #-Sheater-Jones Method

#Gaussian/Normal Kernel
h= c(0.15,0.2,0.3)
par(mfrow=c(1,3))
for(x in h){
  plot(density(sample32, kernel='gaussian', bw=x))
}
#values from 0.35-1.0 are over-smoothing the plot

#Epanechnikov
h= c(0.15,0.2,0.3)
par(mfrow=c(1,3))
for(x in h){
  plot(density(sample32, kernel='epanechnikov', bw=x))
}

#triangular
h= c(0.15,0.2,0.3)
par(mfrow=c(1,3))
for(x in h){ 
  plot(density(sample32, kernel='triangular', bw=x))
}

#rectangular
#tunning
h= c(0.15,0.2,0.3,0.5,0.6,0.7)
par(mfrow=c(2,3))
for(x in h){
  plot(density(sample32, kernel='rectangular', bw=x))
}

#biweight
h= c(0.15,0.2,0.3,0.5,0.6,0.7)
par(mfrow=c(2,3))
for(x in h){
  plot(density(sample32, kernel='biweight', bw=x))
}
#over-smoothing from 0.25 and on

#cosine
h= c(0.15,0.18,0.2)
par(mfrow=c(1,3))
for(x in h){
  plot(density(sample32, kernel='cosine', bw=x))
}
#over-smoothing from 0.3

#alternative way to estimate
#using Silvermans rule and Sheater-Jones method
par(mfrow=c(2,3))
plot(density(sample32), bw=h_opt(sample32),lwd=2,col='blue',main="KDE using h_opt")
plot(density(sample32, bw="nrd"),lwd=2,col='blue',main="KDE using nrd")#normal reference rule-of-thumb
plot(density(sample32, bw="nrd0"),lwd=2,col='blue',main="KDE using nrd0") ##rule of thumb with less sentitivity to outliers
plot(density(sample32, bw="ucv"),lwd=2,col='blue',main="KDE using ucv") #unbiased cross validation (minimizes MISES)
plot(density(sample32, bw="bcv"),lwd=2,col='blue',main="KDE using bcv") #biased cross validation 
plot(density(sample32, bw="sj"),lwd=2,col='blue',main="KDE using SJ")#sheater jones method 

nh= h_opt(sample32) #0.20 optimal bandwidth using chapter 4 formula


par(mfrow=c(2,3))

plot(density(sample32, bw="nrd"), lwd=2, col='green',main="")#normal reference rule-of-thumb
plot(density(sample32, bw="nrd0"),lwd=2, col='green', main='Gaussian Kernel') ##rule of thumb with less sentitivity to outliers
plot(density(sample32, bw="ucv"),lwd=2, col='green',main="") #unbiased cross validation (minimizes MISES)
plot(density(sample32, kernel='epanechnikov', bw="nrd"),lwd=2,col='blue',main="")
plot(density(sample32, kernel='epanechnikov', bw="nrd0"),lwd=2,col='blue', main="Epanechnikov Kernel")  
plot(density(sample32, kernel='epanechnikov', bw="ucv"),lwd=2,col='blue',main="") 
#legend("topright", legend=c('gaussian', 'epanechnikov'), col=c('green', 'blue'), lty=1, cex=0.8, inset=0.02)


#plot(density(sample32, kernel='epanechnikov', bw="nrd"))
#plot(density(sample32, kernel='epanechnikov', bw="nrd0"))  
#plot(density(sample32, kernel='epanechnikov', bw="ucv")) 
#plot(density(sample32, kernel='epanechnikov', bw="bcv")) 
#plot(density(sample32, kernel='epanechnikov', bw="sj"))

par(mfrow=c(1,1))
plot(density(sample32,kernel='gaussian', bw="nrd0"), lwd=2, col="darkgreen", main='Kernel Density Estimator: Gaussian')


h_log <- function(x){
  c = pi^5 * (13/ (3^(7/2)*35) )
  return (c*sd(x)*length(x)^(-1/5)) 
}
h_dexp <- function(x){
  return (sqrt(2)*sd(x)*length(x)^(-1/5)) 
}
h_exp <- function(x){
  return (0.5*sd(x)*length(x)^(-1/5)) 
}


h1=h_log(sample32)
h2=h_dexp(sample32)
h3=h_exp(sample32)

par(mfrow=c(1,3))
plot(density(sample32, bw=h1),lwd=2, col='green',main="") #0.47 over smoothed
plot(density(sample32, bw=h2),lwd=2, col='green',main="") #0.28 over smoothed
plot(density(sample32, bw=h3),lwd=2, col='green',main="") #0.098 under smoothed

plot(density(sample32, kernel='epanechnikov', bw=h1),lwd=2, col='darkgreen',main="") #0.47 over smoothed
plot(density(sample32, kernel='epanechnikov', bw=h2),lwd=2, col='palegreen',main="") #0.28 over smoothed
plot(density(sample32, kernel='epanechnikov', bw=h3),lwd=2, col='green',main="") #0.098 under smoothed


nrd=bw.nrd(sample32) #0.202
nrd0=bw.nrd0(sample32)#0.171
bcv=bw.bcv(sample32) #0.224
ucv=bw.ucv(sample32) #0.126
sj=bw.SJ(sample32) #0.142