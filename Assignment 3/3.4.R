#3.4
setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 3")
source("functions_Ch5.txt")
source("functions_Ch4.txt")

sample34 = list34$sample34 #n=120
true.x = list34$true.density.x #n=512 with x-values of true density
true.y = list34$true.density.y #n=512 with y-values

#1
par(mfrow=c(1,1))
h34 = h_opt(sample34) #0.00788
plot(density(sample34, kernel='gaussian', bw=h34), main='Density Function with h_opt',lwd=2, col='palegreen')

#2) minimizer of the criterion of the CV on [0.0005, 0.008]
CV(h34, sample34, kernel='gaussian')

h_vec = seq(0.0005,0.008,length.out = 100)
cv_crit = sapply(h_vec, CV, sample=sample34, kernel="gaussian")
h_min =h_vec[which(cv_crit == min(cv_crit))] #0.0036
plot(density(sample34, bw=h_min), main='Density Function with Cross-Validation',lwd=2, col='darkgreen')

bw.nrd(sample34) #0.0071
bw.bcv(sample34) #0.0073
bw.ucv(sample34) #0.0037

#3
plot(true.x, true.y, type='l', main='True Density Function',lwd=2, col='darkblue', ylab='density', xlab='values')

par(mfrow=c(1,2))
plot(density(sample34, kernel='gaussian', bw=h34), main='Density Function with h_opt',lwd=2, col='darkgreen')
plot(density(sample34, bw=h_min), main='Density Function with Cross-Validation',lwd=2, col='darkgreen')


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

#changing the optimal bandwitdh
h_log = h_log(sample34) #0.018 
h_dexp = h_dexp(sample34) #0.011
h_exp = h_exp(sample34) #0.0037

par(mfrow=c(1,3))
plot(density(sample34, bw=h_log), lwd=2, col='blue', main='Optimal Logarithmic Bandwith')
plot(density(sample34, bw=h_dexp), lwd=2, col='blue', main='Optimal Double Exp. Bandwith')
plot(density(sample34, bw=h_exp), lwd=2, col='blue', main='Optimal Exponential Bandwith')
