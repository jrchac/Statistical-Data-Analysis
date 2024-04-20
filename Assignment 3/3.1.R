#3.1
#experimenting with bandwidth

setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 3")
#source("functions_Ch3.txt")

sample31 =scan("sample31.txt")
par(mfrow=c(1,2))
hist(sample31, breaks=40)

#Question TA: which plot is better?
#Which is a better estimation? 
#Rule to use the histograms? Different histograms -> different bandwidth
hist(sample31, breaks=80)
plot(density(sample31, bw=0.15))

hist(sample31, breaks=40)
plot(density(sample31, bw=0.3))


#shapiro.test(sample31) #reject normality
#ks.test(sample31, pnorm)
#ks.test(sample31, pexp)
#ks.test(sample31, punif)
#ks.test(sample31, plnorm)

par(mfrow=c(2,3))
plot(density(sample31, kernel='gaussian', bw=0.1))
plot(density(sample31, kernel='gaussian', bw=0.2))
plot(density(sample31, kernel='gaussian', bw=0.3))
plot(density(sample31, kernel="epanechnikov", bw=0.1))
plot(density(sample31, kernel="epanechnikov", bw=0.2))
plot(density(sample31, kernel="epanechnikov", bw=0.3))

# what would the optimum plot using this method would look like?

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

h1 = h_log(sample31)
h2 = h_dexp(sample31)
h3 = h_exp(sample31)

par(mfrow=c(1,3))
plot(density(sample31, kernel='gaussian', bw=h1))
plot(density(sample31, kernel='gaussian', bw=h2))
plot(density(sample31, kernel='gaussian', bw=h3))

