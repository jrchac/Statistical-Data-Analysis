#3.5
setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 3")
source("functions_Ch5.txt")
source("functions_Ch4.txt")

t.sample= scan('t-sample.txt')

#a
par(mfrow=c(1,1))
ecdft = ecdf(t.sample)
ecdf1_sample = ecdft(1) #0.780
plot(ecdft, main='ECDF: Sample', xlab='sample values', col='blue')
#abline(v=1, col='red')
#abline(h=ecdf1_sample, col='red')
points(1, ecdf1_sample, col='red')

#b use the empirical bootstrap with B=2000
set.seed(20230303+34)

Tstatistic = function(x){
  return (ecdf(x)(1))
}

ecdf1_empBS = bootstrap(t.sample, Tstatistic, B=2000)
hist(ecdf1_empBS)
avg_emp = mean(ecdf1_empBS) #0.779


#c use the parametric bootstrap (t distribution with k degrees of freedoms)
set.seed(20230303+34)
s2 = var(t.sample) #1.395
k = 2*s2/(s2-1) #7.064

ecdf1_parBS = replicate(2000,Tstatistic(rt(length(t.sample), df=k)))
hist(ecdf1_parBS)
avg_par = mean(ecdf1_parBS) #0.824

#d
#plot histograms of c and b
#hist for true distribution T

set.seed(20230303+34)
ecdf1_realizations = replicate(2000, Tstatistic(rt(50, df=5)))
avg_realizations = mean(ecdf1_realizations) #0.817

par(mfrow=c(1,3))
hist(ecdf1_empBS,main='Histogram: Empirical Bootstrap', col="lightblue", xlab="T-statistic")
hist(ecdf1_realizations, main='Histogram: Realizations', col="blue", xlab="T-statistic")
hist(ecdf1_parBS,main='Histogram: Parametric Boostrap', col="skyblue", xlab="T-statistic")
#conclusion in this case the parametric is much better method


par(mfrow=c(1,1))
hist(ecdf1_empBS, col="lightblue", xlab="T-statistic")
hist(ecdf1_realizations, col="blue", xlab="T-statistic", add = TRUE, alpha=1)

hist(ecdf1_realizations, col="blue", xlab="T-statistic",add = TRUE)
hist(ecdf1_parBS, col="skyblue")


#e)

sd_empBS = sd(ecdf1_empBS) #0.059
sd_parBS = sd(ecdf1_parBS) #0.054
sd_realizations = sd(ecdf1_realizations) #0.054
