#4.3
setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 4")
source("functions_Ch5.txt")
light= source("light.txt")

sample43 = c(light$value$'1879', light$value$'1882')

Dn = ks.test(sample43, "pnorm", mean = mean(sample43), sd = sd(sample43), alternative = "two.sided")$statistic
#Dn = 0.0671

#test statistic
D = function(sample){
  ks.test(sample, "pnorm", mean = mean(sample), sd = sd(sample), alternative='two.sided')$statistic
}

#empirical BS
BS = bootstrap(sample43, D, B=1000)
mean(BS) #0.09
hist(BS, col='blue', xlab='p-values', main='Histrogram: Empirical Bootstrap')
abline(v= Dn, col='red')

quantile(BS)
quantile(BS, 0.0013) #0.13%

#parametric BS
set.seed(20202020)
BS2= replicate(1000,D(rnorm(length(sample43), mean = mean(sample43), sd=sd(sample43))))
mean(BS2) #0.056
hist(BS2, col='blue', xlab='p-values', main='Histrogram: Parametric Bootstrap')

p_val = mean(BS2 >= Dn) #0.187

