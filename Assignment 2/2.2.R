setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 2")
source("functions_Ch3.txt")
#Excersice 2.2

#a) make quantile function QQ-plots
par(pty='s')
x = seq(0.01, 0.99, 0.01)
# I) logn(0,2) vs exp(2)
lognorm_q = qlnorm(x, meanlog = 0, sdlog = 2)
exp_q = qexp(x, 2)
plot(lognorm_q, exp_q, pch = 20, main="QQ-plot: Log. vs Exp.", 
     xlab = "Lognormal", ylab = "Exponential", type='l', col= 'blue')

#i should have also added a comparison between the both quantile functions of the distributions


plot(exp_q,lognorm_q, pch = 20, main="QQ-plot: Log. vs Exp.", 
          xlab = "Exponential", ylab = "Lognormal", type='l', col= 'blue')

# II) n(2,16) vs t3
norm_q = qnorm(x, mean=2, sd=16)
t3_q = qt(x, df=3)

plot(norm_q, t3_q, type='l', main='QQ-Plot: N(2,16) vs t3-distr.',
     xlab='normal distribution', ylab='t-distribution', col= 'dark green')

# III) chi(20 deg) vs chi(3 deg)
chis_q = qchisq(x, df=20)
chis_q2 = qchisq(x, df=3)
plot(chis_q, chis_q2, type='l', main = 'QQ-Plot: Chisquare(20) vs Chisquare(3)',
     xlab='degree=20', ylab='degree=3')

#just to check a case when there is a straight line
norm1 = qnorm(x)
norm2 = qnorm(x, mean=1, sd=4)
plot(norm1, norm2,type='l')

#getting the 3 plots in only one figure:
par(mfrow=c(1,3), pty='s')

#b) 
#find distribution
#specify a suitable location-scale family 
#give values for the location and scale parameters (the line)

#hint: qqline and abline can be helpful
sample = scan("sample2023a.txt")
sample_quantiles = quantile(sample, probs = seq(0.01,0.99,0.01))
sample_mean = mean(sample)
sample_sd = sd(sample)
sample_var= var(sample)

par(mfrow=c(1,1),pty='s')
hist(sample, breaks=30, main='Histogram: Sample')
symplot(sample, main='Symmetry Plot: Sample')
empiricalDist = ecdf(sample)
plot(empiricalDist, main='ECDF: Sample')

#best candidates
par(mfrow=c(1,2), pty='s')
qqexp(sample, col='blue')   #testing for exponential
abline(a=0.5, b=3.3, col='red')

qqlnorm(sample, col='lightgreen')
abline(a=0.7, b=1.95, col='red')

ks.test(sample, pexp(0.25))
#conclusion: very possible it is exponential (best candidate)
#conclusion: very possible it is logarithmic normal


#testing for chisquare
par(pty='s')
qqchisq(sample, df=6)
abline(a=-0.6, b=0.7, col='red')
#weakest option 

#failed tests:
#testing for normal distribution
par(pty='s')
qqnorm(sample)
qqline(sample)
shapiro.test(sample)
#testing for laplace distribution
qqlaplace(sample)

#testing for cauchy
qqcauchy(sample)

#testing for logistic
qqlogis(sample)

#testing for uniformal
qqunif(sample)

#testing for t-distribution
qqt(sample, df=2)

# we can also try some transformation of the data. Most common: log(x), sqrt(x), x^2