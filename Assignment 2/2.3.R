#2.3
setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 2")
source("functions_Ch3.txt")

sample = scan("sample2023b.txt")

#a) find an appropiate distribution (explore graphically)
# indicate location and scale
par(mfrow=c(1,3))
#hist(sample, breaks=50) #left skewed, with several tops
hist(sample, breaks= 25, main="Histogram: Sample", col='lightyellow')
boxplot(sample, main="Boxplot: Sample", col='lightblue')
symplot(sample, main="Symmetry Plot: Sample", col='blue')
plot(ecdf(sample))
summary(sample)

par(mfrow=c(1,2), pty='s')
qqchisq(sample, df=15, col='green') #this is possible (best candidate)
#abline(0.15,0.25, col='red')
abline(-0.15,0.265, col='red')
qqnorm(sample, col='lightgreen') #possibly this one
qqline(sample, col='red')
shapiro.test(sample) #normality discarded with the Shapiro-Wilk test
qqexp(sample) #less possible 

qqlnorm(sample) #not this one
qqt(sample, df=20) #not this one
qqunif(sample) #not this one
qqcauchy(sample) #not this one
qqlogis(sample)
qqlaplace(sample)



#note: rate = 1/scale


#b) test if sample comes from Gompertz distribution with b=0.7 and n=0.07
rate=0.7
tscale = 1/rate

#qqplot for the Gompertz Distribution 
sample_quantiles = quantile(sample, probs=seq(0.01,0.99,0.01))
gompertz_quantiles = qgompertz(seq(0.01,0.99,0.01),shape=0.07, scale=1)
par(mfrow=c(1,1), pty='s')
plot(sample_quantiles, gompertz_quantiles, main='QQ plot: sample vs Gompertz', col='darkgreen', xlab='sample quantiles', ylab='gompertz quantiles')
abline(0.5,0.5, col='red')

#Kolmogorov-Smirnov
test1 = ks.test(sample, pgompertz, scale=0.07, shape=tscale)
ks_pvalue = test1$p.value
ks_score = test1$statistic
ks_reject = test1$exact
ks_score
ks_pvalue 


#c) use now the chi-square test. choose arguments so the rule of thumb is fulfilled
temp = quantile(sample,probs=seq(0.01,0.99,0.15))
br = c(1, 2.2, 2.6, 3, 3.3, 3.6, 4.3, 9)
test2 = chisquare(sample, pgompertz, shape=0.07, scale=tscale, breaks=br)
chisq_breaks = br
chisq_score = test2$chisquare
chisq_pvalue= test2$pr
chisq_reject = 'true'
chisq_score
#d) do b anc c agree? interpret the results 

