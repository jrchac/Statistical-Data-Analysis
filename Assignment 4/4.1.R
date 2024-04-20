#4.1
setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 4")
source("functions_Ch3.txt")
source("functions_Ch5.txt")

sample41 = scan("birthweight.txt")

#a) explore the distribution graphically
par(mfrow=c(2,2))
hist(sample41, freq=FALSE, main='Histogram: Birth Weight', xlab='grams', col='blue')
symplot(sample41, main="Symplot: Birth Weight", col='darkblue') #most on the line but some high valus fall below the median 
boxplot(sample41, main='Boxplot: Birth Weight', col='skyblue' ) #also symmetrical boxplot with a visible outlier on the lower part of the graph
qqnorm(sample41, col='blue') 
qqline(sample41, col='red')

summary(sample41)
sample.mean = mean(sample41) #2944.66
sample.sd= sd(sample41) #729.

#QQ-plots and Tests #Successful and insightful
par(mfrow=c(1,1),pty='s')
qqnorm(sample41) 
qqline(sample41, col='red')
shapiro.test(sample41) #p-value=0.43 -> cannot reject normality

#other possible approximations
par(mfrow=c(1,3),pty='s')
qqlogis(sample41, col='darkgreen') #good approx.
qqchisq(sample41, df=100, col='purple') #also very good
qqt(sample41, df=100, col='orange') #best candidate so far

#failed
qqcauchy(sample41)
qqexp(sample41) 
qqunif(sample41) 
qqlnorm(sample41)
qqlaplace(sample41) 

#all failed
ks.test(sample41, "plogis", 0, 1)
ks.test(sample41, "pt", df=15)
ks.test(sample41, "pchisq", df=100)
ks.test(sample41, "plogis")
ks.test(sample41, "pnorm", exact=FALSE)


#b) estimate the %5 quant & find a bootstrap estimate of standard deviation
quant5 = quantile(sample41, prob=0.05) #1801
statistic = function(x){ quantile(x, prob=0.05)}

#empirical bootstrap
empBS = bootstrap(sample41, statistic = statistic, B=2000)
empBS.sd = sd(empBS) #115.96
empBS.mean =mean(empBS) #1791.27
#slightly better the empirical than the parametric

#parametric bootstrap
set.seed(202020)
parBS = replicate(2000, statistic(rnorm(length(sample41), mean=sample.mean, sd=sample.sd)))
parBS.sd = sd(parBS) #111.46
parBS.mean = mean(parBS) #1762.71

par(mfrow=c(1,2))
hist(empBS, main='Histogram: Empirical Bootstrap', col='darkgreen')
hist(parBS, main='Histogram: Parametric Bootstrap', col='darkgreen')

#c) use a parametric bootstrap based on lognormal distribution
#parametric bootstrap
log_sample = log(sample41)
log.mean= mean(log_sample)
log.sd = sd(log_sample)

set.seed(202020)
logBS = replicate(2000, statistic(rlnorm(length(sample41), mean=log.mean, sd=log.sd)))
logBS.mean = mean(logBS) #1799.85 
logBS.sd = sd(logBS) #77.50

par(mfrow=c(1,1))
hist(logBS, main='Histogram: Parametric Bootstrap', col='green')

#d) code again c in only one line

set.seed(202020)
BS4 = replicate(2000, quantile(rlnorm(length(sample41), mean=log.mean, sd=log.sd), prob=0.05))
mean.bs4 = mean(replicate(2000, quantile(rlnorm(length(sample41), mean=log.mean, sd=log.sd), prob=0.05)))

mean.bs4 = mean(BS4)
sd.bs4 = sd(BS4) #75.64

set.seed(202020)
sd(replicate(2000, quantile(rlnorm(length(sample41), mean=mean(log(sample41)), sd=sd(log(sample41))), prob=0.05)))
