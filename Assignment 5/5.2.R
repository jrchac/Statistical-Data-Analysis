#5.2
setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 5")
source("functions_Ch5.txt")
sample52 = read.table('clouds.txt')
seeded = sample52$seeded.clouds
unseeded = sample52$unseeded.clouds

#a) graphical and numerical description
par(mfrow=c(1,2))
hist(seeded, col='lightblue', xlab='precipitation')
hist(unseeded, col='lightgreen', xlab='precipitation') #very similar

boxplot(seeded, main='Boxplot: Seeded', col='lightblue')
boxplot(unseeded, main='Boxplot: Unseeded', col='lightgreen') #very similar

par(mfrow=c(1,1), pty='s')
qqplot(seeded, unseeded, main='QQ-Plot: Seeded vs Unseeded', col='darkgreen') #might be same scale-location family

summary(sample52)
cor(sample52)


#b) sample standard deviation
sd.unseeded= sd(unseeded) #278.45
E2.b.sd = sd(seeded) 
sd.seeded =sd(seeded) #650.79

#c) bootstrap estimate of the standard deviation 
set.seed(20202020)
#bs.sd.seeded = mean(bootstrap(seeded, sd, 2000)) #611.69
bs.sd.seeded2 = sd(bootstrap(seeded, sd, 2000)) #167.19

#bs.sd.unseeded = mean(bootstrap(unseeded, sd, 2000)) #260.43
bs.sd.unseeded2 = sd(bootstrap(unseeded, sd, 2000)) #84.03


#d) sample MAD and bootstrap estimate of the MAD
mad.seeded = mad(seeded) #229.95
E2.d.mad= mad(seeded)
mad.unseeded = mad(unseeded) #56.78

#bs.mad.seeded = mean(bootstrap(seeded, mad, 2000)) #220.37
#bs.mad.unseeded = mean(bootstrap(unseeded, mad, 2000)) #60.18

bs.mad.seeded2 = sd(bootstrap(seeded, mad, 2000)) #71.32
bs.mad.unseeded2 = sd(bootstrap(unseeded, mad, 2000)) #33.87

#e) which estimator is preferred? why?
# mad is a better estimator when there are many outliers or when distr. not normal
# sd is heavily influenced by extreme values 
# mad is a robust measure of spread that is less influenced by outliers


#f) which test do you use for testing the location of the precipitation values of the
#seeded clouds: t-test, sign test or the signed rank test? why?

#the sign test is the best options. It is non-parametric (it does not assumes a distribution)
#it is robust to outliers (which we already know we have)
#The other options are problematic for this sample. 
#the t-test is a parametric test that assumes normality and there is no sign of normality
#in this sample (that is heavily skewed).
#the signed rank test is much better suited when the data is symmetric. that is because it
#uses the comparison of the sum of ranks in the positive differences and the sum of the negative
#differences. It is also robust to outliers.


#g) using test selected in f), test if the location is less than 40 with alpha 0.05

binom.test(sum(seeded>40), length(seeded))[[3]] #0.0025
#Conclusion: the location value is signifcantly less than 40

#wilcox.test(seeded, mu=40, alternative='g')[[3]] 
#signed rank test: not optimal due to asymetric data


#h) make two-sided 95%-CI for location on the sign test, signed rank test and t-test
#note: warnings may be ignored

#t-test
t.test(seeded, mu=40)#[179.12, 704.84]
t.test(seeded, conf.level=0.95) #correct way to solve it 
E2.h.CI.t = c(179.12, 704.84)


#signed rank test
wilcox.test(seeded, mu=40, conf.int = TRUE, conf.level = 0.95) #[147.80, 505.35]
E2.h.CI.wilcox=c(147.80, 505.35)

#sign test
rbind(0:length(seeded), round(pbinom(0:length(seeded), size=length(seeded), p=0.5),3))
rbind(0:length(seeded), round(1-pbinom(0:length(seeded)-1, size=length(seeded), p=0.5),3))
CI_l = sort(seeded)[8] 
CI_u = sort(seeded)[19] #
CI = c(CI_l, CI_u) #115.3 and 334.1
E2.h.CI.sign= CI

E2.h.CI.sign.upper.bound.included = FALSE
E2.h.CI.sign.lower.bound.included = TRUE

#1-P(T<=18)
1-pbinom(18, length(seeded)-1, 0.5)
pbinom(9, length(seeded)-1, 0.5)

#i) which interval from h) do you value most? why?
#from the calculated confidence interval, the best/preferable CI is the one with the 
#lower range in the interval. In this case, the best is the signed rank test



