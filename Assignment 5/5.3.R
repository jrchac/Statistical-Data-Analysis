#5.3
setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 5")

sample53 = scan('newcomb.txt')
first20 = sample53[1:20]
last46 = sample53[21:66]

#a
#investigate if there is a difference between first 20 and last 46 observations
#graphical summaries (use same scales for axes)
par(mfrow=c(2,2))
hist(first20, xlim=c(-50,45), ylim=c(0,22), col='green', main='Histogram: First 20 Obs.')
hist(last46, xlim=c(-50,45), ylim=c(0,22), col='blue', main='Histogram: Last 46 Obs.')

ylim <- c(min(first20, last46), max(first20, last46))
boxplot(first20, col='green', ylim=ylim, main='Boxplot: First 20 Obs.')
boxplot(last46, col='blue', ylim=ylim, main='Boxplot: Last 46 Obs.')

par(mfrow=c(1,1), pty='s')
qqplot(first20, last46, col='darkgreen', main='QQ Plot First_20 vs Last_46') #not a straight line/ not from same scale-location fam

par(mfrow=c(1,2), pty='s')
qqnorm(first20, col='green', main='Normal QQ Plot: First20')
qqline(first20)
qqnorm(last46, col ='blue', main='Normal QQ Plot: Last46')
qqline(last46)


#determine an estimate of the difference
mean(first20) #21.75
mean(last46) #28.15
median(first20) #25.5
median(last46) #28

diffMean = mean(last46) - mean(first20) #6.4
diffMedian = median(last46) - median(first20) #2.5

#tests: wilcox.test, ks.test (all tests for 2 samples)
wilcox.test(first20, last46, conf.level = .9) #p-val=0.12
ks.test(first20, last46) #p-val=0.18 



#b
#90%-CI for the sample
#the true values is equal to 24.83 (in sec^-6)
#compare by subtracting 24.8 and then multiplying by 1000
#is the value in the interval?
#conclusion?
mean(sample53) #26.21
median(sample53) #27

t.test(sample53, conf.level=0.9) #(24.01, 28.42)

wilcox.test(sample53, conf.int = TRUE, conf.level = 0.9) #(26.5, 28.50)

true_value = (24.8332-24.8)*1000 #33.20


rbind(0:length(sample53), round(pbinom(0:length(sample53), size=length(sample53), p=0.5),3))
rbind(0:length(sample53), round(1-pbinom(0:length(sample53)-1, size=length(sample53), p=0.5),3))
CI_l = sort(sample53)[25] 
CI_u = sort(sample53)[42] #
CI = c(CI_l, CI_u) 


