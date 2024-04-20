#7.1
setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 7")
source('functions_Ch8.txt')
sample = read.table('geese.txt', header=T)

photo = sample$photo
obs1 = sample$observer1
obs2 = sample$observer.2


# a) for each observer, graph a scatter plot: photo vs. observer
# is a linear regression model appropiate?
par(mfrow = c(1,1))
pairs(sample)

par(mfrow = c(1,2))
plot(photo, obs1)
plot(photo, obs2)


# b) perform linear regression for each observer
# fit parameteres and 
# test the hypotheis B1 =0 vs. alt: B1=!0
lm1 = lm(photo ~ obs1)
summary(lm1)
# intercept=26.65 and b1=0.88
# F-Test: p-value= 1.54e-14
# Multiple R-squared= 0.75

lm2 = lm(photo ~ obs2)
summary(lm2)
# intercept=16.16 and b1= 0.77
# F-Test: p-value= <2.2e-16
# Multiple R-squared= 0.85



# c) plot the residuals against Y for each model 
# add the line y=0 
# what can we say about the model assumptions?
par(mfrow = c(1,2))
plot(photo,lm1$residuals, ylab='model\'s residuals', col='blue', main='Model 1: Residuals vs. Y')
abline(h=0, col='red')
plot(photo,lm2$residuals, ylab='model\'s residuals', col='darkblue', main='Model 2: Residuals vs. Y')
abline(h=0, col='red')


# d) investigate normality with one or more plots (use lm.norm.test)
# more info at 5.5
#simulate a sample of size 31 from N(o,s^2); denote values by e_1*,...e_31*
#compute y_i*= a +bx + e_i*, with i =1,...,31 with a=hat(a) and b= hat(b)
#determine the least squares estimates a and b for linear regression on y_i* on x_i
#compute test statistic of current test

#RSE = summary(obs1_lm)[[6]] #residual standard error
#D = ks.test(residuals(lm1), pnorm, 0., RSE)[[1]]

set.seed(10101101)
#f(x,y) -> y~x
hist(lm.norm.test(obs1, photo, 1000))
hist(lm.norm.test(obs2, photo, 1000))

# QQ-plot of the residuals
par(mfrow = c(1,2), pty='s')
#qqplot(lm1$residuals,lm2$residuals)
qqnorm(lm1$residuals, col='blue', main='Model 1: Normal QQ')
qqline(lm2$residuals, col='red')
qqnorm(lm2$residuals, col='darkblue', main='Model 2: Normal QQ')
qqline(lm2$residuals, col='red')


# e) repeat a-d but first log-transform the data
# does this transf. stabilize the variance of the error variables?
lphoto = log(photo)
lobs1 = log(obs1)
lobs2 = log(obs2)

#   e.a) scatter plot
par(mfrow = c(1,2))
plot(lphoto, lobs1, col='green', main='Model 3: Scatterplot', ylab='log data1')
plot(lphoto, lobs2, col='darkgreen',  main='Model 4: Scatterplot', ylab='log data2')

#   e.b) fit parameters and test b1=0 at level 5%
lm3 = lm(lphoto ~lobs1)
summary(lm3)
# intercept=0.65 and b1=0.90
# F-test p-value= <2.2e-16
# RSE = 0.87

lm4 = lm(lphoto ~lobs2)
summary(lm4)
# intercept=0.57 and b1=0.87
# F-test p-value= <2.2e-16
# RSE = 0.87

#   e.c) plotting residuals vs. y-values
par(mfrow = c(1,2))
plot(photo,lm3$residuals, ylab='model\'s residuals', col='green', main='Model 3: Residuals vs. Y')
abline(h=0, col='red')
plot(photo,lm4$residuals, ylab='model\'s residuals', col='darkgreen', main='Model 4: Residuals vs. Y')
abline(h=0, col='red')



#   e.d) investigate normality of the error
qqnorm(lm3$residuals, col='green', main='Model 3: Normal QQ')
qqline(lm3$residuals, col='red')
qqnorm(lm4$residuals, col='darkgreen', main='Model 4: Normal QQ')
qqline(lm4$residuals, col='red')

set.seed(10101101)
BS =lm.norm.test(lobs1,lphoto, 1000) #f(x,y) -> y~x
lm.norm.test(lobs2, lphoto, 1000)
par(mfrow = c(1,1), pty='m')
hist(lm.norm.test(lphoto, lobs1, 1000), main='Histogram of the Bootstap Sample')
hist(lm.norm.test(lphoto, lobs2, 1000), main='Histogram of the Bootstap Sample')
# QQ-plot of the residuals
par(mfrow = c(1,1), pty='s')
qqplot(lm3$residuals,lm3$residuals)

# f) compare the 4 models
# which one do we trust the most? why ?
# based on original datal or based in the transformation?

#lets plot the models against the data

par(mfrow = c(1,2), pty='m')
plot(photo, obs1, col='blue', main ='Model 1')
abline(a=26.650, b=.88, col='red')

plot(photo, obs2, col='darkblue',main ='Model 2')
abline(a=16.16, b=.77, col='red')

plot(lphoto, lobs1, col='green',main ='Model 3')
abline(a=0.65, b=.9, col='red')

plot(lphoto, lobs2, col='darkgreen',main ='Model 4')
abline(a=0.57, b=.87, col='red')


help("par")

# g) How well does the photo count reflect the observer counts of the number of geese
