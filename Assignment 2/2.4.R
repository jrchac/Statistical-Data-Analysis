#2.4
setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 2")
source("functions_Ch3.txt")

#ankle circumsference (in cm, column 20)
#weight: in kg, col 23
#height: in cm, col 24
#BMI = weight/(height in meter)^2
#data points: rows 248 to 507

#a) obtain the BMIs
#create histograms and boxplots
#check if both info come from same distr.
table = read.table("body.dat.txt", header=FALSE)


df = table[248:507, c(20, 23, 24)]
names(df)[c(1,2,3)] = c("a_cm","weight_kg","height_cm") #a_cm states for ankle circumference in cm
df$BMI = df$weight_kg/((df$height_cm/100)^2)


par(mfrow=c(1,2))
hist(df$a_cm, xlab='ankle in cm', main="Histogram: Ankle Circumsference", col='blue') #looks normal distributed
hist(df$BMI, xlab= 'BMI', main="Histogram: BMI", col='skyblue') #is bell-shaped left-skewed (can also be normal)
par(mfrow=c(1,2))
boxplot(df$a_cm, main="Boxplot: Ankle circumsference", col='blue')
boxplot(df$BM, main="Boxplot: BMI", col='skyblue')

par(mfrow=c(1,1))
boxplot(df$a_cm, df$BMI,main='Boxplot: Ankle cm vs BMI', names= c("ankle cm","BMI"), col=c('blue', 'skyblue'))

#b) are they from the same scale-location family? 
# use qqplot
x = seq(0.00, 1, 0.01)

par(mfrow=c(1,1),pty='s')
q_ankle= quantile(df$a_cm, probs=x)
q_BMI= quantile(df$BMI, probs=x)
plot(q_ankle, q_BMI,col='darkblue', xlab='ankle in cm', ylab='BMI', main="QQ plot: Ankle cm vs. BMI")
abline(-20.27,2, col='red')


par(mfrow=c(1,1),pty='s')
qqplot(df$a_cm, df$BMI, xlab='ankle in cm', ylab='BMI', main="QQ plot: Ankle cm vs. BMI")
abline(-20.,2, col='red')
#conclusion: probably from the same location-scale family 



#c) discuss and conclude if the 2 distr can be modeled as normal distributions
#or even some chi-square
# not required to identify the scale and location parameter
par(mfrow=c(2,2), pty='s')
qqnorm(df$a_cm, main="Ankle Normal QQ-Plot", col='blue')
qqline(df$a_cm)
qqnorm(df$BMI,main="BMI Normal QQ-Plot", col='dark green')
qqline(df$BMI)

shapiro.test(df$a_cm)
shapiro.test(df$BMI)

degree=18
#par(mfrow=c(1,2))
qqchisq(df$a_cm,df=degree, col='blue', main="Ankle Chi-squared QQ-Plot")
qqchisq(df$BMI,df=degree, col='dark green', main="BMI Chi-squared QQ-Plot")

par(mfrow=c(1,1))
plot(ecdf(df$a_cm) ,col = "red", xlim = c(0, 37), ylim = c(0, 1), cex=0.5)
plot(ecdf(df$BMI), col = "blue", add = TRUE, cex=0.5)
curve(pchisq(x, df = degree), lwd = 2, add = TRUE, from = 0, to = 50)
#conclussion: normaility is possible (when not taking into account hypothesis tests)
#however, chisquare distribution is more palpable. the QQ-plots (comparing with each other, as well comparing with the theoretical values coincide)


#d) investigate the normality of the BMI/ankle^2 (without tests)
#create new column
df$ratio = df$BMI/(df$a_cm^2)

par(mfrow=c(1,3))
hist(df$ratio, breaks=40,xlab='ratio', main='Histogram: new ratio', col='lightyellow')
boxplot(df$ratio, main='Boxplot: new ratio', xlab='new ratio', col='brown') #there is a symmetric distribution and two upper outliers
qqnorm(df$ratio, col='blue')
qqline(df$ratio, col='red')
#high indications of normality 

#e) use shapiro wilk test with alpha=0.05 to test normality of d
shapiro.test(df$ratio)
# W=0.9657
# p-value = 7.22e-6f
# result= we reject the Hypothesis or normality

#f) compare goodness-of-fitness for normality for full sample vs first 50 points
#create histograms of data to compare results
#explain the findings
df50 = df[1:50,]
df50

shapiro.test(df$ratio)
shapiro.test(df50$ratio)
#in this case, we fail to reject hypothesis of the normality

par(mfrow=c(1,2))

hist(df$ratio,breaks=20,xlab='ratio', main='Histogram: full sample', col='lightblue')
hist(df50$ratio,breaks=7,xlab='ratio', main='Histogram: 50 data points', col='lightblue')

par(mfrow=c(1,1))
boxplot(df$ratio, df50$ratio)
boxplot(df50$ratio)

#explanation 

