#7.2
setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 7")
source('functions_Ch8.txt')
ST = read.table('steamtable.txt', header=T)
# we will use the variable Steam as the response variable
# there are other 9 variables in the data frame

# a) use the pair function to plot every possible pair of variables 
# perform the first step of the step-up method 
pairs(ST)
cor(ST)

summary(lm(ST$Steam~ST$Fatty.Acid)) #R^2=0.147
summary(lm(ST$Steam~ST$Glycerine)) #R^2=0.093
summary(lm(ST$Steam~ST$Wind.Mph)) #R^2=0.225
summary(lm(ST$Steam~ST$Calendar.Days)) #R^2=0.019
summary(lm(ST$Steam~ST$Operating.Days)) #R^2=0.287
summary(lm(ST$Steam~ST$Freezing.Days)) #R^2=0.4104
summary(lm(ST$Steam~ST$Temperature)) #R^2=0.714
summary(lm(ST$Steam~ST$Wind2)) #R^2=0.156
summary(lm(ST$Steam~ST$Startups)) #R^2=0.146
# According to the step up method, we would add the variable Temperature to the model
# R^2=0.71


# b) find a suitable multiple linear regression model 
# use diagnostic plots to set up and/ or check the model

#Model 2: Steam ~ Temperature + var.
summary(lm(ST$Steam~ST$Temperature+The ST$Fatty.Acid)) #R^2=0.86 & Pr(>|t|)<5%
summary(lm(ST$Steam~ST$Temperature+ST$Glycerine)) #R^2=0.85 & Pr(>|t|)<5%
summary(lm(ST$Steam~ST$Temperature+ST$Wind.Mph)) #R^2=0.71
summary(lm(ST$Steam~ST$Temperature+ST$Calendar.Days)) #R^2=0.76
summary(lm(ST$Steam~ST$Temperature+ST$Operating.Days)) #R^2=0.85 & Pr(>|t|)<5%
summary(lm(ST$Steam~ST$Temperature+ST$Freezing.Days)) #R^2=0.74
summary(lm(ST$Steam~ST$Temperature+ST$Wind2))#R^2=0.72
summary(lm(ST$Steam~ST$Temperature+ST$Startups))#R^2=0.75
# => 3 options to add to the model: Fatty.Acid, Glycerine and Operating.Days

#Model 3.1: Steam ~ Temp + FattyAcid + var
summary(lm(ST$Steam~ST$Temperature+ST$Fatty.Acid+ST$Glycerine)) #0.86
summary(lm(ST$Steam~ST$Temperature+ST$Fatty.Acid+ST$Wind.Mph)) #0.86
summary(lm(ST$Steam~ST$Temperature+ST$Fatty.Acid+ST$Calendar.Days)) #0.864 but p-value>0.05
summary(lm(ST$Steam~ST$Temperature+ST$Fatty.Acid+ST$Operating.Days)) #0.88 but p-value>0.05
summary(lm(ST$Steam~ST$Temperature+ST$Fatty.Acid+ST$Freezing.Days)) #0.86
summary(lm(ST$Steam~ST$Temperature+ST$Fatty.Acid+ST$Wind2))#0.86
summary(lm(ST$Steam~ST$Temperature+ST$Fatty.Acid+ST$Startups)) #0.87 but p-value>0.05

#Model 3.2: Steam ~ Temp + Glycerine + var
summary(lm(ST$Steam~ST$Temperature+ST$Glycerine+ST$Fatty.Acid)) #0.86
summary(lm(ST$Steam~ST$Temperature+ST$Glycerine+ST$Wind.Mph)) #0.85
summary(lm(ST$Steam~ST$Temperature+ST$Glycerine+ST$Calendar.Days)) #0.86
summary(lm(ST$Steam~ST$Temperature+ST$Glycerine+ST$Operating.Days)) #0.86
summary(lm(ST$Steam~ST$Temperature+ST$Glycerine+ST$Freezing.Days)) #0.85
summary(lm(ST$Steam~ST$Temperature+ST$Glycerine+ST$Wind2))#0.85
summary(lm(ST$Steam~ST$Temperature+ST$Glycerine+ST$Startups)) #0.85

#=> we prefer the model: Steam ~ Temp+ FattyAcid (bit better than Temp+Glycerine)

#Model 3.3: Steam ~ Temp + Operating Days + var
summary(lm(ST$Steam~ST$Temperature+ST$Operating.Days+ST$Fatty.Acid)) #0.88 (p-value<5%)
summary(lm(ST$Steam~ST$Temperature+ST$Operating.Days+ST$Wind.Mph)) #0.86
summary(lm(ST$Steam~ST$Temperature+ST$Operating.Days+ST$Calendar.Days)) #0.885 (p-val<5%)
summary(lm(ST$Steam~ST$Temperature+ST$Operating.Days+ST$Glycerine)) #0.86
summary(lm(ST$Steam~ST$Temperature+ST$Operating.Days+ST$Freezing.Days)) #0.86
summary(lm(ST$Steam~ST$Temperature+ST$Operating.Days+ST$Wind2))#0.86
summary(lm(ST$Steam~ST$Temperature+ST$Operating.Days+ST$Startups)) #0.85

#model 4: Steam ~ temperature + operating days + calendar Days + var
summary(lm(ST$Steam~ST$Temperature+ST$Operating.Days+ST$Calendar.Days+ST$Fatty.Acid))#0.893
summary(lm(ST$Steam~ST$Temperature+ST$Operating.Days+ST$Calendar.Days+ST$Wind.Mph)) 
summary(lm(ST$Steam~ST$Temperature+ST$Operating.Days+ST$Calendar.Days+ST$Glycerine))
summary(lm(ST$Steam~ST$Temperature+ST$Operating.Days+ST$Calendar.Days+ST$Freezing.Days))
summary(lm(ST$Steam~ST$Temperature+ST$Operating.Days+ST$Calendar.Days+ST$Wind2))
summary(lm(ST$Steam~ST$Temperature+ST$Operating.Days+ST$Calendar.Days+ST$Startups))

# we do not add any variable because none pass the t-test
# therefore our final model using the step up method is:
# lm(ST$Steam~ST$Temperature+ST$Operating.Days+ST$Fatty.Acid)
model = lm(ST$Steam~ST$Temperature+ST$Operating.Days+ST$Calendar.Days)
model2 = lm(ST$Steam~ST$Temperature+ST$Operating.Days)
summary(model)



# Diagnostics Plots 
# Added Variable Plot
library(car)
avPlots(lm(ST$Steam~ST$Temperature+ST$Operating.Days+ST$Calendar.Days))
avPlots(model)
help(avPlots)
# to visualize the relationship between the response variable 'steam' and each 
# individual predictor

#Scatter plots Y and X_k
par(mfrow=c(2,2))
plot(ST$Steam, ST$Temperature)
plot(ST$Steam, ST$Operating.Days)
plot(ST$Steam, ST$Calendar.Days)

#plot residuals agains Y
par(mfrow=c(1,2), pty='m')
plot(ST$Steam, residuals(model2), col='red', main='Residuals vs. Y (without Calendar days)', xlab='residuals', ylab='steam')
abline(h=0)

#par(mfrow=c(1,1), pty='m')
plot(ST$Steam, residuals(model), col='brown', main='Residuals vs. Y (with calendar days)', xlab='residuals', ylab='steam')
abline(h=0)

# c) check in b for possible influence points and collinearity
# if influence points, fit the model of b without these points

# leverage points
# 2(3+1)/25 = 0.32 
round(hatvalues(model),3)
round(hatvalues(model),3)>2*4/25
#greater than 2*4/25 => (7,0.46), (14,0.493) (19,0.46)

# influence points
par(mfrow=c(1,1))
round(cooks.distance(model), 2)
plot(1:length(ST$Steam), cooks.distance(model), main='Influence Points', xlab='observations', ylab='cooks distance')
lines(1:length(ST$Steam), cooks.distance(model), col='blue')

# if we consider the threshold 1, then there are not any influence points in the data
# however, when the threshold is 4/n (which is also commonly used), then we get
# one influence point -> obs #14 with 0.21 > 0.16=4/n

# remove point 14 from the data set
ST2 = ST #so we do not mess with the original data
toRemove = c(14)
ST2 = ST2[-toRemove, ]
modelC =lm(ST2$Steam~ ST2$Temperature+ST2$Operating.Days+ST2$Calendar.Days)
summary(modelC) #R^2=0.89, RSE=0.59

# Collinearity:
respVar = ST[, c(8,6,5)]
# scatter plots
pairs(respVar, main='Scatterplots of Response Variables')
# we reject collinearity amongst the response variables

#correlation
cor(respVar) # no indication of high correlation 

#Condition indices
conditionindices(respVar)
# not larger than 30



# d) investigate the residuals of the selected model for normality 

par(mfrow = c(1,2), pty='s')
hist(residuals(model), col='lightblue')
qqnorm(residuals(model))
qqline(residuals(model))

shapiro.test(residuals(model))
ks.test(residuals(model))

# we cannot ensure that the normality assumption is met
# however, there are some indices of normality in the error


par(mfrow = c(2,2), pty='s')
hist(residuals(model), col='lightblue')
qqnorm(residuals(model))
qqline(residuals(model))
hist(residuals(model2), col='blue')
qqnorm(residuals(model2))
qqline(residuals(model2))
# testing withou the calendar days variable: better results regarding the 
# normality assumption


# e) is the selected model appropiate for the data?
# connection between response and predictor variables in words
# does it makes sense? is it intuitive? 

Y_pred = -2.97-0.07*(ST$Temperature)+0.19*(ST$Operating.Days)+0.4*(ST$Calendar.Days)

par(mfrow =c(1,1), pty='m')
plot(ST$Steam, Y_pred,pch=16, col='darkgreen', main='Results of the Model', xlab='Steam', ylab='Y predicted')
abline(a = 0, b = 1, col = "red")


# appropiate because: normality of errors seems reasonable, the R^2 is high and the
# results are looking good
# plot



####################################################################################
# not yet revised answer:
# The selected model is appropiate for the data. We have ensure that the model assumptions
# are met, which are linearity, homoscedasticity and normaility of residuals. 
# Diagnostics plots were plotted and carefully analyzed to give strong evidence of 
# the functionality and structure of the model as well as its results and errors. 

# Furthermore, the model is very intuitive. This is because 88% of the model can be
# explain from the response variable alone and that is pretty high. Also because the 
# known relationship between response variables and the predictor variable matches 
# the results of the model. In other words, it makes sense that the temperature is a 
# (most) crucial response variable in relation of steam usage. We can and should say
# similar things about the other variables (in other magnitudes).

