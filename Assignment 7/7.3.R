#7.3 
setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 7")
source('functions_Ch8.txt')

EC = read.table('expensescrime.txt', header=TRUE)
# response variable
expend = EC$expend
# independent variables
bad = EC$bad
crime = EC$crime
lawyers = EC$lawyers
employ = EC$employ
pop =EC$pop

# perform a regression analysis using expend as response variable 
# and bad, crime, lawyers, employ, and popo as independent variables
summary(lm(expend ~ bad)) #R=0.7
summary(lm(expend ~ crime)) #R=0.11
summary(lm(expend ~ lawyers)) #0.94
summary(lm(expend ~ employ)) #0.95
summary(lm(expend ~ pop)) #0.91


summary(lm(expend ~ employ+bad)) #0.955
summary(lm(expend ~ employ+crime)) #0.955
summary(lm(expend ~ employ+lawyers)) #0.963 &p-value<5%
summary(lm(expend ~ employ+pop)) #0.954

summary(lm(expend ~ employ+lawyers+bad))
summary(lm(expend ~ employ+lawyers+crime))
summary(lm(expend ~ employ+lawyers+pop))
#none of these perform better than employ+lawyers

# Model chosen: expend ~ employ + lawyers 
model = lm(expend~ employ + lawyers)
summary(model)
# R^2=0.963 and RES=232.6  


# Diagnostics 
library(car)
# added variable plot
avPlots(model)



# Analysis should include investigation of:
# a) leverage (potential) and influence points
round(hatvalues(model),3)
round(hatvalues(model),3)>2*3/length(expend) #0.118
2*2/length(expend)
#leverage points: 5, 8

round(cooks.distance(model),2)
round(cooks.distance(model),2) > 1
# influence points: 5, 8 
plot(1:length(expend), cooks.distance(model), main='Influence Points',pch=16, xlab='observations', ylab='cooks distance')
lines(1:length(expend), cooks.distance(model), col='blue')


#removing the influence points from the data set
EC2 = EC #so we do not mess with the original data
toRemove = c(5)
EC2 = EC2[-toRemove, ]
modelC =lm(EC2$expend~ EC2$employ+EC2$lawyers)
summary(modelC) #R^2=0.969

toRemove = c(8)
EC2 = EC
EC2 = EC2[-toRemove, ]
modelC2 =lm(EC2$expend~ EC2$employ+EC2$lawyers)
summary(modelC) #R^2=0.969

EC2 = EC
toRemove = c(5,8)
EC2 = EC2[-toRemove, ]
model2 =lm(EC2$expend~ EC2$employ+EC2$lawyers)
summary(modelC) #R^2=0.972

# a much higher performance when removing the influence points from the set
# best when removing the two influence points but notable improvemnts when only
# removing one 



# b) problems due to multi-collinearity (groups of collinear var)
round(cor(EC[c(2:7)]),2)

# pairwise correlations 
round(cor(EC[c(2,6,5)]),2) # very high between variables
# problem: lawyers and employ are very high correlated 0.97

varianceinflation(EC[,c(6,5)]) #larger than 10 are considered problematic
# VIF: all are greater than 10
conditionindices(EC[,c(6,5)]) # much larger than 30
round(vardecomposition(EC[,c(6,5)]),3)


# A solution might be to drop the variable lawyers. The effect on the model with
# the extra response variable is not that large.
# result when removing lawyers: 0.95

plot(EC2$expend, residuals(lm(EC2$expend~EC2$employ)))


# c) Investigation of residuals
par(mfrow =c(1,2), pty='m')
hist(residuals(model2), main='histogram of residuals')
qqnorm(residuals(model2), main='QQ for Residuals')
qqline(residuals(model2))

# residuals vs. Y-values
plot(EC2$expend, residuals(model2),ylab='residuals', xlab='expend', main='Residuals vs Y', col='blue')
abline(h=0)

