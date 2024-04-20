setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 1")
military_per_cap <- read.csv("military-spending-per-capita.csv")
head(military_per_cap)

attach(military_per_cap)

military_per_cap_1988 <- military_per_cap[Year==1988,]
military_per_cap_2020 <- military_per_cap[Year==2020,]

head(military_per_cap_2020)

# in USD
ME1988 <- military_per_cap_1988$military_expenditure_per_capita
ME2020 <- military_per_cap_2020$military_expenditure_per_capita

#############################################################

# a)

# insert your own code here
avg_spending_2020 = mean(ME2020) #average
stdev_spending_2020 = sd(ME2020) #standard deviation
quantiles = quantile(ME2020, probs=c(0.25,0.5,0.75,0.95)) #quantiles
max_spending_2020 = max(ME2020) #maximum
min_spending_2020 = min(ME2020) #minimum

summary(ME2020) #to get all stats in one command
sd(ME2020)
var(ME2020)

#Graphical description:
hist(ME2020, main="Histogram: Spendintures 2020", xlab='countries by index', ylab='expenditure per capita')
boxplot(ME2020, main="Boxplot: Spenditures 2020") #boxplot


#scatter plot with a mean line 
x = seq(1,length(ME2020),1)
plot(x, ME2020, col='blue', main="Scatter plot with mean", xlab='countries by index',ylab='spenditure')
abline(h=avg_spending_2020, col='red')

#############################################################

# b)

index_bi_1988 <- which(military_per_cap_1988$Code %in% military_per_cap_2020$Code)
index_bi_2020 <- which(military_per_cap_2020$Code %in% military_per_cap_1988$Code)

military_bivariate <- cbind(military_per_cap_1988[index_bi_1988,c(1,2,4)], military_per_cap_2020[index_bi_2020,4])
names(military_bivariate)[3:4] <- c("military_expenditure_1988","military_expenditure_2020")

head(military_bivariate)

# insert your own code here
temp = na.omit(military_bivariate) #deleting countries without entries in both years

#numerical:
summary(temp)
correlation = cor(temp$military_expenditure_1988,temp$military_expenditure_2020)

#graphical:
#plotting spenditure 1988 vs 2020
par(mfrow=c(1,1),mar=c(5,4,4,2)+0.1,mgp=c(3,1,0),cex=0.7,las=1)
plot(temp$military_expenditure_1988, temp$military_expenditure_2020, main="Scatter: 1988 vs 2020", xlab='expenditure 1988', ylab='expenditure 2020', col='dark green')

x= seq(1,length(temp$military_expenditure_1988))
data_subset = subset(temp, temp$military_expenditure_1988 > 500) 

plot(x, temp$military_expenditure_1988, col="blue")
points(x, temp$military_expenditure_2020, col='red')
length(x)
length(temp$military_expenditure_1988)

