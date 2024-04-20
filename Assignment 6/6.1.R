#6.1
setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 6")

expenses_crime = read.table('expensescrime.txt', header=TRUE)
sample61 = subset(expenses_crime, pop*1000 > 3000000)


#a) make plots for every pair of variables (use pairs)
numSample = subset(sample61, select=c('expend', 'bad', 'crime', 'lawyers', 'employ', 'pop'))
pairs(numSample)

#b) make a plot of the expend rate (state exp. in the state per citizen, versus crime rate)
#based on the plot, how would you judge the correlation between crime and expend rate

plot(sample61$expend, sample61$crime)
cor(sample61$expend, sample61$crime) #0.3626


################################################################################################
#Assignment Questions:

#c) test with Kendalls and Spearmans rank correlation if the crime and expend rate are dependent

#Kendalls
cor.test(sample61$expend, sample61$crime, method='k')
#p-value = 0.037 vs alpha=0.05 -> we reject the hypothesis of independency
#tau = 0.28

#Spearmans
cor.test(sample61$expend, sample61$crime, method='s')
#p-value = 0.032 vs alpha=0.05 -> we reject the hypothesis of independency
#rho = 0.4078
cor.test(sample61$expend, sample61$crime, method='s')$statistic

#d) read 6.4 and example 6.7
# perform a permutation test for testing dependencies between crime and expend rate (6.4.3)
# based on Spearman's rank correlation coefficient. Alpha=5%

hist(sample(sample61$expend, 1000, replace=TRUE))

B=1000
t = cor.test(sample61$expend, sample61$crime, method='s')[[1]]
t_perm = numeric(B)
twoSamples = c(sample61$expend, sample61$crime)
for (i in 1:B){
  sample1 = sample(twoSamples, size=length(sample61$expend))
  sample2 = sample(twoSamples, size=length(sample61$expend))
  t_perm[i] =  cor.test(sample1, sample2, method='s')[[1]]
}
pl = mean(t_perm < t)
pr = mean(t_perm > t)
p = 2 *min(pl,pr) #0.034
# Just as in the previous tests, we reject the hypothesis p-value < alpha

# Alternatively, we can sample only one time and divide the sample in two


#e)
# use simulations to approx. the A.R.E. of Kendals with respect to Spearmans
# when the data pairs are bivariate t-distr. with scale E = (1 0.2, 0.2 1) and df=6
library(mvtnorm)

areSimulation = function (B, n){
  are=0
  pvalK = pvalSp = numeric(B)
  for (i in 1:B){
    x = rmvt(n, sigma=matrix(c(1,0.2,0.2,1), 2, 2), df=6)
    pvalK[i] = cor(x, method='k')[[3]]
    pvalSp[i] = cor(x, method='s')[[3]]
  }
  powerK = mean(pvalK < 0.05)
  powerSp = mean(pvalSp < 0.05)
  rbind(c("Kendals", 'Spearmans'), c(powerK, powerSp) )
  are = (powerK/powerSp)^2
  return (are)
}


areSimulation(1000, length(sample61$crime)) #aprox = 1.27
# note: this will vary due to randomness


###############################################################
B=1000
t2 = cor.test(sample61$expend, sample61$crime, method='s')[[1]]
t_perm2 = numeric(B)
twoSamples2 = c(sample61$expend, sample61$crime)
for (i in 1:B){
  sample0 = sample(twoSamples2, size=length(twoSamples2))
  sample12 = sample(sample0, length(sample61$crime))
  sample22 = sample(setdiff(sample0, sample12), size=length(sample61$expend))
  t_perm2[i] =  cor.test(sample12, sample22, method='s')[[1]]
}
pl2 = mean(t_perm2 < t2)
pr2 = mean(t_perm2 > t2)
p2 = 2 *min(pl2,pr2) #0.04
p2