#5.1
setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 5")
source('functions_Ch5.txt')

sample51 = scan('statgrades.txt')
mean(sample51) #5.536
median(sample51) #5.69

reject_hypothesis = function(pvalue, alpha) {
  if (pvalue <= alpha) {
    return(TRUE)  # Reject the null hypothesis
  } else {
    return(FALSE)  # Fail to reject the null hypothesis
  }
}

#a) test Ho: m>6 vs. Ha: m<6.0 at alpha=10%
#m is the unkown median of the unknown true grades distribution
alpha = 0.1

#wilcoxon signed rank test is the best option for testing the median 
E1.a.pval = wilcox.test(sample51, mu=6, alternative='less')[[3]] #0.02
E1.a.reject = reject_hypothesis(E1.a.pval,alpha) #TRUE


#b) test Ho: m=5.5 vs. Ha: m=!5.5 at alpha=1%
alpha2 = 0.01
E1.b.pval = wilcox.test(sample51, mu=5.5)[[3]] #p-val=.65
E1.b.reject = reject_hypothesis(E1.b.pval, alpha2) #False


#c) denote by p the prob to get a grade of at least 5.5 
# test Ho: p<=40% vs. Ha: p>40% at alpha=5%

E1.c.pval = binom.test(sum(sample51>=5.5), n=length(sample51), p=0.40, alternative='g')[[3]]
#0.008
E1.c.reject = reject_hypothesis(E1.c.pval , 0.05) #TRUE

