#6.3
setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 6")
source('functions_Ch7.txt')

table63 = read.table('nausea.txt')
table63
# a) which model is most suitable II A, B or C? and why?
#    Given that we already know the fixed number of patients and the number of medicines
#    we choose the model II A. It assumes that the row and column marginals are fixed. 

# b) conduct a hyp-test 
#    when using chi-square check the rule of thumb
chisq.test(table63)$expected # rule of thum is fine
# rule of thumb: at least 5 observations per cell (if not it would not be appropiate)
chisq.test(table63) # p-value=0.07

# c) check for reliability of the p-value of b by using the simulate.p.value of chisq.test
chisq.test(table63, simulate.p.value=T) # p-value=0.07

mean(replicate(100, chisq.test(table63, simulate.p.value=T)[[3]])) #0.071
# not even necessary

# d) compute: 1) contribution
#             2) standardized residuals for the test(s) of b
#    which categories stand out?

residuals = table63 - chisq.test(table63)$expected # residuals
residuals
contribution = chisq.test(table63)$residuals # contribution
contribution

round(chisq.test(table63)$stdres,2)
# which categories stand out?
# more often than expexted under H0: (Chlor, patients), (placebo, nausea), (Dimen, nausea)
# (much) less often than expected under H0: (Chlor, Nausea), (placebo, patient), 
#                                          (dimen, patients)


# e) use bootstrapcat to test the same as b
#    with test statistic: T='largest of the absolute values of the contributions'
#    use the maxcontributionscat function given 
#    note: the test is right-tailed; it rejects for large values of the T

t = max(abs(chisq.test(table63)$residuals))
# simulcat(x) simulates a random contingency table with the same marginals as the 
# contigency table in matrix x
BS = bootstrapcat(table63 ,1000, maxcontributionscat)
mean(BS>=t) #0.055

maxcontributionscat(table63) #1.834

# f) conduct a one-sided Fischer exact test to find out whether Chlorpromazine works better
#    than placebo. 
#    State: 1) null and alternative hypothesis, 2) test statistic, 3) distr. under null.

temp =table63[c(1,2), ]

# testing if they have the same accuracy
fisher.test(temp)#p-value = 0.012 -> reject the hypothesis 

# testing whether chlorpronazine works better than the placebo
fisher.test(temp, alternative='l') #0.007-> reject the hypothesis


