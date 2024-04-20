setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 6")
source('functions_Ch7.txt')

table = read.table('nausea.txt')
cont_table = cbind(table, table[,1]-table[,2])
colnames(cont_table)[3] = 'No Incidence'
cont_table = cont_table[,c(2,3)]
cont_table


# a) which model is most suitable II A, B or C? and why?
# We consider that the most suitable model for the data given is the model II A. 

# b) conduct a hyp-test + when using chi-square check the rule of thumb
chisq.test(cont_table)$expected # rule of thumb is fine
# rule of thumb: at least 5 observations per cell (if not it would not be appropiate)
chisq.test(cont_table) # p-value=6.32e-5


# c) check for reliability of the p-value of b by using the simulate.p.value of chisq.test
chisq.test(cont_table, simulate.p.value=T) #p-value=0.001



# d) compute: 1) contribution
#             2) standardized residuals for the test(s) of b
residuals = cont_table - chisq.test(cont_table)$expected # residuals
residuals
contribution = chisq.test(cont_table)$residuals # contribution
contribution
# which categories stand out?
# Largest contributions:
# much more often: (no, chlor), (yes, placebo), (yes, dimen)
# much more less: (yes, chlor), (no, placebo), (no, dimen)

# normalized residuals
round(chisq.test(cont_table)$stdres,2)

# e) use bootstrapcat to test the same as b
#    with test statistic: T='largest of the absolute values of the contributions'
#    note: the test is right-tailed; it rejects for large values of the T

t = max(abs(chisq.test(cont_table)$residuals))

BS = bootstrapcat(cont_table ,1000, maxcontributionscat)
mean(BS>=t2) #0.002

maxcontributionscat(cont_table) #2.59

# f) conduct a one-sided Fischer exact test to find out whether Chlorpromazine works better
#    than placebo. 
#    State: 1) null and alternative hypothesis, 2) test statistic, 3) distr. under null.

cont_table[c(1,2), ]
temp = matrix(c(95,52,70,100),2,2)

# testing if they have the same accuracy
fisher.test(temp) #p-value < signficance level

# testing whether chlorpronazine works better than the placebo
fisher.test(temp, alternative='g') #p-value > significance level

# re formulating the alternative in terms of N_11
