#6.2
setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 6")

table62 = matrix(c(30, 17, 1067, 1120),2,2)
table62

#a) # relationship between gender and fatalities 
fisher.test(table62)
# p-value = 0.054 > 0.05 => we do not reject hypothesis 

# null hypothesis: no association between gender and infection 
# odds are the same for men and women
# alternative: there is an association and the odds are not equal 


#b)# men fatalities are more often than women
# perform Fisher's exact test
fisher.test(table62, alternative='g')
# p-value=0.03 < 5 => we reject the null hypothesis

# null hypothesis: is that there is no association between gender and infection
# meaning the odds are the same for both gender.
# alternative: there is greater odds of survival for female <=>
# men die more often 


# (30, 17, 1067, 1120)
# hypergeom(2234, 1097, )
# c)
# find the p value from part b with help of phyper
pl=phyper(30,1074,23,47)
pr=1-phyper(30-1, 1074, 23, 47)
c(pl, pr)
# => conclusion: 
# H0 is rejected and we can conclude that there are more men fatalities
# Ho N11 ~ hypergeom(2234,1097,47)
# equivalently in R: phyper(30,1074,23,47)
