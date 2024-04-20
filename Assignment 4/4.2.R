#4.2
setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 4")
#source("functions_Ch3.txt")
source("functions_Ch5.txt")
source("thromboglobulin.txt")
sampleSDRP = thromboglobulin$SDRP
samplePRRP = thromboglobulin$PRRP
sampleCTRP = thromboglobulin$CTRP

SDRPmean = mean(sampleSDPR) #61.93
SDRPmedian = median(sampleSDPR) #49.5
CTRPmean = mean(sampleCTRP) #75.11
CTRPmedian = median (sampleCTRP) #62.5

hist(sampleSDRP, breaks=10) #right-skewed with some outliers
boxplot(sampleSDRP) #not symmetric, some outliers

#a) determine a two-sided 95%-CI for the median of SDRP
BS_median = bootstrap(sampleSDRP, median, B=2000)
CI_median = quantile(BS_median, c(0.025, 0.975)) #[43, 65]


q_lower= quantile(BS_median, 0.025)
q_upper=quantile(BS_median, 1-0.025)
CI = c(2*mean(BS_median) - q_upper, 2*mean(BS_median) - q_lower) #[43,65]

#b) repeat a) with the mean of the distribution
BS_mean = bootstrap(sampleSDPR, mean, B=2000)
CI_mean =quantile(BS_mean, c(0.025, 0.975)) #[50.2, 75.5]


#c) which is a better location estimator?
#the narrower the CI, the better location estimation (better accuracy/ precision)
diff_mean = CI_mean[2]-CI_mean[1] #25.4
diff_median = CI_median[2] - CI_median[1] #22 -> better estimator

var(BS_mean) #41.58
var(BS_median) #31.42
#better: median narrower CI and lower variance in the bootstrap samples 


#d) determine a 90%-CI for diff median between SDRP and CTRP
# z= 1.645
BS_median2 = bootstrap(sampleCTRP, median, B=2000)
CI_median2 = quantile(BS_median2, c(0.05, 0.95)) 
CI_median2 #[53, 72]

#bootstrap for the difference of the median of SDPR and CTRP
diff_medians = BS_median2 - BS_median
CI_diff = quantile(diff_medians, c(0.05, 0.95))
CI_diff #[-1, 26]
                   
#CI = mean(x) - z*sd(x)/sqrt(n)
CI = c(mean(sampleSDPR) - 1.96*sd(sampleSDPR)/sqrt(length(sampleSDPR)), mean(sampleSDPR) + 1.86*sd(sampleSDPR)/sqrt(length(sampleSDPR)))
CI #[49.06, 74.14] vs using-quantile [50.5, 75.05]

#formula for the BS diff of median
BS_diffMedian = function(sample1, sample2, B){
  BS_sample = list()
  for(i in 1:1000){
    sample_temp = median(sample(samplePRRP, size=length(samplePRRP), replace=TRUE))
    sample_temp2 = median(sample(sampleCTRP, size=length(sampleCTRP), replace=TRUE))
    diff = sample_temp -sample_temp2
    BS_sample[i] = diff
  }
  return (BS_sample)
}


#############################################################################################
mean_CI <- function(x, alpha = 0.05) {
  n <- length(x)
  mean_x <- mean(x)
  se_x <- sd(x) / sqrt(n)
  z_crit <- abs(qnorm(alpha / 2, lower.tail = FALSE))
  ME <- z_crit * se_x
  lower_CI <- mean_x - ME
  upper_CI <- mean_x + ME
  return(c(lower_CI, upper_CI))
}


median_CI <- function(x, alpha = 0.05) {
  n <- length(x)
  med_x <- median(x)
  se_x <- 1.253 * sd(x) / sqrt(n)
  z_crit <- abs(qnorm(alpha / 2, lower.tail = FALSE))
  ME <- z_crit * se_x
  lower_CI <- med_x - ME
  upper_CI <- med_x + ME
  return(c(lower_CI, upper_CI))
}
#[Tn − Z∗([(1−α)B]), Tn − Z∗([αB]+1)] = [2Tn − (T^∗)n,([(1−α)B]), 2Tn − (T^∗)n,([αB]+1) .

                                        
                                        