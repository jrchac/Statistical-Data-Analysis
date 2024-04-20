norm <- function(n, mu, signma){
  set.seed(2023210)

  samples = rnorm(n, mu, signma)[1:n]

  quants = quantile(samples,probs= c(0.05,0.5,0.95), type=7)
  loc = mean(samples)
  spread = sd(samples)
  stud_no = c(2699643)
  myList = list(quants, loc, spread, stud_no)
  save(myList, file="C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 1\\myfile1.RData")
}


norm(100, 1, 1)

set.seed(2023210)
x <- rnorm(100, mean=1, sd=1)
mylist_compare <- list(quants = quantile(x, probs=c(0.05, 0.5,0.95)), loc = mean(x), spread = sd(x))
