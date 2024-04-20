CLT_unif <- function(n, m) {
  samples = replicate(n, runif(m))
  means = colMeans(samples)
  
  hist(means, main=paste("Histogram: Sample Means with n=", n, ", m=", m, sep=""),
       xlab="Sample Mean", prob=TRUE)
  
  x = seq(min(means), max(means), length.out=100)
  y = dnorm(x, mean=0.5, sd=1/sqrt(12*m))
  
  lines(x, y, type="l", col="red", lwd=2)
}


par(mfrow=c(2,2))
CLT_unif(50,30)
CLT_unif(50,200)
CLT_unif(300,30)
CLT_unif(300,200)

par(mfrow=c(1,1))
