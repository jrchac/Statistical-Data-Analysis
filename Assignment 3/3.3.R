#3.3 
#find suitable kernel density estimate
#hint: assign no mass to ^f(x) for x<0 (positive sample)

setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 3")
source("functions_Ch5.txt")
source("functions_Ch4.txt")
sample33 =scan("sample33.txt")

par(mfrow=c(1,1))
hist(sample33, breaks=15) # right-skewed, bell shape

#Steps:
#1)we transform the data into a log(sample), so no values are assign for x<0
y= log(sample33)
yrange = seq(min(y), max(y), length.out=512)

lines(exp(yrange), density(y, from=min(yrange), to=max(yrange))$y/exp(yrange), col='red')

#2) derive KDE
par(mfrow=c(1,1))
par(mfrow=c(2,2))
density_log = density(y, kernel='epanechnikov', bw='nrd')
#density_log1 = density(y, kernel='gaussian', bw='nrd')
#density_log2= density(y, kernel='epanechnikov', bw='bcv')
#density_log3 = density(y, kernel='gaussian', bw='bcv')


#3) transform back 
# Thus, fy(t) = fx(exp(t)) · exp(t) for their densities.

density_sample = list(x=exp(density_log$x), y=exp(density_log$y))
#density_sample1 = list(x=exp(density_log$x), y=exp(density_log$y)) 
#density_sample2 = list(x=exp(density_log$x), y=exp(density_log$y)) 
#density_sample3 = list(x=exp(density_log$x), y=exp(density_log$y)) 

#par(mfrow=c(1,1))
plot(density_sample, type='l', lwd=2, col='darkgreen')
#plot(density_sample1, type='l', lwd=2)
#plot(density_sample2, type='l', lwd=2, col='darkgreen')
#plot(density_sample3, type='l', lwd=2)



#2nd approach: using the code in the hint
par(mfrow=c(2,3))
plot(density(sample33), lwd=2, col='red')
plot(exp(yrange), density(y, bw='nrd', from=min(yrange), to=max(yrange))$y/exp(yrange), type='l', lwd=2)
plot(exp(yrange), density(y, bw='nrd0', from=min(yrange), to=max(yrange))$y/exp(yrange), type='l', lwd=2)
plot(exp(yrange), density(y, bw='bcv', from=min(yrange), to=max(yrange))$y/exp(yrange), type='l', lwd=2)
plot(exp(yrange), density(y, bw='ucv', from=min(yrange), to=max(yrange))$y/exp(yrange), type='l', lwd=2)
plot(exp(yrange), density(y, bw='sj', from=min(yrange), to=max(yrange))$y/exp(yrange), type='l', lwd=2)


#through symmetrization
sym_sample= c(-rev(sample33), sample33)
plot(density(sym_sample))



sample_data <- read.table("sample33.txt", header = FALSE)

# take the log transformation of the sample
log_sample <- log(sample_data$V1)

# set up the plot window
plot(NULL, xlim=c(0, max(sample_data$V1)), ylim=c(0, 0.6), xlab="x", ylab="Density")

# compute the kernel density estimate for the log-transformed sample
log_kde <- density(log_sample, kernel="gaussian", bw="nrd0")

# plot the density estimate of the log-transformed sample
lines(exp(log_kde$x), log_kde$y/exp(log_kde$x), col="red")

# compute the density estimate for the original sample using the log-transformed KDE
yrange <- seq(min(log_sample), max(log_sample), length.out=512)
fx <- density(log_sample, kernel="gaussian", bw="nrd0", from=min(yrange), to=max(yrange))
lines(fx$x, fx$y, col="blue")

fx <- density(log_sample, kernel="gaussian", bw="nrd0", from=min(yrange), to=max(yrange))
plot(NULL, xlim=c(0, max(fx$x)), ylim=c(0, 0.63), xlab="x", ylab="Density")
lines(fx$x, fx$y, col="blue")

fx2 <- density(log_sample, kernel="gaussian", bw="nrd", from=min(yrange), to=max(yrange))
plot(fx2)

