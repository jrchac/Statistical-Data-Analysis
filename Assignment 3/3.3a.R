#3.3 second attempt
setwd("C:\\Users\\josec\\OneDrive\\Documentos\\VU\\Statisitcal Data Analysis\\Assignment 3")
source("functions_Ch5.txt")
source("functions_Ch4.txt")
sample33 =scan("sample33.txt")

library(logKDE)

###################################################################
sample33 =scan("sample33.txt")
par(mfrow=c(1,1))
hist(sample33, main="Histogram: Sample33", xlab='values', col='lightblue')

y=log(sample33)
yrange = seq(min(y), max(y), length.out=512)

#plot of "wrong" KDE 
par(mfrow=c(1,2))
plot(density(sample33), main="KDE without transformation", lwd=2, col=rainbow(7)[6])
plot(density(y), main='KDE of the log(sample)',lwd=2,col=rainbow(7)[5])


help("logdensity")

#plot of correct KDE
plot(exp(yrange), density(y, bw='nrd', from=min(yrange), to=max(yrange))$y/exp(yrange), type='l',
     main='KDE using hint code',lwd=2, col='darkgreen', ylab='Density', xlab='N = 100 Bandwidth = 0.2916')

par(mfrow=c(1,2))
plot(logdensity(sample33), lwd=3,col='darkblue', main="KDE using logdensity")
plot(logdensity(sample33, bw=0.95), lwd=3,col='blue', main="KDE using logdensity")

#plot of correct KDE usign fast fourier transformation
plot(logdensity_fft(sample33), lwd=3,col=rainbow(7)[5])


#lognormal using lognormal bandwidths
plot(logdensity(sample33, bw = bw.logG(sample33)), lwd=3,col='pink')
plot(logdensity(sample33, bw = bw.logCV(sample33)), lwd=3,col='purple')


#optimal estimators
par(mfrow=c(1,2))
plot(logdensity(sample33, bw='nrd'), main='Gaussian KDE', lwd=2, col="#8FB1C7")
plot(logdensity(sample33, bw='SJ-dpi'), main='Gaussian KDE', lwd=2, col="#000066")


#bandwidths
par(mfrow=c(2,3))
#bw_list = list('nrd', 'nrd0', 'ucv','bcv', 'sj')
bw_list = list('nrd', 'nrd0', 'ucv','bcv','SJ-ste', 'SJ-dpi')
col_list = list("#FFFFFF","#8FB1C7" ,"#5A85AC" ,"#2E5E92", "#143C78", "#000066")

bw_list2= list('nrd', 'nrd0')
col_list2 = list("#143C78", "#000066")
for (b in bw_list) {
  plot(logdensity(sample33, bw=b), lwd=2, 
       main=sprintf("Kernel: Gaussian Bandwidth used: %s",b), col="#5A85AC")
}



#trying different kernels
kernel_list = list(
#logKDE.g = logdensity_fft(sample33, kernel = 'gaussian'),
logKDE.e = logdensity_fft(sample33, kernel = 'epanechnikov'),
logKDE.t = logdensity_fft(sample33, kernel = 'triangular'),
logKDE.u = logdensity_fft(sample33, kernel = 'uniform'),
logKDE.l = logdensity_fft(sample33, kernel = 'logistic'),
logKDE.lp = logdensity_fft(sample33, kernel = 'laplace'))

#plotting everything in one graph (overwhelming )
par(mfrow=c(1,1))
plot(logdensity(sample33), lwd=3 ,col='black') #base
i=1
for (estimate in kernel_list){
  lines(estimate$x, estimate$y, col=rainbow(7)[i])
  i=i+1
}

#recommendation for the different kernels:
#maybe create 2 plots with 3 kernels each
