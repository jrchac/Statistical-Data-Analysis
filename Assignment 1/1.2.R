set.seed(1234)
x = rnorm(50, 0, sqrt(2))
y = rnorm(50)
mean(x)
sd(x)
var(x)
cor(x,y)
x[x<0]
sum(x<0)
hist(x, prob=T)
help(hist)
?hist
f <- function(x){x*x}
u = seq(-5,5,0.1)
v = dnorm(u, 0, sqrt(2))
lines(u,v)
{hist(x,xlim=c(-6,6),prob=T) + lines(u,v)}
plot(sort(x),1:50/50, type(s), ylim=c(0,1), xlab="x", ylab="the ecdf of x")
lines(u, pnorm(u,0, sqrt(2)))
w = seq(-pi, pi, length=100)
plot(cos(w),sin(w), type="1")
