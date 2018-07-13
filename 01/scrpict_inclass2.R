a <- rnorm(100000, mean=-1, sd=1)
b <- rnorm(100000, mean=3, sd=sqrt(2))
add <- a+b
hist(a, breaks=seq(-10, 10, length=101),freq=FALSE, col="orange")
hist(b, breaks=seq(-10, 10, length=101),freq=FALSE, col="blue", add=TRUE)
hist(add, breaks=seq(-20, 30, length=101),freq=FALSE,col="red",add=TRUE)

samples <- rnorm(100000)
hist(samples, breaks=seq(-10, 10, length=101), freq=FALSE)
curve(dnorm(x, mean=2, sd=sqrt(3)), from=-10, to=10, col="red", add=TRUE)
