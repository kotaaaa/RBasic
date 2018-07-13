a <- rnorm(100000, mean=-1, sd=1)
b <- rnorm(100000, mean=3, sd=sqrt(2))
add <- a + b
#samples <- rnorm(100000)
samples <- a + b
#hist(samples, breaks=seq(-10,10,length=101),freq=FALSE)
hist(add, breaks=seq(-10,10,length=101),freq=FALSE)
curve(dnorm(x, mean=2, sd=sqrt(3)), from=-10, to=10,col="red", add=TRUE)


#課題5
add2 <- a + b/2
hist(add2, breaks=seq(-10,10,length=101),freq=FALSE)
curve(dnorm(x, mean=1/2, sd=sqrt(2)), from=-10, to=10,col="green", add=TRUE)


kadai5 <- a + b/2
hist(kadai5, breaks=seq(-10, 10, length=101), freq=FALSE)
curve(dnorm(x, mean=1/2, sd=1.2), from=-10, to=10,col="red", add=TRUE)

d <- rnorm(100000, mean=-1, sd=1)
e <- rnorm(100000, mean=3, sd=sqrt(2))

kadai5_1 <- d - e/2
#dadd <- d - e/2
#hist(dadd, breaks=seq(-10,10,length=101),freq=FALSE)
hist(kadai5_1, breaks=seq(-10,10,length=101),freq=FALSE,col="blue")
curve(dnorm(x, mean=-5/2, sd=sqrt(3/2)), from=-10, to=10,col="red", add=TRUE)


just_add <- d+e
hist(just_add, breaks=seq(-10,10,length=101),freq=FALSE)
curve(dnorm(x, mean=2, sd=sqrt(3)), from=-10, to=10,col="red", add=TRUE)