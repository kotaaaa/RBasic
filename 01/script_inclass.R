x <- rnorm(3000); y <-rnorm(3000)
plot(x,y)

#課題1
curve(dnorm(x, mean=-0.2, sd=2.5), from=-5, to=5,col="red")
curve(dnorm(x, mean=1.2, sd=3), from=-5, to=5, col="blue", add=TRUE)
curve(dnorm(x, mean=-2, sd=2.8), from=-5, to=5, col="green", add=TRUE)


#課題2
curve(dnorm(x, mean=0, sd=1), from=-5, to=5, col="green")#標準正規分布
curve(dt(x, 1), from=-5, to=5, col="blue", add=TRUE)#自由分布
curve(dt(x, 2), from=-5, to=5, col="red", add=TRUE)
curve(dt(x, 5), from=-5, to=5, col="orange", add=TRUE)
curve(dt(x, 10), from=-5, to=5, col="purple", add=TRUE)


#課題3
curve(dnorm(x, mean=0, sd=1), from=-5, to=5, col="green")
qnorm(0.025, mean=0, sd=1)
qnorm(0.005, mean=0, sd=1)
qnorm(0.975, mean=0, sd=1)
qnorm(0.995, mean=0, sd=1)

#出力
#Browse[2]> qnorm(0.025, mean=0, sd=1)
#[1] -1.959964
#Browse[2]> qnorm(0.005, mean=0, sd=1)
#[1] -2.575829
#Browse[2]> qnorm(0.975, mean=0, sd=1)
#[1] 1.959964
#Browse[2]> qnorm(0.995, mean=0, sd=1)
#[1] 2.575829


#課題4
curve(dchisq(x, df=10), from=-30, to=30, col="red")
qchisq(0.975, df=10)
qchisq(0.975, df=10, lower=FALSE)



curve(pnorm(x, mean=0, sd=1), from=-5, to=5, col="red")#標準正規分布の累積確率分布
curve(dnorm(x, mean=0, sd=1), from=-5, to=5, col="green", add=TRUE)#標準正規分布
curve(dchiqs(x, df=10, ncp=1), from=-5, to=5, col="green", add=TRUE)#カイ二乗分布


#課題5
samples <- rnorm(100000)
#hist(samples, breaks=seq(-5, 5, length=101))
hist(samples, breaks=seq(-5, 5, length=101),freq=FALSE)
curve(dnorm(x, mean=0, sd=1), from=-5, to=5, col="green", add=TRUE)#標準正規分布


a <- rnorm(100000, mean=-1, sd=1)
b <- rnorm(100000, mean=3, sd=sqrt(2))
add <- a+b
#hist(add, breaks=seq(-5, 5, length=101),freq=FALSE)
hist(add, breaks=seq(-30, 30, length=101),freq=FALSE, col="red")

samples <- rnorm(100000)
hist(samples, breaks=seq(-10, 10, length=101),freq=FALSE)
curve(dnorm(x, mean=2, sd=sqrt(3)), from=-10, to=10, col="blue", add=TRUE)

add <- a - b/2
hist(add, breaks=seq(-10, 10, length=101),freq=FALSE)
curve(dnorm(x, mean=2, sd=sqrt(3)), from=-10, to=10, col="red", add=TRUE)
curve(dnorm(x, mean=1.2, sd=sqrt(3)), from=-10, to=10, col="red", add=TRUE)
curve(dnorm(x, mean=1, sd=sqrt(1/4)), from=-10, to=10, col="red", add=TRUE)


a <- rchisq(10000, dr=3)
b <- rchisq(10000, dr=5)
add <- a+b
hist(add, breaks=seq(-10, 10, length=101),freq=FALSE)






