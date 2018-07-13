curve(dchisq(x, 2), from=-10, to=10, col="green")
curve(dchisq(x, 5), from=-10, to=10, col="blue", add=TRUE)
curve(dchisq(x, 7), from=-10, to=10, col="orange", add=TRUE)

samples <- 
hist(samples, breaks=seq(-5, 5,length=101))



kai2 <- function(df) {
  sum(rnorm(df, mean=0, sd=1)**2)
}
#??????2(?????????????????????) 
X <- rnorm(100000, mean=0, sd=1)
kai2 <- function(df) {
  sum(rnorm(df, mean=0, sd=1)**2)
}
df <- 30#df<-30??????????????????
Y <- sapply(c(1:100000), function(x) { kai2(df)} )

T_dtb <- X/sqrt(Y/df)
hist(T_dtb, freq=FALSE, breaks=30, ylim=c(0, 0.55),col="green")
curve(dt(x, df), from=-5, to=5, col="red", add=TRUE)

legend("topleft", legend = c("degree of freedom:30", "theoretical(t_distribution)"), col = c("green", "red"), lty = 1)
#??????2(?????????????????????) 



#??????????????????(?????????2)
df <- 10
x <- sapply(c(1:10000), function(x) { kai2(df)} )
hist(x, freq=FALSE, breaks=30, ylim=c(0, 0.5))
curve(dchisq(x, df), 0, 30, col="red",add=TRUE)

curve(dt(x, df), from=-5, to=5, col="red", add=TRUE)



X <- rnorm(100000, mean=0, sd=1)
kai2 <- function(df) {
  sum(rnorm(df, mean=0, sd=1)**2)
}
df <- 30#df<-30??????????????????
Y <- sapply(c(1:100000), function(x) { kai2(df)} )

T_dtb <- X/sqrt(Y/df)
hist(T_dtb, freq=FALSE, breaks=60, ylim=c(0, 0.55),col="orange")
curve(dt(x, df), from=-5, to=5, col="red", add=TRUE)

hist(Y, freq=FALSE, breaks=60, ylim=c(0, 0.1),col="orange")
curve(dchisq(x, 30), from=10, to=70, col="blue", add=TRUE)









