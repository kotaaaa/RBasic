x <- c(1,2,3,4,5)
x
y <- c(1:5,3:1)
y
z <- c(rep(3,4),rep(c(1,5,10),c(2,3,4)))
z
a <- c("A","B","C")
a
mean(1:5)
a <- c(1,2,3)
a
1:5
seq(0,10,by=2)
seq(0, 10, length=5)
c(5,5,5,5) -c(1,2,3,4)

c(5,5,5,5)- c(1,2)
c(5,5,5,5)- 1
y <- c(2,3,4,5,6)
y - x
com <- c(1,2,Inf, 4,5)
mean(com)


curve(sin(x*x), from=0, to=5)
dev.copy2eps(file="filename2.eps", width=6)

pdf()
curve(sin(x*x), from=0, to=5)
dev.off

a[3] <- 10
a
a <- 1:5
b <-replace(a, c(2,4), c(-2,-4))
b
d <- replace(a,c(2,4),NA)
d
replace(d, which(is.na(d)), 0)
mean(d)
d
e <- d[!is.na(d)]
e
mean(e)


n <- 10
sum <- 0
for(i in 1:n)
  sum <- sum +i
print(sum)



myabs <- function(x){
  if(x >= 0)
    return(x)
  else 
    return(-x)
}

myabs(-5)


png()
curve(sin(x**x), from=0, to=5)
dev.off()

x<-1:10
plot(x)
x <- rnorm(3000); y <-rnorm(3000)
plot(x,y)

