#ページ1の上側
n <- 100; m <- 10000; mu <- 0
data <- numeric(m)
for(i in 1:m){
  data[i] <- mean(rnorm(n)) - mu
  
}

hist(data, breaks=seq(-2, 2, length=50))


#ページ1の下側
n <- c(10, 50, 250, 1250); m <- 10000; mu<- 0; sigma <-1/3 
data <- 1:m
op <- par(mfrow=c(2,2))
for(j in 1:4){
  for(i in 1:m){
    #data[i] <- mean(rnorm(n[j])) - mu#正規分布
    #data[i] <- mean(runif(n[j], min=-1.0, max=1.0)) - mu#一様乱数
    data[i] <- (mean(runif(n[j], min=-1.0, max=1.0)) - mu*sqrt(n[j]))/sigma#課題1
  }
  hist(data, breaks=seq(-2, 2, length=50), main=paste("n=", n[j]))
}

#ページ3の上側
n = 10000; rho = 0.3
u <- rnorm(n); v <- rnorm(n)
x <- u 
y <- rho * u +sqrt(1-rho*rho) * v
plot(x, y)
cor(x,y)

#ページ3の下側

n <- 10
x <- rnorm(n)
var(x)
y <- (x-mean(x)) * (x - mean(x))
mean(y) * n/(n-1)
#(x-mean(x)) * (x - mean(x))


#kadai3 #for で回す(不偏分散の分布)
n <- 20; m <- 10000
ue <- numeric(m)
me <- numeric(m)
for (i in 1:m){
  x <- rnorm(n)
  #var(x)
  y <- (x-mean(x)) * (x - mean(x))
  ue[i] <- mean(y) * n/(n-1)
  me[i] <- mean(y)
}
hist(ue, breaks=seq(0, 3, length=50), col="red")
hist(me, breaks=seq(0, 3, length=50), col="blue", add=TRUE)

#runif(min=1, max=3)

#ページ4の下側
n <- 10
x <- rnorm(n)
z <- mean(x) * sqrt(n)
{
if(abs(z)>=0.52)
  print("Rejected")
else
  print("Not Rejected")  
}
rn <- rnorm(100000)
hist(rn, breaks=seq(-5, 5, length=50))
#qnorm(0.3, lower.tail=FALSE)


#ページ5の上側
cnt <- 0; m<-10000
n<-10
for(i in 1:m){
  x <- rnorm(n, mean=1)
  z <- mean(x) *sqrt(n)
  if(abs(z)>=1.96)
    cnt <- cnt +1
}
cnt/m

pnorm(1.96-sqrt(10))

#about p value(5ページ下t分布に従うかどうか)
n <- 10
x <- rnorm(n)
c <- mean(x) *sqrt(n) / sqrt(var(x))
c

qt(0.025,df=n-1)
qt(0.025,df=n-1,lower.tail=FALSE)


#ページ5の上側(t分布に従うことの検証,m回繰り返し)
cnt <- 0; m<-10000
n<-20
theta = qt(0.025,df=n-1,lower.tail=FALSE)
for(i in 1:m){
  x <- rnorm(n)
  z <- mean(x)*sqrt(n)/sqrt(var(x))
  if(abs(z)>=theta)
    cnt <- cnt + 1
}


#ページ6の上側
t <- mean(x) *sqrt(n)/sqrt(var(x))
p <- pt(t, df=9, lower.tail=FALSE)*2
p
t.test(x)

#kadai7
x <- c(-0.655, 1.13, 2.90, -0.339, 1.969, -0.46, 1.74, 1.81)
t <- mean(x) *sqrt(8)/sqrt(var(x))
t
p1 <- pt(t, df=7, lower.tail=FALSE)*2
p1
t.test(x)

#confi <- pt

#kadai7-2
t_par <- qt(0.975, df=7)#qtでt(n-1)(alpha)の値を求める．
confi_up <- mean(x) + t_par*sqrt(var(x)/8)
confi_down <- mean(x) - t_par*sqrt(var(x)/8)
confi_up
confi_down


#kadai8
x <- c(-0.655, 1.13, 2.90, -0.339, 1.969, -0.46, 1.74, 1.81)
t.test(x,conf.level=0.95)#95％信頼区間とp値を表示する
t.test(x,conf.level=0.99)#99％信頼区間とp値を表示する
t.test(x,conf.level=0.95, alternative="greater")#95％信頼区間とp値を表示する
t.test(x,conf.level=0.99, alternative="greater")#99％信頼区間とp値を表示する

#t.test(x,conf.level=0.95, alternative="greater")#alternative="less"で片側検定の逆側

#p値0.03443は，95％信頼区間 0.1192095<μ<Infで信頼区間内にないので，H0を棄却する．
#p値0.03443は，99％信頼区間 -0.406649<μ<Infで信頼区間内にあるので，H0を棄却しない．

#t.test
x <-rnorm(10)
t.test(x)


#kadai1
#m→∞(ここではm = 100000とする)のとき，[-1, 1]における一様分布に対しても，
#サンプル平均と真の平均の差が従う分布は，
#正規分布の標準化の後に，標準正規分布に従うことを示す．

#n <- c(10, 50, 250, 1250); m <- 10000; mu<- 0; sigma <- sqrt(1/3)
n <- c(10, 50, 250, 1250); m <- 100000; mu<- 0; sigma <- sqrt(1/3)
data <- 1:m
data
op <- par(mfrow=c(2,2))
for(j in 1:4){
  for(i in 1:m){
    #data[i] <- mean(rnorm(n[j])) - mu#正規分布
    #data[i] <- mean(runif(n[j], min=-1.0, max=1.0)) - mu#一様乱数
    data[i] <- (mean(runif(n[j], min=-1.0, max=1.0)) - mu)*sqrt(n[j])/sigma#課題1
  }
  hist(data, breaks=seq(-10, 10, length=30), main=paste("n=", n[j]), freq=FALSE)
  curve(dnorm(x, mean=0, sd=1), from=-5, to=5, col="green",add=TRUE)#標準正規分布
}
#x <- (1000)
#curve(dnorm(x, mean=0, sd=1), from=-5, to=5, col="red")
#data


#hist(runif(10, min=-1,max=1))
mu <- 0; sigma <- 1/3
center <- (mean(runif(1000000, min=-1.0, max=1.0)) - mu)*sqrt(1000000)/sigma
hist(center, breaks=seq(-5, 5, length=50))
