#1ページ
curve(df(x, df1=10, df2=15),from=0, to=10)
curve(df(x, df1=9, df2=14),from=0, to=10, col='orange', add=TRUE)

qf(0.025, df1=9, df2=14)
qf(0.025, df1=9, df2=14, lower.tail = FALSE)


m <- 10; m <- 15
data1 <- rnorm(m, mean=0, sd=1);data2 <- rnorm(n, mean=0, sd=2)
var1 <- var(data1); var2 <- var(data2)
ratio <- var1/var2;
lower <- qf(0.025, df1=m-1,df2=n-1)
upper <- qf(0.025, df1=m-1,df2=n-1, lower.tail = FALSE)
lower
upper

lower_p <- pf(ratio)
upper_p <- pf(0.025, df1=m-1,df2=n-1, lower.tail = FALSE)
lower_p
upper_p
{
  if((ratio <= lower)||(ratio >= upper))
    print("Null hypothesis is rejected")
  else
    print("Null hypothesis is not rejected")
}

var.test(data1,data2)

#3ページ下
m<-10; n<-15
data1 <- rnorm(m,mean=0,sd=1)
data2 <- rnorm(n,mean=1,sd=1)
#result <- t.test(data1, data2,var.equal=TRUE)
result <- t.test(data1, data2)#welchのt検定
#result$p.value
result


#kadai3??
m<-10; n<-15
data1 <- rnorm(m,mean=0,sd=1)
data2 <- rnorm(n,mean=1,sd=2)
mean1 <- mean(data1)
mean2 <- mean(data2)
var1 <- var(data1)
var2 <- var(data2)

t1 <- (mean1 - mean2) / sqrt(var1/m+var2/n)
t1#ここの値と
t.test(data1, data2)#t.testのtの値がおおよそ一致する
#result <- t.test(data1, data2,var.equal=TRUE)
#result <- t.test(data1, data2)#welchのt検定
#result$p.value
#result

#5ページ
curve(dchisq(x,df=5),from=0, to=30)
qchisq(0.95,df=5)

dice <- c(22, 27, 28, 14, 16, 13)
chisq.test(dice)

pchisq(10.9, df=5, lower.tail = FALSE)

#7ページ
x <- matrix(c(14, 8, 4, 17),ncol=2, byrow=T)
x <- matrix(c(13, 9, 5, 16),ncol=2, byrow=T)
chisq.test(x, correct=FALSE)



#kadai1
m <- 10; n <- 15
data1 <- rnorm(m, mean=0, sd=1);data2 <- rnorm(n, mean=0, sd=2)
var1 <- var(data1); var2 <- var(data2)
ratio <- var1/var2;
ratio
#lower <- qf(0.025, df1=m-1,df2=n-1)
#upper <- qf(0.025, df1=m-1,df2=n-1, lower.tail = FALSE)
#lower
#upper

lower_p <- pf(ratio,df1=m-1,df2=n-1)*2#pfはデフォルトで片側検定なので2倍する．
#upper_p <- pf(ratio,df1=m-1,df2=n-1,lower.tail = FALSE)

#{
# if((ratio <= lower)||(ratio >= upper))
#    print("Null hypothesis is rejected")
#  else
#    print("Null hypothesis is not rejected")
#}

lower_p
upper_p
var.test(data1,data2)
#f分布のグラフの出し方
#curve(df(x,df1=m-1,df2=n-1), xlim=c(0, 2))



#kadai1_for submit 
m <- 10; n <- 15
data1 <- rnorm(m, mean=0, sd=1);data2 <- rnorm(n, mean=0, sd=2)
var1 <- var(data1); var2 <- var(data2)
ratio <- var1/var2;

both_side_p <- pf(ratio,df1=m-1,df2=n-1)*2#pfはデフォルトで片側検定なので2倍する．
both_side_p#この値と
var.test(data1,data2)#var.testのp値が一致することを確認した．



#kadai3
m <- 10; n <- 15
data1 <- rnorm(m, mean=0, sd=1);data2 <- rnorm(n, mean=1, sd=1)
#t.test(data1,data2)

var1 <- var(data1); var2 <- var(data2)
mean1 <- mean(data1); mean2 <- mean(data2)
t_welch = (mean1 - mean2)/sqrt(var1/m + var2/n)

f_define <- ((var1/m + var2/n)**2)/(var1**2/(m**2*(m-1)) + var2**2/(n**2*(n-1)))
welch_p <- pt(t_welch,df=f_define)*2
t_welch#自作の検定統計量t
f_define#自作の自由度f
welch_p#自作のp値
t.test(data1,data2)#自作のそれぞれの値がt.testで得られる値と一致することを確認する．


#kadai5
d <- rmultinom(n=30,size=50,prob=c(0.1,0.3,0.2,0.25,0.15))#多項分布に従う確率ベクトルを生成する方法
# t(d)
#d <- rmultinom(n=30,size=6,prob=c(0.6,0.6,0.6,0.6,0.6,0.6))
# dice <- c(22, 27, 30, 15, 18, 8)
# d <- rmultinom(n=30,size=120,prob=c(1/6,1/6,1/6,1/6,1/6,1/6))
# t(d)

# chisq.test(dice)
chisq.test(d)


#cai <- (20 - dice)**2/dice**2

dice <- c(22, 27, 30, 15, 18, 8)
# chisq.test(dice)
d <- rmultinom(n=30,size=50,prob=c(0.1,0.3,0.2,0.25,0.15))#多項分布に従う確率ベクトルを生成する方法
t(d)
#d <- rmultinom(n=30,size=50,prob=c(0.1,0.3,0.2,0.25,0.15))#多項分布に従う確率ベクトルを生成する方法
n <- 10
d <- rmultinom(n,size=120,prob=c(1/6,1/6,1/6,1/6,1/6,1/6))
for(i in 1:n){
  t(d[,i])
  test <- chisq.test(d[,i])
  test$p.value
}

d <- rmultinom(10,size=120,prob=c(1/6,1/6,1/6,1/6,1/6,1/6))
# t(d)
t(d[,1])
test <- chisq.test(d[,1])
test
test$p.value


#Aさん，とBさんがじゃんけんを10回行う．
#条件としてはBさんのそれぞれの手を出す確率を，グー：2/3,チョキ：1/3, パー：0とAさんがわかっている状況を考えて，
#Aさんは，それを考慮してグー：2/3, チョキ：0，パー1/3で手を出すという状況を仮定する．
#d <- rmultinom(10,size=10,prob=c(4/9,4/9,1/9))#Aさんが，あいこ，勝ち，負けになる確率
d <- rmultinom(1,size=10,prob=c(4/9,4/9,1/9))#Aさんが，あいこ，勝ち，負けになる確率
# t(d)
t(d[,1])
t(d)
test <- chisq.test(d)
test
#test$p.value#これでもp値を見ることができる

#kadai5 for submit
#Aさん，とBさんがじゃんけんを20回行う．
#条件としてはBさんのそれぞれの手を出す確率を，グー：2/3,チョキ：1/3, パー：0とAさんがわかっている状況を考えて，
#Aさんは，それを考慮してグー：2/3, チョキ：0，パー1/3で手を出すという状況を仮定する．
d <- rmultinom(1,size=20,prob=c(4/9,4/9,1/9))#Aさんが，あいこ，勝ち，負けになる確率
t(d)
test <- chisq.test(d)
test


#kadai6
#nを（A1∩B1）に属する生徒の数とする．するとnの取りうる値の範囲は3≦n≦18となる．
data_chi<-numeric(16)
for(n in 3:18){
  x <- matrix(c(n,22-n, 18-n, 3+n),ncol=2, byrow=T)
  # x <- matrix(c(13, 7, 5, 18),ncol=2, byrow=T)
  test <- chisq.test(x, correct=FALSE)
  data_chi[n] <- test$p.value
}
t(data_chi)

#kadai6
#nを（A1∩B1）に属する生徒の数とする．するとnの取りうる値の範囲は3≦n≦18となる．
data_chi<-numeric(18)
for(n in 0:18){
  x <- matrix(c(n,22-n, 18-n, 3+n),ncol=2, byrow=T)
  # x <- matrix(c(13, 7, 5, 18),ncol=2, byrow=T)
  test <- chisq.test(x, correct=FALSE)
  data_chi[n] <- test$p.value
}
t(data_chi)

