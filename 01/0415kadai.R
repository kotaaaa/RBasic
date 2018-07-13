#課題1
kai2 <- function(df) {#標準正規分布から，自由度dfだけ，サンプルを抽出し，その二乗和を算出する
  sum(rnorm(df, mean=0, sd=1)**2)
}
#カイ二乗分布(自由度5)
df <- 5
x <- sapply(c(1:100000), function(x) { kai2(df)} )
hist(x, freq=FALSE, breaks=30, ylim=c(0, 0.5),col="blue")
curve(dchisq(x, df), 0, 30, col="blue",add=TRUE)

#カイ二乗分布(自由度10)
df <- 10
x <- sapply(c(1:100000), function(x) { kai2(df)} )
hist(x, freq=FALSE, breaks=30, ylim=c(0, 0.5),col="red",add=TRUE)
curve(dchisq(x, df), 0, 30, col="red",add=TRUE)

legend("topleft", legend = c("degree of freedom:5", "degree of freedom:10"), col = c("blue", "red"), lty = 1)

#課題2
X <- rnorm(100000, mean=0, sd=1)
kai2 <- function(df) {
  sum(rnorm(df, mean=0, sd=1)**2)
}
df <- 30#df<-30でも確かめる
Y <- sapply(c(1:100000), function(x) { kai2(df)} )

T_dtb <- X/sqrt(Y/df)
hist(T_dtb, freq=FALSE, breaks=30, ylim=c(0, 0.55),col="green")
curve(dt(x, df), from=-5, to=5, col="red", add=TRUE)

legend("topleft", legend = c("degree of freedom:30", "theoretical(t_distribution)"), col = c("green", "red"), lty = 1)

#課題3
#標準正規分布に従う1000点のサンプルを取得
L_U <- rnorm(1000, mean=0, sd=1)
L_V <- rnorm(1000, mean=0, sd=1)

#アルファ，ベータ，ローを定義する
rho = 0#rho=0.3, 0.6, -0.3
alpha = (sqrt(1+rho)+sqrt(1-rho))/2
beta = (sqrt(1+rho)-sqrt(1-rho))/2

#ラージX，ラージYを定義する
L_X = alpha*L_U + beta*L_V
L_Y = beta*L_U + alpha*L_V

#2次元空間にプロットする
plot(L_X, L_Y, main="Scatter plot(rho=0)")

#以下ラージX，ラージYがそれぞれ標準正規分布に従うかの確認のソース
#hist(L_X, freq=FALSE, breaks=30, ylim=c(0, 0.5),col="orange")
#hist(L_Y, freq=FALSE, breaks=30, ylim=c(0, 0.5),col="green")
#curve(dnorm(x, mean=0, sd=1), from=-10, to=10, col="red", add=TRUE)


