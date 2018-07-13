x<-read.table('anova1_sample.txt', header=T)
attach(x)#ヘッダーを変数として使えるようにする
summary(x)


ave<-c(mean(A), mean(B), mean(C), mean(D))
std<-c(sd(A),sd(B),sd(C),sd(D))


b<- barplot(ave, ylim=c(0,100), ylab='score', xlab='class', names.arg=c('A','B','C','D'))
arrows(b, ave+std,b,ave-std, angle=90, length=0.1)
arrows(b, ave-std,b,ave+std, angle=90, length=0.1)


#両組全体でプロット
plot(1:length(c(A,B)), c(A,B),ylim=c(60,100), ylab='score',xlab='order', pch=21,bg='red')
abline(h=mean(c(A,B)), col='blue')
#残差に線を引いてみる
for(i in 1:length(c(A,B))) lines(c(i,i), c(mean(c(A,B)), c(A,B)[i]), col='red')
SSY<-sum((c(A,B)-mean(c(A,B)))^2)
SSY



#次は，組毎にプロット(A,B)
plot(1:length(c(A,B)), c(A,B),ylim=c(60,100), ylab='score',xlab='order', pch=21,col=c(rep(1,8), rep(2,8)))
abline(h=mean(A), col=1)
abline(h=mean(B), col=2)

index<-1:16
for(i in 1:8)
lines(c(index[i], index[i]), c(mean(A),A[i]), col=1)

for(i in 9:16)
lines(c(index[i], index[i]), c(mean(B),B[i-8]), col=2)

#残差に線を引いてみる
# for(i in 1:length(c(A,B))) lines(c(i,i), c(mean(c(A,B)), c(A,B)[i]), col='red')
# SSY<-sum((c(A,B)-mean(c(A,B)))^2)
# SSY

SSE<-sum((A-mean(A))^2)+sum((B-mean(B))^2)
SSE



#分散分析表を作ってみよう
SSY<-sum((c(A,B)-mean(c(A,B)))^2)
SSE<-sum((A-mean(A))^2)+sum((B-mean(B))^2)
SSA<-SSY-SSE

SSY
SSE
SSA


JY<-length(c(A,B))-1
JA<-1
JE<-JY-JA

JY
JA
JE

SA<-SSA/JA
SE<-SSE/JE

SA
SE

#次は，組毎にプロット(A,B,C,D)
plot(1:length(c(A,B)), c(A,B),ylim=c(60,100), ylab='score',xlab='order', pch=21,col=c(rep(1,8), rep(2,8),rep(3,8), rep(4,8)))
abline(h=mean(A), col=1)
abline(h=mean(B), col=2)
abline(h=mean(C), col=3)
abline(h=mean(D), col=4)

index<-1:32
for(i in 1:8)
  lines(c(index[i], index[i]), c(mean(A),A[i]), col=1)
for(i in 9:16)
  lines(c(index[i], index[i]), c(mean(B),B[i-8]), col=2)
for(i in 17:24)
  lines(c(index[i], index[i]), c(mean(A),A[i-16]), col=3)
for(i in 25:32)
  lines(c(index[i], index[i]), c(mean(B),B[i-24]), col=4)

#全組全体でプロット(A,B,C,D)
plot(1:length(c(A,B,C,D)), c(A,B,C,D),ylim=c(60,100), ylab='score',xlab='order', pch=21,bg='red')
abline(h=mean(c(A,B,C,D)), col='blue')
for(i in 1:length(c(A,B,C,D))) lines(c(i,i),c(mean(c(A,B,C,D)), c(A,B,C,D)[i]), col='red')

#分散分析表を作ってみよう(A,B,C,D)
SSY<-sum((c(A,B,C,D)-mean(c(A,B,C,D)))^2)
SSE<-sum((A-mean(A))^2)+sum((B-mean(B))^2)+sum((C-mean(C))^2)+sum((D-mean(D))^2)
SSA<-SSY-SSE

SSY
SSE
SSA


JY<-length(c(A,B,C,D))-1#(A,B,C,D)
JA<-3
JE<-JY-JA

JY
JA
JE

SA<-SSA/JA#(A,B,C,D)
SE<-SSE/JE

SA
SE

#F比を求める
FA<-SA/SE
qf(0.95,JA,JE)
1-pf(FA,JA,JE)


x<-c(A,B,C,D)
plot(x,df(x,3,28),type='n', xlim=c(0,5), ylim=c(0,1))
curve(df(x,3,28),type='l', add=T)
curve(pf(x,3,28),type='l', lty=3, add=T)
abline=(h=0.05)
lower.alpha5<-qf(0.05,3,28)
lower.alpha5
abline(v=lower.alpha5)
points(lower.alpha5,0.05,cex=3.0,pch='*')
upper.alpha5<-qf(0.05,3,28,lower.tail=FALSE)
upper.alpha5
abline(v=upper.alpha5)
points(upper.alpha5,0.95,cex=3.0,pch='*')
abline(v=FA)


#関数を使って分散分析
x<-read.table('anova1_sample.txt', header=T)
y<-stack(x)
y


summary(aov(y$values ~ y$ind))
  # summary(aov(values~ind)#書き方
oneway.test(y$values ~ y$ind, var.equal=TRUE)
  # oneway.test(values~ind)#書き方


#t検定
t.test(A,B,var.equal=T)
t.test(A,C,var.equal=T)
t.test(A,D,var.equal=T)
t.test(B,C,var.equal=T)
t.test(B,D,var.equal=T)
t.test(C,D,var.equal=T)

#多重比較
pairwise.t.test(y$values, y$ind, p.adj='bonf')




data1 <- c(6,7,8,5,6,7,7,8,9,10,11,12,8,9,10,13,14,15)
data2 <- c(3,4,5,5,6,7,7,8,9,9,10,11,4,5,6,6,7,8)
data1

element1 <- factor(rep(c(rep('levelA1',3),rep('levelA2',3)),3))
element2 <- factor(c(rep('levelB1',6),rep('levelB2',6),rep('levelB3',6)))
summary(aov(data1~element1*element2))
summary(aov(data2~element1*element2))

interaction.plot(element1,element2,data1)
interaction.plot(element2,element1,data1)


#演習3
#関数を使って分散分析
# options(digits=7)
x<-read.table('anova3_sample.txt', header=T,fileEncoding = "UTF-8")
attach(x)#ヘッダーを変数として使えるようにする
x

micrograms <- x$Amount.g
hours <- x$time.h.
div <- x$division


microgram <- factor(micrograms)
hour <- factor(hours)
summary(aov(div~microgram*hour))
# summary(aov(data2~element1*element2))

interaction.plot(microgram,hour,div)
interaction.plot(hour,microgram,div)

qf(0.95,2,3)

qf(0.95,3,28)
1-pf(4.8841, 3,28)

#多重比較してみる（薬剤の量が細胞分裂に影響するのか判断する）
pairwise.t.test(div, microgram, p.adj='bonf')
#多重比較してみる（薬剤の時間が細胞分裂に影響するのか判断する）
pairwise.t.test(div,hour, p.adj='bonf')

summary(aov(y$values ~ y$ind))
# summary(aov(values~ind)#書き方
oneway.test(y$values ~ y$ind, var.equal=TRUE)
# oneway.test(values~ind)#書き方
