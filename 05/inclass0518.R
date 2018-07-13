x<-read.table("corr1_sample.txt", header=T)
attach(x)
x
plot(x,pch=21,col="blue", bg="red")

var(math)
plot(math, science,pch=21, col="blue",bg="red")
abline(v=mean(math),col="red")
for(i in 1:length(math)) lines(c(math[i],mean(math)), c(science[i], science[i]) ,col="red")#,science)

var(science)
# plot(math, science,pch=21, col="blue",bg="red")
abline(h=mean(science),col="blue")
for(i in 1:length(math)) lines(c(math[i],math[i]), c(science[i], mean(science)),col="blue")#,math)

var(math, science)


var(math,science) /sqrt(var(math)*var(science))
cor(math,science)
cor.test(math,science)
cor(x)
cor.test(japanese,math)

# plot(x[,1:4],lower.panel=panel.cor,upper.panel=panel.smooth, diag.panel=panel.hist)

panel.hist<-function(x,...)
{
  usr<-par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2],0,1.5))
  h<-hist(x,plot=FALSE)
  breaks<-h$breaks
  nB<-length(breaks)
  y<h$counts; y<-y/max(y)
  rect(breaks[-nB],0,breaks[-1], y,col="grey70",...)

}
  
panel.cor <-function(x,y,digits=3)
{
  usr<-par("usr");on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r<-cor(x,y,use="complete")
  txt<-round(r, digits)
  text(0.5, 0.5, txt, cex = 0.8 / strwidth(txt) * abs(r) ^ 0.25)
}
 
plot(x[,1:4],lower.panel=panel.cor,upper.panel=panel.smooth, diag.panel=panel.hist) 
  


#単回帰分析

lm(science~math)

plot(math,science,pch=21,col="blue", bg="red")
abline(lm(science~math),col="purple")

fitted<-predict(lm(science~math))
fitted
science
for (i in 1:16) lines(c(math[i],math[i]), c(science[i], fitted[i]), col="red")
b<-seq(0.4,0.65,0.002)
sse<-numeric(length(b))
# length(b)
# sse
for (i in 1:length(b)) {
  a<-mean(science) - b[i]*mean(math)
  residual<-science - a - b[i]*math
  sse[i] <- sum(residual^2)
}
  
plot(b,sse, type="l", ylim=c(220,270))
abline(h=min(sse),col="orange", lty=2)
b[which(sse==min(sse))]
  

SSX <- sum(math^2) -sum(math)^2/length(math)
SSY <- sum(science^2) -sum(science)^2/length(science)
SSXY <- sum(math*science) - sum(math)*sum(science)/length(math)

b<-SSXY/SSX
e<-sum(science)/length(science)-(SSXY/SSX)*sum(math)/length(math)
b
e

SSY
SSR<-SSXY^2/SSX
SSE<-SSY-SSR
SSE<-SSY - SSR
SR<-SSR/1
SE<-SSE/(15-1)
F<-SR/SE
F
1-pf(F,1,14)

SSR/SSY


model <- (lm(science~math))
summary(model)
summary.aov(model)


#重回帰分析
x<-read.table("regress1_sample.txt", header=T)
attach(x)
x
cor(x)

ans<-lm(tokei~biseki+daisu+kakuritsu,x)
summary(ans)

x<-read.table("regress2_sample.txt", header=T)
attach(x)
x
cor.test(tokei,taiiku)


library(MASS)
ans<-lm(tokei~biseki+daisu+kakuritsu+kakuritsu+taiiku,x)
summary(ans)
ans2<-stepAIC(ans)
summary(ans2)

x<-read.table("regress3_sample.txt", header=T)
attach(x)
plot(amount, taste, pch=19,cex=1.5)

ans1<-lm(taste~amount)
summary(ans1)
ans3<-lm(taste~amount+I(amount^2)+I(amount^3))
summary(ans3)
ans5<-lm(taste~amount+I(amount^2)+I(amount^3)+I(amount^4)+I(amount^5))
summary(ans5)

xx1<-seq(0,10,by=0.05)
new.data<-data.frame(amount=xx1)
yy1<-predict(ans1,new.data)
yy3<-predict(ans3,new.data)
yy5<-predict(ans5,new.data)

matlines(xx1,cbind(yy1,yy3), lty=3:2,col="orange")
matlines(xx1,cbind(yy1,yy3,yy5), lty=3:1,col="blue")

legend("bottomright", c("1dim","3dim","5dim"), lty=3:1)
###