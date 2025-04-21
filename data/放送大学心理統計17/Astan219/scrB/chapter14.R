#第14章用スクリプト
getwd()                        #working directoryの確認
source('myfunc/myfunc.R')      #自作関数の読み込み
library(rstan)                 #パッケージrstanの呼び出し
rstan_options(auto_write=T)
options(mc.cores=parallel::detectCores())

#表14.1「大腸がんデータ」入力
(can <- read.csv("scrB/大腸がん.csv",header=T))

#図14.1
pairs(can)

#表14.2
round(cov(can)*46/47)

#表14.3
round(cor(can),3)
vari<-function(x){mean((x-mean(x))^2)}
round(sqrt(apply(can,2,"vari")),2)
round(apply(can,2,"mean"),2)

#表14.4
out1<-Reg(can[,5],can[,c(1:4)], fi=NA)
pri1<-print(out1,5)

#表14.5
out2<-Reg(can[,5],can[,c(2,4)], fi=out1$fit)
pri2<-print(out2,3)

#図14.3
plot(pri2$yhat,pri2$resi,type="n")
abline(h=0,lwd=2)
text(pri2$yhat,pri2$resi,rownames(can))

#表14.6
Xnew<-matrix(c(0,0,300,0,0,100,150,50),,2,T)
pri3<-print(out2,Xnew=Xnew)
(tab1406<-data.frame(
  肉類=Xnew[,1],
  酒類=Xnew[,2],
  予測値=round(pri3$yhatc[,"EAP"],1),
  postsd=round(pri3$yhatc[,"post.sd"],1),
#  確信025=round(pri3$yhatc[,"0.025"],1),
  確信95=round(pri3$yhatc[,"0.95"],1),
  sd=round(pri3$yastc[,"sd"],1),
#  予測025=round(pri3$yastc[,"0.025"],1),
  予測95=round(pri3$yastc[,"0.95"],1)))

