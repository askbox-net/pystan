#第11章用スクリプト
getwd()                        #working directoryの確認
source('myfunc/myfunc.R')      #自作関数の読み込み
library(rstan)                 #パッケージrstanの呼び出し
rstan_options(auto_write=T)
options(mc.cores=parallel::detectCores())

#餡の選好問題
  x <- 305                               #正反応数 
  n <- 500                               #データ数 

#1つの2項分布に関する推測
aaa<-Bi01(x,n, fi=NA)

#表11.2
aaa$fit
mean(0.65<aaa$theta)
mean(330<aaa$xaste)
mean(1.6<aaa$Odds)

#ブランド認知問題1
  x <- c(85,31)                          #正反応数 
  n <- c(123,121)                        #データ数 

#2つの2項分布に関する推測
ccc<-Bi02(x,n, fi=NA)

#表11.4
ccc$fit
mean(0.3<ccc$p_sa)
mean(3.0<ccc$p_hi)
mean(8.0<ccc$Odds_hi)

#お年玉問題
  x <- c(42,31,29,20)                     #正反応数 
  n <- c(51,49,50,48)                     #データ数 
  g <- length(x)                          #群の数

#g個の2項分布に関する推測
ddd<-Bi03(x,n, fi=NA)

#表11.6
colnames(ddd$p)    <-paste("p",1:4,sep="")
gqcal(ddd$p)    ;#

#表11.7
phc02(0,ddd$p,cc="gtc",3)

#連言命題が正しい確率
printIJ(ddd$U2,IJ=rbind(c(1,2),c(2,3),c(3,4)))
printIJ(ddd$U2,IJ=rbind(c(1,2),c(1,3),c(2,4),c(3,4)))
printIJ(ddd$U2,IJ=rbind(c(1,2),c(1,3),c(1,4)))
printIJ(ddd$U2,IJ=rbind(c(2,4),c(3,4),c(1,4)))
