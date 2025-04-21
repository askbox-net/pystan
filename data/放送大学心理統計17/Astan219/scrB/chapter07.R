#第7章と第8章用スクリプト
getwd()                        #working directoryの確認
source('myfunc/myfunc.R')      #自作関数の読み込み
library(rstan)                 #パッケージrstanの呼び出し
rstan_options(auto_write=T)
options(mc.cores=parallel::detectCores())

#表7.1の「パスタ」データ入力
x1<- c(110,232,176,207,122,202,191,124,193,250); #目測群
x2<- c(130,268,104,185,128,147,162, 68,142,175); #実測群
x<-cbind(x1,x2)

#要約統計量　表7.2
h1<-mean(x1);round(h1,1)                #平均値
h2<-mean(x2);round(h2,1)
van<-function(x){mean((x-mean(x))^2)}   #分散を計算する関数
va1<-van(x1);round(va1,1)               #分散
va2<-van(x2);round(va2,1)
sd1<-sqrt(va1);round(sd1,2)             #標準偏差
sd2<-sqrt(va2);round(sd2,2)
(me1<-median(x1))                       #中央値
(me2<-median(x2))
round(quantile(x1,type =2),1)           #％点
round(quantile(x2,type =2),1)
plot(x2,x1);abline(0,1,lwd=1.5)         #図7.2

#共分散と相関係数
v1<-x1-h1;round(v1,1)                   #平均偏差データ　表7.3
v2<-x2-h2;round(v2,1)
Co<-mean(v1*v2);round(Co,1)             #共分散
z1<-v1/sd1;round(z1,2)                  #標準化  表7.4
z2<-v2/sd2;round(z2,2)
r<-mean(z1*z2);round(r,2)               #相関係数

#対応ある2群の差の推測（標準偏差は共通）
outEQU<-G2pair(x,EQU=1,prior=T,mL=0, mH=250, sL=0, sH=125, fi=NA)
EQU<-print(outEQU,onlydiff=T,3,cr1=10,cr2=10,cr3=0.3,cr4=30,
                 rb= 0.5,pr1=0.60,pr2=0.70,pr3=0.70,pr4=0.8)
#対応ある2群の差の推測（標準偏差が異なる）
outDEF<-G2pair(x,EQU=0,prior=T,mL=0, mH=250, sL=0, sH=125, fi=outEQU$fit)
DEF<-print(outDEF,onlydiff=T,3,cr1=10,cr2=10,cr3=0.3,cr4=30,
                 rb= 0.5,pr1=0.60,pr2=0.70,pr3=0.70,pr4=0.8)

#各種事後分布
hist(EQU$G[,1], breaks=100)       #図8.1
hist(EQU$G[,7], breaks=100)       #図8.2
hist(EQU$G[,8], breaks=100)       #図8.3
hist(EQU$G[,9], breaks=100)       #図8.4
hist(EQU$G[,12],breaks=100)       #図8.5
