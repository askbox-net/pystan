#第5章と第6章用スクリプト
getwd()                        #working directoryの確認
source('myfunc/myfunc.R')      #自作関数の読み込み
library(rstan)                 #パッケージrstanの呼び出し
rstan_options(auto_write=T)
options(mc.cores=parallel::detectCores())

#表5.1の聴音条件のデータ入力
x1<-c(32.30,34.24,28.10,33.40,37.71,31.62,31.37,35.85,32.33,34.04,
     34.96,31.43,35.28,30.19,35.09,33.38,31.49,28.44,32.12,31.81)
#表1.1の対照条件のデータ入力
x2<-c(31.43,31.09,33.38,30.49,29.62,35.40,32.58,28.96,29.43,28.52,
     25.39,32.68,30.51,30.15,32.33,30.43,32.50,32.07,32.35,31.57)

#要約統計量　
(n1<-length(x1));(n2<-length(x2))         #データの数
mean(x1);mean(x2)                         #平均値
van<-function(x){mean((x-mean(x))^2)}     #分散を計算する関数
van(x1);van(x2)                           #分散
sqrt(van(x1));sqrt(van(x2))               #標準偏差
sort(x1);sort(x2)                         #小さい順に並べる
median(x1);median(x2)                     #中央値
quantile(x1,type =2);quantile(x2,type =2) #％点

#図5.1箱ひげ図
x<-c(x1,x2);y<-c(rep("実験群",n1),rep("対照群",n2))
boxplot(x~y,cex.axis=2.0)                 

#独立した2群の差の推測（標準偏差は共通）
outEQU<-G2Ind(x1,x2,EQU=1,prior=F, fi=NA)
#独立した2群の差の推測（標準偏差が異なる）
outDEF<-G2Ind(x1,x2,EQU=0,prior=F, fi=outEQU$fit)
#出力パート
outEQU2<-print(outEQU,3,cr1=1,cr2=1,cr3=0.2,pr1=0.75,pr2=0.75,pr3=0.75)
outDEF2<-print(outDEF,3,cr1=1,cr2=1,cr3=0.2,pr1=0.75,pr2=0.75,pr3=0.75)

#図5.2 母平均の差の事後分布
hist(outEQU2$G[,1],breaks=100)      

#伝統的なt検定、等分散・両側
t.test(x1,x2,var.equal = T)
