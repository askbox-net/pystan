#第4章用スクリプト
getwd()                        #working directoryの確認
source('myfunc/myfunc.R')      #自作関数の読み込み
library(rstan)                 #パッケージrstanの呼び出し
rstan_options(auto_write=T)
options(mc.cores=parallel::detectCores())

#表1.1の「知覚時間」のデータ入力
x<-c(31.43,31.09,33.38,30.49,29.62,
     35.40,32.58,28.96,29.43,28.52,
     25.39,32.68,30.51,30.15,32.33,
     30.43,32.50,32.07,32.35,31.57)

out <-G1mean(x,prior=F, fi=NA) #平均値等に関する推測
out2<-print(out,degits=2,cr1=30,cr2a=30.5,cr2b=29.5,
             cr3=30,cr4=30,cr5=0.5,pr1=0.25,pr2=0.2)

#各種事後分布
hist(out2$G[,1],breaks=100)       #図4.1
hist(out2$G[,2],breaks=100)       #図4.2
hist(out2$G[,3],breaks=100)       #図4.3
hist(out2$G[,4],breaks=100)       #図4.4
hist(out2$G[,5],breaks=100)       #図4.5
hist(out2$G[,6],breaks=100)       #図4.6
