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

#生成量の計算
bunsan <-out$sigma^2
hendou <-out$sigma/out$mu
delta30<-(out$mu-30)/out$sigma
quarter<-out$mu-0.675*out$sigma
yosokbu<-pnorm(30.5,out$mu,out$sigma)-pnorm(29.5,out$mu,out$sigma)
hirit30<-out$xaste/30

#表4-1　生成量の推定結果(分布の要約) 
gqcal(bunsan )#分散
gqcal(hendou )#変動係数
gqcal(delta30)#デルタ30
gqcal(quarter)#第1四分位
gqcal(yosokbu)#29.5秒<x<30.5秒のデータが観察される確率
gqcal(hirit30)#85gに対する比

#各種事後分布
hist(bunsan ,breaks=100)       #図4.1
hist(hendou ,breaks=100)       #図4.2
hist(delta30,breaks=100)       #図4.3
hist(quarter,breaks=100)       #図4.4
hist(yosokbu,breaks=100)       #図4.5
hist(hirit30,breaks=100)       #図4.6
