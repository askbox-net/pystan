#第2章用スクリプト
getwd()                        #working directoryの確認
source('myfunc/myfunc.R')      #自作関数の読み込み

#図2.2　恩赦される確率の事後分布
set.seed(1234)                     #乱数の種
nod<-1000000                       #乱数の数
BdAa<-runif(nod)                   #f(Bd|Aa)の確率分布 
AaBd<-BdAa/(BdAa+1)                #事後分布
mean(AaBd)                         #EAP推定値
median(AaBd)                       #MED推定値
hist(AaBd,breaks=50,freq=F,col=3)  #事後分布、MAP推定値=0.5

#第2章演習問題 1
(0.15 * 0.5)/((0.15 * 0.5)+(0.02 * (1-0.5)))
(0.22 *  0.8823529)/((0.22 *  0.8823529)+(0.01 * (1- 0.8823529)))
(0.25 *  0.9939759)/((0.25 *  0.9939759)+(0.01 * (1- 0.9939759)))
