#第3章用スクリプト
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

out <-G1mean(x,prior=F, fi=NA)#正規分布に関する推測
#図3.1 トレースプロット
traceplot(out$fit, inc_warmup = F, pars=c("mu","sigma"))
out2<-print(out,2)            #表3.1, 表3.2

hist(out$mu,breaks=100)       #図3.3 平均の事後分布
hist(out$sigma,breaks=100)    #図3.4 sdの事後分布
hist(out$xaste,breaks=100)    #図3.5 事後予測分布

