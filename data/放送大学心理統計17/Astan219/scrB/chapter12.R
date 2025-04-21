#第12章用スクリプト
getwd()                        #working directoryの確認
source('myfunc/myfunc.R')      #自作関数の読み込み
library(rstan)                 #パッケージrstanの呼び出し
rstan_options(auto_write=T)
options(mc.cores=parallel::detectCores())

#ペット問題
  x <- c(32,29,18,15,10)
#カテゴリ数がkの比率の推測
out<-Mu01(x, fi=NA)
print(out,3)
printIJ(out$U2,IJ=rbind(c(1,2),c(2,3),c(3,4)))
printIJ(out$U2,IJ=rbind(c(1,3),c(2,3),c(1,4),c(2,4)))

#ブランド認知問題2
  x <- matrix(c(
       70,30,
       28,72),2,2,T)                         #反応数 
apply(x,1,"sum");apply(x,2,"sum")

#対応ある2×2のクロス表の分析
out1<-Mu02(x, fi=NA)
print(out1,3)

#パスタ問題
  x <- matrix(c(
       19,  9,  6,
       10, 19,  5,
       15, 14, 18),3,3,T)                    #反応数 
apply(x,1,"sum");apply(x,2,"sum");sum(x)

#対応あるa×bのクロス表の分析
out<-Mu02(x, fi=out1$fit)
print(out,3)
Up<-out$Up; Um<-out$Um
round(mean(Up[,1,1]*Up[,2,2]*Up[,3,3]*Um[,1,2]*Um[,2,3]),3)
round(mean(Up[,1,1]*Up[,2,2]*Up[,3,3]),3)
