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
out<-Bi01(x,n, fi=NA)
print(out,3,pr1=0.65,cr1=330,cr2=1.6)

#ブランド認知問題1
  x <- c(85,31)                          #正反応数 
  n <- c(123,121)                        #データ数 

#2つの2項分布に関する推測
out<-Bi02(x,n, fi=NA)
print(out,3,cr1=0.3,cr2=3.0,cr3=8.0)

#お年玉問題
  x <- c(42,31,29,20)                     #正反応数 
  n <- c(51,49,50,48)                     #データ数 
  g <- length(x)                          #群の数

#g個の2項分布に関する推測
out<-Bi03(x,n, fi=NA)
print(out,3)
printIJ(out$U2,IJ=rbind(c(1,2),c(2,3),c(3,4)))
printIJ(out$U2,IJ=rbind(c(1,2),c(1,3),c(2,4),c(3,4)))
printIJ(out$U2,IJ=rbind(c(1,2),c(1,3),c(1,4)))
printIJ(out$U2,IJ=rbind(c(2,4),c(3,4),c(1,4)))
