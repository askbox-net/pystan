#第12章用スクリプト
getwd()                        #working directoryの確認
source('myfunc/myfunc.R')      #自作関数の読み込み
library(rstan)                 #パッケージrstanの呼び出し
rstan_options(auto_write=T)
options(mc.cores=parallel::detectCores())

#ペット問題
  x <- c(32,29,18,15,10)
#カテゴリ数がkの比率の推測
bbb<-Mu01(x, fi=NA)

#表12.2
colnames(bbb$pi)<-paste("p",1:6,sep="")
gqcal(bbb$pi)                 ; #

#表12.3
phc02(c=0.0 ,bbb$pi,cc="gtc") ; #

#連言命題が正しい確率
printIJ(bbb$U2,IJ=rbind(c(1,2),c(2,3),c(3,4)))       ;#(12.12)
printIJ(bbb$U2,IJ=rbind(c(1,3),c(2,3),c(1,4),c(2,4)));#(12.13)

#ブランド認知問題2
  x <- matrix(c(
       70,30,
       28,72),2,2,T)                         #反応数 
apply(x,1,"sum");apply(x,2,"sum")

#対応ある2×2のクロス表の分析
eee<-Mu02(x, fi=NA)
pim  <-cbind(eee$pim[,1,]  ,eee$pim[,2,])
res  <-cbind(eee$res[,1,]  ,eee$res[,2,])
colnames(pim)<-colnames(res)<-as.vector(t(outer(1:2,1:2,paste,sep=",")))

#表12.8
gqcal(pim)  ;  

#表12.11
gqcal(res)  ;  
gqcal(eee$V)  ;#


#パスタ問題
  x <- matrix(c(
       19,  9,  6,
       10, 19,  5,
       15, 14, 18),3,3,T)                    #反応数 
apply(x,1,"sum");apply(x,2,"sum");sum(x)

#対応あるa×bのクロス表の分析
fff<-Mu02(x, fi=NA)

#結果の取り出し(配列を行列にしている)
pim  <-cbind(fff$pim[,1,],fff$pim[,2,],fff$pim[,3,])
UpM   <-cbind(fff$Up[,1,]  ,fff$Up[,2,],fff$Up[,3,])
colnames(pim)<-colnames(UpM)<-as.vector(t(outer(1:3,1:3,paste,sep=",")))

#表12.15
gqcal(pim)  ;  

#クラメルの連関係数
gqcal(fff$V ) ;#p.203

#表12.16
colnames(fff$pa)<-paste("pa",1:3,sep="")
colnames(fff$pb)<-paste("pb",1:3,sep="")
gqcal(fff$pa) ;#
gqcal(fff$pb) ;#

#表12.17
(Upphc<-matrix(round(apply(UpM,2,mean),3),3,3,byrow=T))    ;
1-Upphc

Up<-fff$Up; Um<-fff$Um
round(mean(Up[,1,1]*Up[,2,2]*Up[,3,3]*Um[,1,2]*Um[,2,3]),3);#(12.45)
round(mean(Up[,1,1]*Up[,2,2]*Up[,3,3]),3)                  ;#(12.46)
