#第9章用スクリプト
getwd()                        #working directoryの確認
source('myfunc/myfunc.R')      #自作関数の読み込み
library(rstan)                 #パッケージrstanの呼び出し
rstan_options(auto_write=T)
options(mc.cores=parallel::detectCores())

#表9.1の「知覚時間」データ入力
#対照
x1<-c(31.43,31.09,33.38,30.49,29.62,35.40,32.58,28.96,29.43,28.52,
     25.39,32.68,30.51,30.15,32.33,30.43,32.50,32.07,32.35,31.57)
#聴音
x2<-c(32.30,34.24,28.10,33.40,37.71,31.62,31.37,35.85,32.33,34.04,
     34.96,31.43,35.28,30.19,35.09,33.38,31.49,28.44,32.12,31.81)
#音読
x3<-c(31.62,37.04,33.76,30.01,34.18,33.08,28.77,33.90,28.06,37.54,
     33.89,32.23,35.95,36.68,33.57,30.87,32.20,29.98,33.08,35.12)
#運動
x4<-c(27.79,30.33,29.02,27.02,35.28,26.89,30.33,27.98,27.99,28.04,
     25.02,28.80,30.74,26.79,28.65,32.43,29.13,29.50,28.14,32.19)
n1<-length(x1);n2<-length(x2);n3<-length(x3);n4<-length(x4);

#図9.1
y<-c(x1,x2,x3,x4)
AA<-c(rep("x1対照",n1),rep("x2聴音",n2),rep("x3音読",n3),rep("x4運動",n4))
boxplot(y~AA,cex.axis=2.0)

#１要因実験の推測
A<-c(rep(1,n1),rep(2,n2),rep(3,n3),rep(4,n4))    #水準の指定
aaa<-E1Ind(y,A,prior=T,mL=0, mH=100, sL=0, sH=50, fi=NA)

#表9.2 母数の推定結果
colnames(aaa$muA)    <-paste("muA",1:4,sep="")
gqcal(aaa$muA,   2)    ;
gqcal(aaa$sigmaE,2)    ;

#表9.3 全平均と水準効果の推定結果
gqcal(aaa$mu,2)    ;
colnames(aaa$aj)    <-paste("aj",1:4,sep="")
gqcal(aaa$aj,2)    ;

#図9.2
size<-dim(aaa$mu);              #乱数数
yy<-aaa$aj[,1]; for (i in 2:4){yy<-c(yy,aaa$aj[,i])};
boxplot(yy~rep(c("x1対照","x2聴音","x3音読","x4運動"),each=size))
abline(h=0.0,lwd=2.0)

#表9.4 水準の効果が0より大きい（小さい）確率
(Upphc<-round(apply(aaa$Ubig,2,mean),3))    ;
1-Upphc;

#表9.5 効果の大きさに関する生成量の推定結果
gqcal(aaa$sigmaA,3)    ;
gqcal(aaa$eta2,  3)    ;
gqcal(aaa$delta, 3)    ;

#説明率と効果量の事後分布
hist(aaa$eta2, breaks=100)       #図9.3
hist(aaa$delta, breaks=100)      #図9.4

#表9.6 行の水準の効果が列の水準の効果より大きい確率
phc02(0,aaa$aj, cc="gtc")

#連言命題が正しい確率
printIJ(aaa$U2,IJ=rbind(c(3,2),c(2,1),c(1,4)));#(9.18)
printIJ(aaa$U2,IJ=rbind(c(3,1),c(2,1),c(1,4)));#(9.19)
printIJ(aaa$U2,IJ=rbind(c(3,4),c(2,4),c(1,4)));#(9.20)

#表9.7 特に興味のある2水準間の比較(音読・聴音)
E1betw_level(aaa,3,3,2,cr1=0.5);

#表9.8 特に興味のある2水準間の比較(音読・運動)
E1betw_level(aaa,3,3,4,cr1=2.0);
