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

#対応ある2群の差の推測（標準偏差が異なる）
outDEF<-G2pair(x,EQU=0,prior=T,mL=0, mH=250, sL=0, sH=125, fi=outEQU$fit)

#表7.5
outEQU$fit

#表7.6
outDEF$fit

#第8章はここから
#EQUのMCMC標本の名前が長いので取り出す。outEQU$sigma1をsigmaとする
Emu1<-outEQU$mu1;Emu2<-outEQU$mu2;Esigma<-outEQU$sigma1;Erho<-outEQU$rho;
Exaste1<-outEQU$xaste1;Exaste2<-outEQU$xaste2;Elog_lik<-outEQU$log_lik;

#DEFのMCMC標本の名前が長いので取り出す。
Dmu1<-outDEF$mu1;Dmu2<-outDEF$mu2;Dsigma1<-outDEF$sigma1;Drho<-outDEF$rho;
Dsigma2<-outDEF$sigma2;Dxaste1<-outDEF$xaste1;Dxaste2<-outDEF$xaste2;
Dlog_lik<-outDEF$log_lik;

#表8.1 平均値の差の推測
Emudef  <-Emu1-Emu2;                           #
Dmudef  <-Dmu1-Dmu2;                           #
gqcal(Emudef)
gqcal(Dmudef)
phc01(c(0,10),Emudef,cc="gtc",byoga="no")
phc01(c(0,10),Dmudef,cc="gtc",byoga="no")
hist(Emudef, breaks=100);                                    #図8.1

#RQ.5
mean(0.5<Erho)

#表8.2 差得点の標準偏差の推測
EsigmaD  <-Esigma*sqrt(2*(1-Erho));                          #(8.4)
DsigmaD  <-sqrt(Dsigma1^2+Dsigma2^2-2*Drho*Dsigma1*Dsigma2); #(8.5)
gqcal(EsigmaD)
gqcal(DsigmaD)
phc01(30,EsigmaD,cc="ltc",byoga="no");                       #(8.6)
phc01(30,DsigmaD,cc="ltc",byoga="no")
hist(EsigmaD, breaks=100);                                   #図8.2

#表8.3 差得点の効果量の推測
EdeltaD  <-Emudef/EsigmaD;                                   #(8.9)
DdeltaD  <-Dmudef/DsigmaD;                                   #(8.10)
gqcal(EdeltaD)
gqcal(DdeltaD)
phc01(0.3,EdeltaD,cc="gtc",byoga="no");                      #(8.11)
phc01(0.3,DdeltaD,cc="gtc",byoga="no")
hist(EdeltaD, breaks=100);                                   #図8.3

#表8.4 差得点の優越率の推測
EyuetsuD <-pnorm(Emudef/EsigmaD,0,1);                        #(8.14)
DyuetsuD <-pnorm(Dmudef/DsigmaD,0,1);                        #(8.15)
gqcal(EyuetsuD)
gqcal(DyuetsuD)
phc01(0.7,EyuetsuD,cc="gtc",byoga="no");                     #(8.16)
phc01(0.7,DyuetsuD,cc="gtc",byoga="no");                     #(8.16)
hist(EdeltaD, breaks=100);                                   #図8.4

#表8.5 差得点の閾上率の推測
EikijyoD10<-pnorm((Emudef-10)/EsigmaD,0,1);                  #(8.19)
DikijyoD10<-pnorm((Dmudef-10)/DsigmaD,0,1);                  #(8.20)
gqcal(EikijyoD10)
gqcal(DikijyoD10)
phc01(0.7,EikijyoD10,cc="gtc",byoga="no");                   #(8.21)
phc01(0.7,DikijyoD10,cc="gtc",byoga="no");                   #(8.21)
hist(EikijyoD10,breaks=100);                                 #図8.5
