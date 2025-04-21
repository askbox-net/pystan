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

#表5.2 「知覚データ」の数値要約　
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
outEQU<-G2Ind(x1,x2,EQU=1,prior=F)
#MCMC標本の名前が長いので取り出す。outEQU$sigma1をsigmaとする
Emu1<-outEQU$mu1;Emu2<-outEQU$mu2;Esigma<-outEQU$sigma1;
Exaste1<-outEQU$xaste1;Exaste2<-outEQU$xaste2;Elog_lik<-outEQU$log_lik;

#独立した2群の差の推測（標準偏差が異なる）
outDEF<-G2Ind(x1,x2,EQU=0,prior=F)
#MCMC標本の名前が長いので取り出す。
Dmu1<-outDEF$mu1;Dmu2<-outDEF$mu2;Dsigma1<-outDEF$sigma1;
Dsigma2<-outDEF$sigma2;Dxaste1<-outDEF$xaste1;Dxaste2<-outDEF$xaste2;
Dlog_lik<-outDEF$log_lik;

#表5.3
outEQU$fit
Emudef  <-Emu1-Emu2;                           #
gqcal(Emudef)

#表5.4
outDEF$fit
Dmudef  <-Dmu1-Dmu2;                           #
gqcal(Dmudef)

#図5.2 母平均の差の事後分布
hist(outEQU$mu1-outEQU$mu2,breaks=100)      

#表5.5
phc01(c(0,1),Emudef,cc="gtc",byoga="no")
phc01(c(0,1),Dmudef,cc="gtc",byoga="no")

#伝統的なt検定、等分散・両側
t.test(x1,x2,var.equal = T)

#表6.1　デルタ
Edelta   <-Emudef/Esigma;                                 #(6.3)
Ddelta1  <-Dmudef/Dsigma1;                                #(6.4)
Ddelta2  <-Dmudef/Dsigma2;                                #(6.4)
gqcal(Edelta )
gqcal(Ddelta1)
gqcal(Ddelta2)
phc01(0.2, Edelta ,0      ,cc="gtc", byoga="no");         #(6.5)
phc01(0.2, Ddelta1,0      ,cc="gtc", byoga="no");         #
phc01(0.2, Ddelta2,0      ,cc="gtc", byoga="no");         #

#表6.2 非重複度
EU3    <-pnorm(Emu1,Emu2,Esigma);                         #(6.6)
DU31   <-pnorm(Dmu1,Dmu2,Dsigma2);                        #(6.10)
DU32   <-1-pnorm(Dmu2,Dmu1,Dsigma1);                      #(6.11)
gqcal(EU3)
gqcal(DU31)
gqcal(DU32)
phc01(0.75,EU3    ,0      ,cc="gtc", byoga="no");         #(6.13)
phc01(0.75,DU31   ,0      ,cc="gtc", byoga="no");         #
phc01(0.75,DU32   ,0      ,cc="gtc", byoga="no");         #

#表6.3 優越率
Eyuetsu <-pnorm(Edelta/sqrt(2),0,1);                      #(6.18)
Dyuetsu <-pnorm(Dmudef/sqrt(Dsigma1^2+Dsigma2^2),0,1);    #(6.19)
gqcal(Eyuetsu)
phc01(0,   Exaste1,Exaste2,cc="gtc", byoga="no");         #(6.20)
gqcal(Dyuetsu)
phc01(0,   Dxaste1,Dxaste2,cc="gtc", byoga="no");         #(6.20)
phc01(0.75,Eyuetsu,0      ,cc="gtc", byoga="no");         #(6.21)
phc01(0.75,Dyuetsu,0      ,cc="gtc", byoga="no");         #(6.21)

#表6.4 閾上率
Eikijyo1<-pnorm((Emudef-1)/(sqrt(2)*Esigma),0,1);         #(6.24)
Dikijyo1<-pnorm((Dmudef-1)/sqrt(Dsigma1^2+Dsigma2^2),0,1);#(6.25)
gqcal(Eikijyo1)
phc01(1.0 ,Exaste1,Exaste2,cc="gtc", byoga="no");         #(6.26)
gqcal(Dikijyo1)
phc01(1.0 ,Dxaste1,Dxaste2,cc="gtc", byoga="no");         #(6.26)
phc01(0.75,Eikijyo1,0     ,cc="gtc", byoga="no");         #(6.27)
phc01(0.75,Dikijyo1,0     ,cc="gtc", byoga="no");         #(6.27)

#表6.5 WAICによるモデル比較
(Ewaic<- (-2)*(log(mean(exp(Elog_lik)))) + 2*(var(Elog_lik)))
(Dwaic<- (-2)*(log(mean(exp(Dlog_lik)))) + 2*(var(Dlog_lik)))
