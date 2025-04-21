#第13章用スクリプト
getwd()                        #working directoryの確認
source('myfunc/myfunc.R')      #自作関数の読み込み
library(rstan)                 #パッケージrstanの呼び出し
rstan_options(auto_write=T)
options(mc.cores=parallel::detectCores())

#表13.1 「肺がんデータ」入力
(smoke <- read.csv("scrB/smoke.csv", sep=',',header=T,row.names=1))

#図13.1
plot(smoke$喫煙数,smoke$肺がん,type="n",xlim=c(12,45),ylim=c(10,28))
text(smoke$喫煙数,smoke$肺がん,labels =rownames(smoke))

#図13.2
plot(smoke$喫煙数,smoke$肺がん,type="p",xlim=c(12,45),ylim=c(10,28))
abline(a=6.466,b=0.529)

#単回帰分析の実行
out<-Reg(smoke[,2],smoke[,1], fi=NA)
out2<-print(out)
out2$resi

#図13.4
plot(smoke[,1],out2$resi,type="n",)
abline(h=0,lwd=2)
text(smoke[,1],out2$resi,rownames(smoke))

#図13.5
out3<-print(out,Xnew=seq(9,47,2))
plot(smoke$喫煙数,smoke$肺がん,type="p",xlim=c(12,45),ylim=c(10,28))
abline(a=6.466,b=0.529)
lines(seq(9,47,2),out3$yhatc[,"0.025"],lty=2)
lines(seq(9,47,2),out3$yhatc[,"0.975"],lty=2)
lines(seq(9,47,2),out3$yastc[,"0.025"],lty=3)
lines(seq(9,47,2),out3$yastc[,"0.975"],lty=3)

#表13.3 
out4<-print(out,Xnew=smoke[,1])
(tab1303<-data.frame(
  州名=rownames(smoke),
  予測値=round(out4$yhatc[,"EAP"],2),
  残差=round(out4$resi,2),
  postsd=round(out4$yhatc[,"post.sd"],2),
  確信025=round(out4$yhatc[,"0.025"],2),
  確信975=round(out4$yhatc[,"0.975"],2),
  sd=round(out4$yastc[,"sd"],2),
  予測025=round(out4$yastc[,"0.025"],2),
  予測975=round(out4$yastc[,"0.975"],2)))


