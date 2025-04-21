#第1章用スクリプト
getwd()                        #working directoryの確認
source('myfunc/myfunc.R')      #自作関数の読み込み

#表1.1の「知覚時間」のデータ入力
x<-c(31.43,31.09,33.38,30.49,29.62,
     35.40,32.58,28.96,29.43,28.52,
     25.39,32.68,30.51,30.15,32.33,
     30.43,32.50,32.07,32.35,31.57)

#表1.2　度数分布表
(xs50<-seq(24.5,35.5,1))            #階級値
(xt<-table(cut(x,xs50,right =F)))   #度数
(xp<-xt/length(x))                  #確率
cumsum(xt)                          #累積度数
cumsum(xp)                          #累積確率

#図1.1　ヒストグラム
par(mfrow=c(1,2))
xs50<-seq(24,36,1); hist(x,xs50,xlab="時間(秒)",col=4)
xs25<-seq(24,36,2); hist(x,xs25,xlab="時間(秒)",col=2)
par(mfrow=c(1,1))

#「知覚時間」要約統計量(積率系)　
mean(x)                               #平均値
van<-function(x){mean((x-mean(x))^2)} #分散を計算する関数
round(s2<-van(x),3)                   #分散
round(s<-sqrt(s2),3)                  #標準偏差

#「知覚時間」要約統計量(分位系)　
sort(x)                               #小さい順に並べる
median(x)                             #中央値
quantile(x,0.3,type =1)               #30％点
quantile(x,0.7,type =1)               #70％点
rev(sort(table(round(x))))            #階級幅1秒の最頻値
