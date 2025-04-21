//1要因対応なし アンバランスでも計算可能
data { 
  int<lower=0> n;                              //データ数 
  int<lower=0> a;                              //A水準数   
  vector[n]    y;                              //特性値   
  int<lower=0> A[n];                           //A水準   
}
parameters {
  vector                   [a] muA;            //A平均
  real<lower=0>             sigmaE;            //誤差SD
}
transformed parameters {
}
model {
  for(i in 1:n){y[i]~normal(muA[A[i]],sigmaE);}//正規分布
}
generated quantities{
  real<lower=0>            sigmaA;             //要因ASD
  real<lower=0,upper=1>      eta2;             //説明率
  real<lower=0>             delta;             //効果量
  real                         mu;             //全平均
  vector                   [a] aj;             //A効果
  real<lower=0,upper=1>   Ubig[a];             //論理値0以上
  real<lower=0,upper=1>   Usma[a];             //論理値0以下
  real<lower=0,upper=1>   U2[a,a];             //2水準比較
  real log_lik;
  sigmaA  = sqrt(variance(muA)*(a-1)/a);       
  eta2  = pow(sigmaA,2)/(pow(sigmaA,2)+pow(sigmaE,2));
  delta  = sigmaA/sigmaE;
  mu  = mean(muA);
  for (i in 1:a){aj[i]  = muA[i]-mu;
                 Ubig[i] =aj[i]>0 ? 1 : 0;
                 Usma[i] =aj[i]<0 ? 1 : 0;
                 U2[i,i] =0;                   }
  for (i in 1:(a-1)){
    for (j in (i+1):a){
      U2[i,j] = aj[i]-aj[j]>0 ? 1 : 0;
      U2[j,i] = !(U2[i,j]);  }  }
  log_lik  = 0.0;
  for(i in 1:n){ log_lik  = log_lik + 
        normal_lpdf(y[i] | muA[A[i]], sigmaE); //対数尤度
        }
}
