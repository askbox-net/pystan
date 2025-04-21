import stan
import arviz
from os.path import basename, dirname
#import matplotlib.pyplot as plt


#表1.1の「知覚時間」のデータ入力
data = [31.43,31.09,33.38,30.49,29.62,
     35.40,32.58,28.96,29.43,28.52,
     25.39,32.68,30.51,30.15,32.33,
     30.43,32.50,32.07,32.35,31.57]


model_code = """
data {
    int<lower=0>  n;            //データ数
    array[n] real<lower=0> x;   //データ
}
parameters {
    real              mu;    //平均(範囲指定)
    real<lower=0>     sigma; //標準偏差(範囲指定)
}
transformed parameters {
}
model {
    x ~ normal(mu,sigma);      //正規分布
}
generated quantities {
    real xaste;
    real log_lik;
    xaste = normal_rng(mu,sigma);     //予測分布
    log_lik = normal_lpdf(x|mu,sigma); //対数確率
}
"""

posterior = stan.build(model_code, data={ 'x': data, 'n': len(data) })
fit = posterior.sample(num_chains=4, num_samples=1000)

axes = arviz.plot_trace(fit)
fig = axes.ravel()[0].figure
fig.savefig("%s/%s1.png" % (dirname(__file__), basename(__file__)), dpi=90)

df = fit.to_frame()  # pandas `DataFrame, requires pandas

df.to_csv(f'{__file__}.csv')
print(df)

#pdata = fit.extract()

print(fit['sigma'])
