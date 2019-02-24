rm(list=ls())
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

set.seed(20190225)
N <- 500  #データ数
et <- 10  #真の年齢の散らばり
mu <- 30  #写真の真の平均年齢
ex <- 2.5 #アプリの評価誤差(既知)

X_True <- rnorm(N,mu,et) # 本当の年齢データ
X_dat  <- X_True + rnorm(N,0,ex) # 推定される年齢データ
Y      <- -120 + 2 * X_True + rnorm(N,0,30) 
dataset <- list(N=N, D=X_dat, Y=Y)

model2 <- stan_model("uncertain_reg.stan")
fit <- sampling(model2,data=dataset)
print(fit,pars=c("mu_x","sigX","sigY","beta0","beta1"))

## 一般的な回帰分析と比較してみよう

# 補正がない場合
summary(lm(Y~X_dat))
# 真の値の場合
summary(lm(Y~X_True))
