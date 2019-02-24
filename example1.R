rm(list=ls())
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# データ
x <- c(-27.020,3.570,8.191,9.898,9.603,9.945,10.056)

# stanのコンパイル
model1 <- stan_model("7scientist.stan")
fit1 <- sampling(model1, data=list(X=x))
print(fit1)
plot(fit1)

