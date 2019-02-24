rm(list=ls())
set.seed(20190225)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(MASS)
library(ggplot2)

N <- 200
mu <- c(50,60)
sd <- c(10,10)
rho <- 0.7
Sig <- matrix(nrow=2,ncol=2)
Sig[1,1] <- sd[1]*sd[1]
Sig[1,2] <- sd[1]*sd[2]*rho
Sig[2,1] <- sd[2]*sd[1]*rho
Sig[2,2] <- sd[2]*sd[2]
# 乱数の発生
X <- mvrnorm(N,mu,Sig,empirical=T)

dat <- data.frame(X)
dat$FLG <- factor(ifelse(dat$X1>40,1,2),labels=c("pass","fail"))
# 描画
g <- ggplot(dat,aes(x=X1,y=X2,group=FLG,color=FLG)) + geom_point()
g
# 相関係数の算出
### データ全体の場合
cor(X)
### 選抜効果
cor(X[X[,1]>40,])
# 欠損値を作る
X[,2] <- ifelse(X[,1]<=40,NA,X[,2])

# 欠損値のあるデータとそうでないデータに分ける
completeX <- subset(X,X[,1]>40)
missingX <- subset(X[,1],X[,1]<=40)
dataset <- list(Nobs=nrow(completeX),Nmiss=length(missingX),
                 obsX=completeX,missX=missingX)
## データセットの確認
dataset

model3 <- stan_model("missing_corr.stan",model_name="Missing Corr")
fit3 <- sampling(model3,dataset)
print(fit3,pars=c("mu","sd1","sd2","rho"))
plot(fit3,pars=c("rho"),show_density=TRUE)
