rm(list=ls())
set.seed(20190225)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(MASS)
library(tidyverse)

#############状態空間モデル
dat <- read_csv("weight.csv")
dat.comp <- na.omit(dat)
ggplot(dat.comp,aes(x=date,y=weight))+geom_point()+geom_line()

dataset <- list(N=nrow(dat.comp),W=dat.comp$weight)
model5 <- stan_model("weight.stan",model_name="state space modeling")
fit5 <- sampling(model5,data=dataset)
fit5
print(fit5,pars=c("sig","tau"))
print(fit5,pars=c("mu"))


###########  欠損値対応&未来予測
#データの体重部分
W <- dat$weight 
#欠測値の数を数えます
Nmiss <- sum(is.na(W))
# 予測したい日数
predN <- 10
Nmiss <- Nmiss + predN　
#データがもし欠損であれば9999という数字を入れます
W <- ifelse(is.na(W),9999,W)　
predW <- rep(9999,predN)
W <- c(W,predW)
# データの確認
W

model5.missing <- stan_model("weight2.stan",model_name="Missing and Predict")
dataset <- list(N=length(W),W=W,Nmiss=Nmiss)
fit5.missing <- sampling(model5.missing,data=dataset)
print(fit5.missing,pars=c("sig","tau"))
print(fit5.missing,pars=c("mu"))
print(fit5.missing,pars=c("Miss_W"))

# 描画
rstan::extract(fit5.missing,pars="mu") %>% data.frame %>% 
  tidyr::gather(key,val,factor_key=T) %>% group_by(key) %>% 
  summarise(MAP=median(val),U75=quantile(val,probs = 0.75),L25=quantile(val,probs = 0.25)) -> Mu

rstan::extract(fit5.missing,pars="Miss_W") %>% data.frame %>% 
  tidyr::gather(key,val,factor_key=T) %>% group_by(key) %>% 
  summarise(MAP=median(val),U75=quantile(val,probs = 0.75),L25=quantile(val,probs = 0.25)) -> Ws

count <- 0
W2 <- c()
for(i in 1:length(W)){
  if(W[i]==9999){
    count <- count + 1
    W2[i] <- Ws$MAP[count]
  }else{
    W2[i] <- W[i]
  }
}

Mu$W <- W
Mu$W2 <- W2
Mu$FLG <- ifelse(Mu$W==9999,1,2) %>% factor(labels=c("Missing","Observed"))
ggplot(Mu,aes(x=1:NROW(Mu),y=MAP))+
  geom_point()+geom_errorbar(aes(ymin = L25,ymax = U75)) +
  xlab("Date")+ylab("Weight")+
  geom_point(aes(y=W2,color=FLG))
