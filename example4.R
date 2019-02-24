rm(list=ls())
set.seed(20190225)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(MASS)
library(tidyverse)

k1 <- c(1,1,1,1,0,0,1,1,0,1,0,0,1,0,0,1,0,1,0,0,
        0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,1,0,0,0,1,1,0,0,0,0,1,0,0,0,0,0,0,0,
        0,0,0,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,
        1,0,1,1,0,1,1,1,0,1,0,0,1,0,0,0,0,1,0,0,
        1,1,0,1,0,0,0,1,0,1,0,1,1,0,0,1,0,1,0,0,
        0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,1,1,0,0,0,0,1,0,1,0,0,1,0,0,0,0,1,0,1,
        1,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0)

k <- matrix(k1, NROW=10, byrow=T)
np <- NROW(k)
nq <- NCOL(k)
dataset <- list(N=np,M=nq,K=k)
model4.1 <- stan_model("TwentyQuestions.stan")
fit1 <- sampling(model4.1,dataset)
print(fit1,pars=c("p"))
print(fit1,pars=c("q"))


k %>% as.data.frame() %>% 
  dplyr::mutate(.,id=rownames(.)) %>% 
  tidyr::gather(key,val,-id,na.rm=T,factor_key=T) -> k.set

dataset <- list(L=nrow(k.set),N=10,M=20,
                Pid=as.numeric(k.set$id),
                Qid=as.numeric(k.set$key),
                resp=k.set$val)

model4.2 <- stan_model("TwentyQuestions2.stan",model_name="logistic func")
fit2 <- sampling(model4.2,dataset)
print(fit2,pars="p")
print(fit2,pars="q")


k2 <- c(1,1,1,1,0,0,1,1,0,1,0,0,NA,0,0,1,0,1,0,0,
        0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,1,0,0,0,1,1,0,0,0,0,1,0,0,0,0,0,0,0,
        0,0,0,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,
        1,0,1,1,0,1,1,1,0,1,0,0,1,0,0,0,0,1,0,0,
        1,1,0,1,0,0,0,1,0,1,0,1,1,0,0,1,0,1,0,0,
        0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
        0,0,0,0,NA,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,1,1,0,0,0,0,1,0,1,0,0,1,0,0,0,0,1,0,1,
        1,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,NA,0,0)
k <- matrix(k2, nrow=10, byrow=T)
np <- nrow(k)
nq <- ncol(k)


k %>% as.data.frame() %>% dplyr::mutate(.,id=rownames(.)) %>%
  tidyr::gather(key,val,-id,na.rm=T,factor_key=T) -> k.set

dataset <- list(L=nrow(k.set),N=10,M=20,
                Pid=as.numeric(k.set$id),
                Qid=as.numeric(k.set$key),resp=k.set$val)
model4.3 <- stan_model("TwentyQuestions2.stan",model_name="2PL IRT")
fit3 <- sampling(model4.2,dataset)
print(fit3,pars="p")

