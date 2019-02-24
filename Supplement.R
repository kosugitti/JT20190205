# 環境設定 --------------------------------------------------------------------
rm(list=ls())
source("~/Dropbox/Utilities/distribution_diagrams-master/plot_dist.R")

# Example1 ----------------------------------------------------------------

png("example1_1.png",width=300,height=150)
plot_dist(dists$normal,labels=c(mean=expression(mu)))
dev.off()


# wide-normal
wnormal <- dists$normal
k <- 1
png("example1_2.png",width = 300,height = 150)
for(i in seq(0.55,3,0.75)){
  k <- k+1
  wnormal$ddist_params$sd <- i
  plot_dist(wnormal,labels=c(mean=expression(mu)))
  par(new=T)
}
dev.off()

png("example1_3.png",width = 300,height = 150)
plot_dist(dists$normal,labels=c(mean=expression(mu),right_sd=expression(sigma[i])))
dev.off()

png("example1_4.png",width = 300,height = 150)
plot_dist(dists$normal,labels=c(mean=0,right_sd=expression(100)))
dev.off()

png("example1_5.png",width = 300,height = 150)
plot_dist(dists$half_cauchy,labels=c(scale=5))
dev.off()


# Example2 ----------------------------------------------------------------
library(MASS)
N <- 200
rho <- 0.8
mu <- c(0, 0)
sd1 <- 1
sd2 <- 1
cov <- matrix(ncol = 2, nrow = 2)
cov[1, 1] <- sd1^2
cov[2, 2] <- sd2^2
cov[1, 2] <- sd1 * sd2 * rho
cov[2, 1] <- sd1 * sd2 * rho
df <- data.frame(mvrnorm(N, mu, cov, empirical = TRUE))
p <- ggplot(df,aes(x = X1, y = X2)) + geom_point()+xlab("D")+ylab("Y")
ggsave(file = "example2_1.png", plot = p, dpi = 100, width = 3, height = 3)

png("example2_2.png",width = 300,height = 150)
plot_dist(dists$normal,labels=c(mean=expression(mu[D]),right_sd=expression(sigma[D])))
dev.off()
png("example2_3.png",width = 300,height = 150)
plot_dist(dists$normal,labels=c(mean=expression(mu[y]),right_sd=expression(sigma[y])))
dev.off()
png("example2_4.png",width = 300,height = 150)
plot_dist(dists$normal,labels=c(mean=expression(mu[x]),right_sd=expression(sigma[x])))
dev.off()
png("example2_5.png",width = 300,height = 150)
plot_dist(dists$normal,labels=c(mean=expression(0),right_sd=expression(2.5)))
dev.off()
png("example2_6.png",width = 300,height = 150)
plot_dist(dists$normal,labels=c(mean=expression(0),right_sd=expression(1000)))
dev.off()
# Example3 ----------------------------------------------------------------

png("example3_1.png",width = 300,height = 150)
plot_dist(dists$normal,labels=c(mean=expression(mu[1]),right_sd=expression(sigma[1])))
dev.off()

png("example3_2.png",width = 300,height = 150)
plot_dist(dists$normal,labels=c(mean=expression(mu[2]),right_sd=expression(sigma[2])))
dev.off()


library(MASS)
N <- 200
rho <- 0.8
mu <- c(0, 0)
sd1 <- 1
sd2 <- 1
cov <- matrix(ncol = 2, nrow = 2)
cov[1, 1] <- sd1^2
cov[2, 2] <- sd2^2
cov[1, 2] <- sd1 * sd2 * rho
cov[2, 1] <- sd1 * sd2 * rho
df <- data.frame(mvrnorm(N, mu, cov, empirical = TRUE))
p <- ggplot(df,aes(x = X1, y = X2)) + geom_point()+xlab("X1")+ylab("X2")
ggsave(file = "example3_3.png", plot = p, dpi = 100, width = 3, height = 3)


# Example4 ----------------------------------------------------------------


png("example4_1.png",width=300,height=150)
plot_dist(dists$bernoulli,labels = c(p=expression(theta[ij])))
dev.off()

beta11 <- dists$beta
beta11$ddist_params$shape1 <- 1
beta11$ddist_params$shape2 <- 1
png("example4_2.png",width=300,height=150)
plot_dist(beta11,labels = c(params = "1, 1"))
dev.off()

png("example4_3.png",width=300,height=150)
plot_dist(dists$normal,labels = c(mean=0,right_sd=1))
dev.off()

png("example4_4.png",width=300,height=150)
plot_dist(dists$normal,labels = c(mean=0,right_sd=5))
dev.off()

logistic3PL <- function(x,a=1,b=0,c=0){
  return(c + (1-c) / (1+exp(-1.7*a*(x-b))))
}


g1 <- ggplot(data = data.frame(X = c(0, 100)), aes(x = X)) + xlim(-4,4)+
  stat_function(fun = logistic3PL, args = list(a = 1, b = 0), color="black")
g2 <- g1 + stat_function(fun = logistic3PL, args = list(a = 0.5, b = 0), color="red")
g3 <- g2 + stat_function(fun = logistic3PL, args = list(a = 1.5, b = 0), color="red")
g4 <- g1 + stat_function(fun = logistic3PL, args = list(a = 1, b = -1), color="blue")
g5 <- g4 + stat_function(fun = logistic3PL, args = list(a = 1, b = 2), color="blue")
g6 <- g1 + stat_function(fun = logistic3PL, args = list(a = 1, b = 0, c=0.3), color="purple")

ggsave(file = "example4_5.png", plot = g1, dpi = 100, width = 6, height = 3)
ggsave(file = "example4_6.png", plot = g2, dpi = 100, width = 6, height = 3)
ggsave(file = "example4_7.png", plot = g3, dpi = 100, width = 6, height = 3)
ggsave(file = "example4_8.png", plot = g4, dpi = 100, width = 6, height = 3)
ggsave(file = "example4_9.png", plot = g5, dpi = 100, width = 6, height = 3)
ggsave(file = "example4_10.png", plot = g6, dpi = 100, width = 6, height = 3)

# Example5 ----------------------------------------------------------------

dat <- read_csv("weight.csv")
ggplot(dat,aes(x=1:NROW(dat),y=weight))+geom_point()+geom_line()+  xlab("Date")+ylab("Weight")

png("example5_1.png",width=300,height=150)
plot_dist(dists$normal,labels = c(mean=expression(mu[1]),right_sd=expression(sigma)))
dev.off()
png("example5_2.png",width=300,height=150)
plot_dist(dists$normal,labels = c(mean=expression(mu[2]),right_sd=expression(sigma)))
dev.off()
png("example5_3.png",width=300,height=150)
plot_dist(dists$normal,labels = c(mean=expression(mu[3]),right_sd=expression(sigma)))
dev.off()
png("example5_4.png",width=300,height=150)
plot_dist(dists$normal,labels = c(mean=expression(mu[1]),right_sd=expression(tau)))
dev.off()
png("example5_5.png",width=300,height=150)
plot_dist(dists$normal,labels = c(mean=expression(mu[2]),right_sd=expression(tau)))
dev.off()

