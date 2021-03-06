#################################################################################################
#################################################################################################
#################################################################################################
setwd("your directory")
require(dplyr)

true_mu = 1
true_sigma = 1

# Plots -------------------------------------------------------------------
dt1 <- read.table('SincensuraSincovCG_100000.txt', 
                 col.names=c('mu', 'sigma', 'n'))

dt2 <- read.table('ConcensuraSincovCG_10%_100000.txt', 
                  col.names=c('mu', 'sigma', 'n'))

dt3 <- read.table('ConcensuraSincovCG_30%_100000.txt', 
                  col.names=c('mu', 'sigma', 'n'))

dt4 <- read.table('ConcensuraSincovCG_50%_100000.txt', 
                  col.names=c('mu', 'sigma', 'n'))

# MSE
res1 <- dt1 %>% group_by(n) %>% summarise(mu=mean((true_mu - mu)^2), 
                                        sigma=mean((true_sigma - sigma)^2))

res2 <- dt2 %>% group_by(n) %>% summarise(mu=mean((true_mu - mu)^2), 
                                        sigma=mean((true_sigma - sigma)^2))

res3 <- dt3 %>% group_by(n) %>% summarise(mu=mean((true_mu - mu)^2), 
                                        sigma=mean((true_sigma - sigma)^2))

res4 <- dt4 %>% group_by(n) %>% summarise(mu=mean((true_mu - mu)^2), 
                                        sigma=mean((true_sigma - sigma)^2))
                                        


pdf('mse_ConcensuraSincov_unificado_100000.pdf', width=9, height=5)
par(mfrow=c(1, 2))
#mu
plot(x=res1$n, y=res1$mu, xlab="n", ylim=c(0, 0.085), cex.axis=0.8,
     type='b', las=1, ylab='MSE', main=expression(mu), pch=1)
lines(x=res2$n, y=res2$mu, type="b", pch=2)
lines(x=res3$n, y=res3$mu, type="b", pch=0)
lines(x=res4$n, y=res4$mu, type="b", pch=5)
legend("topright", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=0, lty='dashed', col='red')
#sigma
plot(x=res1$n, y=res1$sigma, xlab="n", ylim=c(0, 0.085), cex.axis=0.8,
     type='b', las=1, ylab='MSE', main=expression(sigma), pch=1)
lines(x=res2$n, y=res2$sigma, type="b", pch=2)
lines(x=res3$n, y=res3$sigma, type="b", pch=0)
lines(x=res4$n, y=res4$sigma, type="b", pch=5)
legend("topright", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=0, lty='dashed', col='red')
dev.off()

png(file="RW_mu.png",width=800, height=600,res=120)
#mu
plot(x=res1$n, y=res1$mu, xlab="n", ylim=c(0, 0.085), cex.axis=0.8, cex.axis=1, cex.lab=1.3,
     type='b', las=1, ylab='MSE', main=expression(mu), pch=1, lwd=3, col="black")
lines(x=res2$n, y=res2$mu, type="b", pch=1, col="red", lwd=3)
lines(x=res3$n, y=res3$mu, type="b", pch=1, col="green", lwd=3)
lines(x=res4$n, y=res4$mu, type="b", pch=1, col="blue", lwd=3)
op <- par(cex = 1.5)
legend("topright", legend=c("0%", "10%", "30%", "50%"), cex=0.8, bty="n", pch=c(1, 1, 1, 1), col=c("black", "red", "green", "blue"), lwd=3)
abline(h=0, lty='dashed', col='red')
dev.off()

png(file="RW_sigma.png",width=800, height=600,res=120)
#sigma
plot(x=res1$n, y=res1$sigma, xlab="n", ylim=c(0, 0.085), cex.axis=0.8, cex.axis=1, cex.lab=1.3,
     type='b', las=1, ylab='MSE', main=expression(sigma), pch=1, lwd=3)
lines(x=res2$n, y=res2$sigma, type="b", pch=1, col="red",lwd=3)
lines(x=res3$n, y=res3$sigma, type="b", pch=1, col="green",lwd=3)
lines(x=res4$n, y=res4$sigma, type="b", pch=1, col="blue",lwd=3)
op <- par(cex = 1.5)
legend("topright", legend=c("0%", "10%", "30%", "50%"), cex=0.8, bty="n", pch=c(1, 1, 1, 1), col=c("black", "red", "green", "blue"), lwd=3)
abline(h=0, lty='dashed', col='red')
dev.off()


# BIAS
res1 <- dt1 %>% group_by(n) %>% summarise(mu=true_mu - mean(mu), 
                                        sigma=true_sigma - mean(sigma))
res2 <- dt2 %>% group_by(n) %>% summarise(mu=true_mu - mean(mu), 
                                        sigma=true_sigma - mean(sigma))
res3 <- dt3 %>% group_by(n) %>% summarise(mu=true_mu - mean(mu), 
                                        sigma=true_sigma - mean(sigma))
res4 <- dt4 %>% group_by(n) %>% summarise(mu=true_mu - mean(mu), 
                                        sigma=true_sigma - mean(sigma))
                                        

pdf('bias_ConcensuraSincov_unificado_100000.pdf', width=9, height=5)
par(mfrow=c(1, 2))
#mu
plot(x=res1$n, y=res1$mu, xlab="n", pch=1, cex.axis=0.8, ylim=c(-0.065, 0),
     type='b', las=1, ylab='Bias', main=expression(mu))
lines(x=res2$n, y=res2$mu, type="b", pch=2)
lines(x=res3$n, y=res3$mu, type="b", pch=0)
lines(x=res4$n, y=res4$mu, type="b", pch=5)
legend("right", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=0, lty='dashed', col='red')
#sigma
plot(x=res1$n, y=res1$sigma, xlab="n", cex.axis=0.8, ylim=c(-0.065, 0),
     type='b', las=1, ylab='Bias', main=expression(sigma), pch=1)
lines(x=res2$n, y=res2$sigma, type="b", pch=2)
lines(x=res3$n, y=res3$sigma, type="b", pch=0)
lines(x=res4$n, y=res4$sigma, type="b", pch=5)
legend("right", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=0, lty='dashed', col='red')
dev.off()

# MEAN
res1 <- dt1 %>% group_by(n) %>% summarise(mu=mean(mu), 
                                        sigma=mean(sigma))
res2 <- dt2 %>% group_by(n) %>% summarise(mu=mean(mu), 
                                        sigma=mean(sigma))
res3 <- dt3 %>% group_by(n) %>% summarise(mu=mean(mu), 
                                        sigma=mean(sigma))
res4 <- dt4 %>% group_by(n) %>% summarise(mu=mean(mu), 
                                        sigma=mean(sigma))

pdf('mean_ConcensuraSincov_unificado_100000.pdf', width=9, height=5)
par(mfrow=c(1, 2))
#mu
plot(x=res1$n, y=res1$mu, xlab="n", cex.axis=0.8, ylim=c(1, 1.065),
     type='b', las=1, ylab='Mean', main=expression(mu), pch=1)
lines(x=res2$n, y=res2$mu, type="b", pch=2)
lines(x=res3$n, y=res3$mu, type="b", pch=0)
lines(x=res4$n, y=res4$mu, type="b", pch=5)
legend("topright", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=1, lty='dashed', col='red')
#sigma
plot(x=res1$n, y=res1$sigma, xlab="n",  cex.axis=0.8, ylim=c(1, 1.065),
     type='b', las=1, ylab='Mean', main=expression(sigma), pch=1)
lines(x=res2$n, y=res2$sigma, type="b", pch=2)
lines(x=res3$n, y=res3$sigma, type="b", pch=0)
lines(x=res4$n, y=res4$sigma, type="b", pch=5)
legend("topright", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=1, lty='dashed', col='red')
dev.off()

# VAR
res1 <- dt1 %>% group_by(n) %>% summarise(mu=var(mu), 
                                        sigma=var(sigma))
res2 <- dt2 %>% group_by(n) %>% summarise(mu=var(mu), 
                                        sigma=var(sigma))
res3 <- dt3 %>% group_by(n) %>% summarise(mu=var(mu), 
                                        sigma=var(sigma))
res4 <- dt4 %>% group_by(n) %>% summarise(mu=var(mu), 
                                        sigma=var(sigma))
                                    

pdf('var_ConcensuraSincov_unificado_100000.pdf', width=9, height=5)
par(mfrow=c(1, 2))
#mu
plot(x=res1$n, y=res1$mu, xlab="n", ylim=c(0, 0.085), cex.axis=0.8, 
     type='b', las=1, ylab='Variance', main=expression(mu), pch=1)
lines(x=res2$n, y=res2$mu, type="b", pch=2)
lines(x=res3$n, y=res3$mu, type="b", pch=0)
lines(x=res4$n, y=res4$mu, type="b", pch=5)
legend("topright", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=0, lty='dashed', col='red')
#sigma
plot(x=res1$n, y=res1$sigma, xlab="n", ylim=c(0, 0.085), cex.axis=0.8, 
     type='b', las=1, ylab='Variance', main=expression(sigma), pch=1)
lines(x=res2$n, y=res2$sigma, type="b", pch=2)
lines(x=res3$n, y=res3$sigma, type="b", pch=0)
lines(x=res4$n, y=res4$sigma, type="b", pch=5)
legend("topright", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=0, lty='dashed', col='red')
dev.off()

