#################################################################################################
################################SincensuraConcov#################################################
#################################################################################################
setwd("your directory")
require(dplyr)

true_mu = 1
true_sigma = 1

# Plots -------------------------------------------------------------------
dt <- read.table('ConcensuraSincovCG_50%_100000.txt', 
                 col.names=c('mu', 'sigma', 'n'))

# MSE
res <- dt %>% group_by(n) %>% summarise(mu=mean((true_mu - mu)^2), 
                                        sigma=mean((true_sigma - sigma)^2))
                                        


pdf('mse_ConcensuraSincov_100000.pdf', width=9, height=5)
par(mfrow=c(1, 2))
#mu
plot(x=res$n, y=res$mu, xlab="n", ylim=c(0, 0.08),
     type='b', las=1, ylab='MSE', main=expression(mu))
abline(h=0, lty='dashed', col='red')
#sigma
plot(x=res$n, y=res$sigma, xlab="n", ylim=c(0, 0.08),
     type='b', las=1, ylab='MSE', main=expression(sigma))
abline(h=0, lty='dashed', col='red')
dev.off()

# BIAS
res <- dt %>% group_by(n) %>% summarise(mu=true_mu - mean(mu), 
                                        sigma=true_sigma - mean(sigma))
                                        

pdf('bias_ConcensuraSincov_100000.pdf', width=9, height=5)
par(mfrow=c(1, 2))
#mu
plot(x=res$n, y=res$mu, xlab="n", ylim=c(-0.06, 0),
     type='b', las=1, ylab='bias', main=expression(mu))
abline(h=0, lty='dashed', col='red')
#sigma
plot(x=res$n, y=res$sigma, xlab="n", ylim=c(-0.06, 0),
     type='b', las=1, ylab='bias', main=expression(sigma))
abline(h=0, lty='dashed', col='red')
dev.off()

# MEAN
res <- dt %>% group_by(n) %>% summarise(mu=mean(mu), 
                                        sigma=mean(sigma))

pdf('mean_ConcensuraSincov_100000.pdf', width=9, height=5)
par(mfrow=c(1, 2))
#mu
plot(x=res$n, y=res$mu, xlab="n", ylim=c(1, 1.06),
     type='b', las=1, ylab='mean', main=expression(mu))
abline(h=1, lty='dashed', col='red')
#sigma
plot(x=res$n, y=res$sigma, xlab="n", ylim=c(1, 1.06), 
     type='b', las=1, ylab='mean', main=expression(sigma))
abline(h=1, lty='dashed', col='red')
dev.off()

# VAR
res <- dt %>% group_by(n) %>% summarise(mu=var(mu), 
                                        sigma=var(sigma))
                                    

pdf('var_ConcensuraSincov_100000.pdf', width=9, height=5)
par(mfrow=c(1, 2))
#mu
plot(x=res$n, y=res$mu, xlab="n", ylim=c(0, 0.08),
     type='b', las=1, ylab='VAR', main=expression(mu))
abline(h=0, lty='dashed', col='red')
#sigma
plot(x=res$n, y=res$sigma, xlab="n", ylim=c(0, 0.08),
     type='b', las=1, ylab='VAR', main=expression(sigma))
abline(h=0, lty='dashed', col='red')
dev.off()

