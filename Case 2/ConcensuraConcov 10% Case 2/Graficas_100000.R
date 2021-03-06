#################################################################################################
################################SincensuraConcov#################################################
#################################################################################################
setwd("your directory")
require(dplyr)

# Plots -------------------------------------------------------------------
dt <- read.table('ConcensuraConcovCG_100000.txt', 
                 col.names=c('interceptx1', 'x1', #Coeficientes para CG()
                             'interceptx2', 'x2', #Coeficientes para CG()
                             'n'))

# MSE
res <- dt %>% group_by(n) %>% summarise(interceptx1=mean((1.5 - interceptx1)^2), 
                                        x1=mean((-1.5 - x1)^2),
                                        interceptx2=mean((2 - interceptx2)^2), 
                                        x2=mean((-2 - x2)^2))
                                        


pdf('mse_beta_ConcensuraConcov_100000.pdf', width=10, height=5)
par(mfrow=c(1, 2))
#interceptx1
plot(x=res$n, y=res$interceptx1, xlab="n", ylim=c(0, 15),
     type='b', las=1, ylab='MSE', main=expression(beta[0]))
abline(h=0, lty='dashed', col='red')
#x1
plot(x=res$n, y=res$x1, xlab="n", ylim=c(0, 15),
     type='b', las=1, ylab='MSE', main=expression(beta[1]))
abline(h=0, lty='dashed', col='red')
dev.off()

pdf('mse_gamma_ConcensuraConcov_100000.pdf', width=10, height=5)
par(mfrow=c(1, 2))
#interceptx2
plot(x=res$n, y=res$interceptx2, xlab="n", ylim=c(0, 8),
     type='b', las=1, ylab='MSE', main=expression(gamma[0]))
abline(h=0, lty='dashed', col='red')
#x1
plot(x=res$n, y=res$x2, xlab="n",  ylim=c(0, 8),
     type='b', las=1, ylab='MSE', main=expression(gamma[1]))
abline(h=0, lty='dashed', col='red')
dev.off()

# BIAS
res <- dt %>% group_by(n) %>% summarise(interceptx1=1.5 - mean(interceptx1), 
                                        x1=-1.5 - mean(x1),
                                        interceptx2=2 - mean(interceptx2), 
                                        x2=-2 - mean(x2))
                                        

pdf('bias_beta_ConcensuraConcov_100000.pdf', width=10, height=5)
par(mfrow=c(1, 2))
#interceptx1
plot(x=res$n, y=res$interceptx1, xlab="n", ylim=c(-0.15, 0),
     type='b', las=1, ylab='BIAS', main=expression(beta[0]))
abline(h=0, lty='dashed', col='red')
#x1
plot(x=res$n, y=res$x1, xlab="n", ylim=c(0, 0.15),
     type='b', las=1, ylab='BIAS', main=expression(beta[1]))
abline(h=0, lty='dashed', col='red')
dev.off()

pdf('bias_gamma_ConcensuraConcov_100000.pdf', width=10, height=5)
par(mfrow=c(1, 2))
#interceptx2
plot(x=res$n, y=res$interceptx2, xlab="n",  ylim=c(-0.09, 0.015),
     type='b', las=1, ylab='BIAS', main=expression(gamma[0]))
abline(h=0, lty='dashed', col='red')
#x1
plot(x=res$n, y=res$x2, xlab="n", ylim=c(-0.09, 0.015),
     type='b', las=1, ylab='BIAS', main=expression(gamma[1]))
abline(h=0, lty='dashed', col='red')
dev.off()

# MEAN
res <- dt %>% group_by(n) %>% summarise(interceptx1=mean(interceptx1), 
                                        x1=mean(x1),
                                        interceptx2=mean(interceptx2), 
                                        x2=mean(x2))


pdf('mean_beta_ConcensuraConcov_100000.pdf', width=10, height=5)
par(mfrow=c(1, 2))
#interceptx1
plot(x=res$n, y=res$interceptx1, xlab="n",  ylim=c(1.50, 1.65),
     type='b', las=1, ylab='MEAN', main=expression(beta[0]))
abline(h=1.5, lty='dashed', col='red')
#x1
plot(x=res$n, y=res$x1, xlab="n", ylim=c(-1.65, -1.50),
     type='b', las=1, ylab='MEAN', main=expression(beta[1]))
abline(h=-1.5, lty='dashed', col='red')
dev.off()

pdf('mean_gamma_ConcensuraConcov_100000.pdf', width=10, height=5)
par(mfrow=c(1, 2))
#interceptx2
plot(x=res$n, y=res$interceptx2, xlab="n", ylim=c(1.99, 2.09),
     type='b', las=1, ylab='MEAN', main=expression(gamma[0]))
abline(h=2, lty='dashed', col='red')
#x1
plot(x=res$n, y=res$x2, xlab="n", ylim=c(-2.09, -1.99),
     type='b', las=1, ylab='MEAN', main=expression(gamma[1]))
abline(h=-2, lty='dashed', col='red')
dev.off()

# VAR
res <- dt %>% group_by(n) %>% summarise(interceptx1=var(interceptx1), 
                                        x1=var(x1),
                                        interceptx2=var(interceptx2), 
                                        x2=var(x2))
                                    

pdf('var_beta_ConcensuraConcov_100000.pdf', width=10, height=5)
par(mfrow=c(1, 2))
#interceptx1
plot(x=res$n, y=res$interceptx1, xlab="n", ylim=c(0, 15),
     type='b', las=1, ylab='VAR', main=expression(beta[0]))
abline(h=0, lty='dashed', col='red')
#x1
plot(x=res$n, y=res$x1, xlab="n", ylim=c(0, 15),
     type='b', las=1, ylab='VAR', main=expression(beta[1]))
abline(h=0, lty='dashed', col='red')
dev.off()

pdf('var_gamma_ConcensuraConcov_100000.pdf', width=10, height=5)
par(mfrow=c(1, 2))
#interceptx2
plot(x=res$n, y=res$interceptx2, xlab="n", ylim=c(0, 8),
     type='b', las=1, ylab='VAR', main=expression(gamma[0]))
abline(h=0, lty='dashed', col='red')
#x1
plot(x=res$n, y=res$x2, xlab="n",  ylim=c(0, 8),
     type='b', las=1, ylab='VAR', main=expression(gamma[1]))
abline(h=0, lty='dashed', col='red')
dev.off()