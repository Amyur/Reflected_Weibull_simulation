#################################################################################################
################################SincensuraConcov#################################################
#################################################################################################
setwd("your directory")
require(dplyr)

# Plots -------------------------------------------------------------------
dt <- read.table('SincensuraConcovCG_100000.txt', 
                 col.names=c('interceptx1', 'x1', #Coeficientes para CG()
                             'interceptx2', 'x2', #Coeficientes para CG()
                             'n'))

# MSE
res <- dt %>% group_by(n) %>% summarise(interceptx1=mean((1.5 - interceptx1)^2), 
                                        x1=mean((-1.5 - x1)^2),
                                        interceptx2=mean((2 - interceptx2)^2), 
                                        x2=mean((-2 - x2)^2))
                                        


pdf('mse_SincensuraConcov_100000.pdf', width=12, height=8)
par(mfrow=c(2, 2))
#interceptx1
plot(x=res$n, y=res$interceptx1, xlab="n", ylim=c(0, 15),
     type='b', las=1, ylab='MSE', main=expression(beta[0]))
abline(h=0, lty='dashed', col='red')
#x1
plot(x=res$n, y=res$x1, xlab="n", ylim=c(0, 15),
     type='b', las=1, ylab='MSE', main=expression(beta[1]))
abline(h=0, lty='dashed', col='red')
#interceptx2
plot(x=res$n, y=res$interceptx2, xlab="n", ylim=c(0, 15),
     type='b', las=1, ylab='MSE', main=expression(gamma[0]))
abline(h=0, lty='dashed', col='red')
#x1
plot(x=res$n, y=res$x2, xlab="n", ylim=c(0, 15),
     type='b', las=1, ylab='MSE', main=expression(gamma[1]))
abline(h=0, lty='dashed', col='red')
dev.off()

# BIAS
res <- dt %>% group_by(n) %>% summarise(interceptx1=1.5 - mean(interceptx1), 
                                        x1=-1.5 - mean(x1),
                                        interceptx2=2 - mean(interceptx2), 
                                        x2=-2 - mean(x2))
                                        

pdf('bias_SincensuraConcov_100000.pdf', width=12, height=8)
par(mfrow=c(2, 2))
#interceptx1
plot(x=res$n, y=res$interceptx1, xlab="n", 
     type='b', las=1, ylab='BIAS', main=expression(beta[0]))
abline(h=0, lty='dashed', col='red')
#x1
plot(x=res$n, y=res$x1, xlab="n",  ylim=c(0, 0.15),
     type='b', las=1, ylab='BIAS', main=expression(beta[1]))
abline(h=0, lty='dashed', col='red')
#interceptx2
plot(x=res$n, y=res$interceptx2, xlab="n",  ylim=c(-0.15, 0),
     type='b', las=1, ylab='BIAS', main=expression(gamma[0]))
abline(h=0, lty='dashed', col='red')
#x1
plot(x=res$n, y=res$x2, xlab="n",  ylim=c(-0.1, 0.15),
     type='b', las=1, ylab='BIAS', main=expression(gamma[1]))
abline(h=0, lty='dashed', col='red')
dev.off()

# MEAN
res <- dt %>% group_by(n) %>% summarise(interceptx1=mean(interceptx1), 
                                        x1=mean(x1),
                                        interceptx2=mean(interceptx2), 
                                        x2=mean(x2))


pdf('mean_SincensuraConcov_100000.pdf', width=12, height=8)
par(mfrow=c(2, 2))
#interceptx1
plot(x=res$n, y=res$interceptx1, xlab="n", ylim=c(1.45, 1.65),
     type='b', las=1, ylab='MEAN', main=expression(beta[0]))
abline(h=1.5, lty='dashed', col='red')
#x1
plot(x=res$n, y=res$x1, xlab="n", ylim=c(-1.65, -1.45),
     type='b', las=1, ylab='MEAN', main=expression(beta[1]))
abline(h=-1.5, lty='dashed', col='red')
#interceptx2
plot(x=res$n, y=res$interceptx2, xlab="n", ylim=c(1.95, 2.1),
     type='b', las=1, ylab='MEAN', main=expression(gamma[0]))
abline(h=2, lty='dashed', col='red')
#x1
plot(x=res$n, y=res$x2, xlab="n",  ylim=c(-2.1, -1.95),
     type='b', las=1, ylab='MEAN', main=expression(gamma[1]))
abline(h=-2, lty='dashed', col='red')
dev.off()

# VAR
res <- dt %>% group_by(n) %>% summarise(interceptx1=var(interceptx1), 
                                        x1=var(x1),
                                        interceptx2=var(interceptx2), 
                                        x2=var(x2))
                                    

pdf('var_SincensuraConcov_100000.pdf', width=12, height=8)
par(mfrow=c(2, 2))
#interceptx1
plot(x=res$n, y=res$interceptx1, xlab="n", ylim=c(0, 15),
     type='b', las=1, ylab='VAR', main=expression(beta[0]))
abline(h=0, lty='dashed', col='red')
#x1
plot(x=res$n, y=res$x1, xlab="n", ylim=c(0, 15),
     type='b', las=1, ylab='VAR', main=expression(beta[1]))
abline(h=0, lty='dashed', col='red')
#interceptx2
plot(x=res$n, y=res$interceptx2, xlab="n",  ylim=c(0, 15),
     type='b', las=1, ylab='VAR', main=expression(gamma[0]))
abline(h=0, lty='dashed', col='red')
#x1
plot(x=res$n, y=res$x2, xlab="n",  ylim=c(0, 15),
     type='b', las=1, ylab='VAR', main=expression(gamma[1]))
abline(h=0, lty='dashed', col='red')
dev.off()