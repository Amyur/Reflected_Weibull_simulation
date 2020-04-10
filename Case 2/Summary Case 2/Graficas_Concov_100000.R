#################################################################################################
################################SincensuraConcov#################################################
#################################################################################################
setwd("your directory")
require(dplyr)

# Plots -------------------------------------------------------------------
dt1 <- read.table('SincensuraConcovCG_100000.txt', 
                 col.names=c('interceptx1', 'x1', #Coeficientes para CG()
                             'interceptx2', 'x2', #Coeficientes para CG()
                             'n'))
dt2 <- read.table('ConcensuraConcovCG_10%_100000.txt', 
                 col.names=c('interceptx1', 'x1', #Coeficientes para CG()
                             'interceptx2', 'x2', #Coeficientes para CG()
                             'n'))
dt3 <- read.table('ConcensuraConcovCG_30%_100000.txt', 
                 col.names=c('interceptx1', 'x1', #Coeficientes para CG()
                             'interceptx2', 'x2', #Coeficientes para CG()
                             'n'))
dt4 <- read.table('ConcensuraConcovCG_50%_100000.txt', 
                 col.names=c('interceptx1', 'x1', #Coeficientes para CG()
                             'interceptx2', 'x2', #Coeficientes para CG()
                             'n'))

# MSE
res1 <- dt1 %>% group_by(n) %>% summarise(interceptx1=mean((1.5 - interceptx1)^2), 
                                        x1=mean((-1.5 - x1)^2),
                                        interceptx2=mean((2 - interceptx2)^2), 
                                        x2=mean((-2 - x2)^2))
res2 <- dt2 %>% group_by(n) %>% summarise(interceptx1=mean((1.5 - interceptx1)^2), 
                                        x1=mean((-1.5 - x1)^2),
                                        interceptx2=mean((2 - interceptx2)^2), 
                                        x2=mean((-2 - x2)^2))
res3 <- dt3 %>% group_by(n) %>% summarise(interceptx1=mean((1.5 - interceptx1)^2), 
                                        x1=mean((-1.5 - x1)^2),
                                        interceptx2=mean((2 - interceptx2)^2), 
                                        x2=mean((-2 - x2)^2))
res4 <- dt4 %>% group_by(n) %>% summarise(interceptx1=mean((1.5 - interceptx1)^2), 
                                        x1=mean((-1.5 - x1)^2),
                                        interceptx2=mean((2 - interceptx2)^2), 
                                        x2=mean((-2 - x2)^2))
                                        


pdf('mse_beta_ConcensuraConcov_unificado_100000.pdf', width=10, height=5)
par(mfrow=c(1, 2))
#interceptx1
plot(x=res1$n, y=res1$interceptx1, xlab="n", cex.axis=0.8, ylim=c(0, 15),
     type='b', las=1, ylab='MSE', main=expression(beta[0]), pch=1)
lines(x=res2$n, y=res2$interceptx1, type="b", pch=2)
lines(x=res3$n, y=res3$interceptx1, type="b", pch=0)
lines(x=res4$n, y=res4$interceptx1, type="b", pch=5)
legend("topright", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=0, lty='dashed', col='red')
#x1
plot(x=res1$n, y=res1$x1, xlab="n", cex.axis=0.8, ylim=c(0, 15),
     type='b', las=1, ylab='MSE', main=expression(beta[1]), pch=1)
lines(x=res2$n, y=res2$x1, type="b", pch=2)
lines(x=res3$n, y=res3$x1, type="b", pch=0)
lines(x=res4$n, y=res4$x1, type="b", pch=5)
legend("topright", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=0, lty='dashed', col='red')
dev.off()

pdf('mse_gamma_ConcensuraConcov_unificado_100000.pdf', width=10, height=5)
par(mfrow=c(1, 2))
#interceptx2
plot(x=res1$n, y=res1$interceptx2, xlab="n", cex.axis=0.8, ylim=c(0, 7.5),
     type='b', las=1, ylab='MSE', main=expression(gamma[0]), pch=1)
lines(x=res2$n, y=res2$interceptx2, type="b", pch=2)
lines(x=res3$n, y=res3$interceptx2, type="b", pch=0)
lines(x=res4$n, y=res4$interceptx2, type="b", pch=5)
legend("topright", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=0, lty='dashed', col='red')
#x2
plot(x=res1$n, y=res1$x2, xlab="n", cex.axis=0.8, ylim=c(0, 7.5),
     type='b', las=1, ylab='MSE', main=expression(gamma[1]), pch=1)
lines(x=res2$n, y=res2$x2, type="b", pch=2)
lines(x=res3$n, y=res3$x2, type="b", pch=0)
lines(x=res4$n, y=res4$x2, type="b", pch=5)
legend("topright", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=0, lty='dashed', col='red')
dev.off()

# BIAS
res1 <- dt1 %>% group_by(n) %>% summarise(interceptx1=1.5 - mean(interceptx1), 
                                        x1=-1.5 - mean(x1),
                                        interceptx2=2 - mean(interceptx2), 
                                        x2=-2 - mean(x2))
res2 <- dt2 %>% group_by(n) %>% summarise(interceptx1=1.5 - mean(interceptx1), 
                                        x1=-1.5 - mean(x1),
                                        interceptx2=2 - mean(interceptx2), 
                                        x2=-2 - mean(x2))
res3 <- dt3 %>% group_by(n) %>% summarise(interceptx1=1.5 - mean(interceptx1), 
                                        x1=-1.5 - mean(x1),
                                        interceptx2=2 - mean(interceptx2), 
                                        x2=-2 - mean(x2))
res4 <- dt4 %>% group_by(n) %>% summarise(interceptx1=1.5 - mean(interceptx1), 
                                        x1=-1.5 - mean(x1),
                                        interceptx2=2 - mean(interceptx2), 
                                        x2=-2 - mean(x2))
                                        

pdf('bias_beta_ConcensuraConcov_unificado_100000.pdf', width=10, height=5)
par(mfrow=c(1, 2))
#interceptx1
plot(x=res1$n, y=res1$interceptx1, xlab="n", cex.axis=0.8, ylim=c(-0.15, 0.02),
     type='b', las=1, ylab='Bias', main=expression(beta[0]), pch=1)
lines(x=res2$n, y=res2$interceptx1, type="b", pch=2)
lines(x=res3$n, y=res3$interceptx1, type="b", pch=0)
lines(x=res4$n, y=res4$interceptx1, type="b", pch=5)
legend("right", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=0, lty='dashed', col='red')
#x1
plot(x=res1$n, y=res1$x1, xlab="n", cex.axis=0.8, ylim=c(-0.02, 0.15),
     type='b', las=1, ylab='Bias', main=expression(beta[1]), pch=1)
lines(x=res2$n, y=res2$x1, type="b", pch=2)
lines(x=res3$n, y=res3$x1, type="b", pch=0)
lines(x=res4$n, y=res4$x1, type="b", pch=5)
legend("right", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=0, lty='dashed', col='red')
dev.off()

pdf('bias_gamma_ConcensuraConcov_unificado_100000.pdf', width=10, height=5)
par(mfrow=c(1, 2))
#interceptx2
plot(x=res1$n, y=res1$interceptx2, xlab="n", cex.axis=0.8, ylim=c(-0.085, 0.008),
     type='b', las=1, ylab='Bias', main=expression(gamma[0]), pch=1)
lines(x=res2$n, y=res2$interceptx2, type="b", pch=2)
lines(x=res3$n, y=res3$interceptx2, type="b", pch=0)
lines(x=res4$n, y=res4$interceptx2, type="b", pch=5)
legend("right", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=0, lty='dashed', col='red')
#x2
plot(x=res1$n, y=res1$x2, xlab="n", cex.axis=0.8, ylim=c(-0.008, 0.085),
     type='b', las=1, ylab='Bias', main=expression(gamma[1]), pch=1)
lines(x=res2$n, y=res2$x2, type="b", pch=2)
lines(x=res3$n, y=res3$x2, type="b", pch=0)
lines(x=res4$n, y=res4$x2, type="b", pch=5)
legend("right", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=0, lty='dashed', col='red')
dev.off()

# MEAN
res1 <- dt1 %>% group_by(n) %>% summarise(interceptx1=mean(interceptx1), 
                                        x1=mean(x1),
                                        interceptx2=mean(interceptx2), 
                                        x2=mean(x2))
res2 <- dt2 %>% group_by(n) %>% summarise(interceptx1=mean(interceptx1), 
                                        x1=mean(x1),
                                        interceptx2=mean(interceptx2), 
                                        x2=mean(x2))
res3 <- dt3 %>% group_by(n) %>% summarise(interceptx1=mean(interceptx1), 
                                        x1=mean(x1),
                                        interceptx2=mean(interceptx2), 
                                        x2=mean(x2))
res4 <- dt4 %>% group_by(n) %>% summarise(interceptx1=mean(interceptx1), 
                                        x1=mean(x1),
                                        interceptx2=mean(interceptx2), 
                                        x2=mean(x2))


pdf('mean_beta_ConcensuraConcov_unificado_100000.pdf', width=10, height=5)
par(mfrow=c(1, 2))
#interceptx1
plot(x=res1$n, y=res1$interceptx1, xlab="n", cex.axis=0.8, ylim=c(1.47, 1.65),
     type='b', las=1, ylab='Mean', main=expression(beta[0]), pch=1)
lines(x=res2$n, y=res2$interceptx1, type="b", pch=2)
lines(x=res3$n, y=res3$interceptx1, type="b", pch=0)
lines(x=res4$n, y=res4$interceptx1, type="b", pch=5)
legend("right", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=1.5, lty='dashed', col='red')
#x1
plot(x=res1$n, y=res1$x1, xlab="n", cex.axis=0.8, ylim=c(-1.65, -1.47),
     type='b', las=1, ylab='Mean', main=expression(beta[1]), pch=1)
lines(x=res2$n, y=res2$x1, type="b", pch=2)
lines(x=res3$n, y=res3$x1, type="b", pch=0)
lines(x=res4$n, y=res4$x1, type="b", pch=5)
legend("right", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=-1.5, lty='dashed', col='red')
dev.off()

pdf('mean_gamma_ConcensuraConcov_unificado_100000.pdf', width=10, height=5)
par(mfrow=c(1, 2))
#interceptx2
plot(x=res1$n, y=res1$interceptx2, xlab="n", cex.axis=0.8, ylim=c(1.99, 2.09),
     type='b', las=1, ylab='Mean', main=expression(gamma[0]), pch=1)
lines(x=res2$n, y=res2$interceptx2, type="b", pch=2)
lines(x=res3$n, y=res3$interceptx2, type="b", pch=0)
lines(x=res4$n, y=res4$interceptx2, type="b", pch=5)
legend("right", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=2, lty='dashed', col='red')
#x2
plot(x=res1$n, y=res1$x2, xlab="n", cex.axis=0.8, ylim=c(-2.09, -1.99),
     type='b', las=1, ylab='Mean', main=expression(gamma[1]), pch=1)
lines(x=res2$n, y=res2$x2, type="b", pch=2)
lines(x=res3$n, y=res3$x2, type="b", pch=0)
lines(x=res4$n, y=res4$x2, type="b", pch=5)
legend("right", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=-2, lty='dashed', col='red')
dev.off()

# VAR
res1 <- dt1 %>% group_by(n) %>% summarise(interceptx1=var(interceptx1), 
                                        x1=var(x1),
                                        interceptx2=var(interceptx2), 
                                        x2=var(x2))
res2 <- dt2 %>% group_by(n) %>% summarise(interceptx1=var(interceptx1), 
                                        x1=var(x1),
                                        interceptx2=var(interceptx2), 
                                        x2=var(x2))
res3 <- dt3 %>% group_by(n) %>% summarise(interceptx1=var(interceptx1), 
                                        x1=var(x1),
                                        interceptx2=var(interceptx2), 
                                        x2=var(x2))
res4 <- dt4 %>% group_by(n) %>% summarise(interceptx1=var(interceptx1), 
                                        x1=var(x1),
                                        interceptx2=var(interceptx2), 
                                        x2=var(x2))
                                    

pdf('var_beta_ConcensuraConcov_unificado_100000.pdf', width=10, height=5)
par(mfrow=c(1, 2))
#interceptx1
plot(x=res1$n, y=res1$interceptx1, xlab="n", cex.axis=0.8, ylim=c(0, 15),
     type='b', las=1, ylab='Variance', main=expression(beta[0]), pch=1)
lines(x=res2$n, y=res2$interceptx1, type="b", pch=2)
lines(x=res3$n, y=res3$interceptx1, type="b", pch=0)
lines(x=res4$n, y=res4$interceptx1, type="b", pch=5)
legend("right", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=0, lty='dashed', col='red')
#x1
plot(x=res1$n, y=res1$x1, xlab="n", cex.axis=0.8, ylim=c(0, 15),
     type='b', las=1, ylab='Variance', main=expression(beta[1]), pch=1)
lines(x=res2$n, y=res2$x1, type="b", pch=2)
lines(x=res3$n, y=res3$x1, type="b", pch=0)
lines(x=res4$n, y=res4$x1, type="b", pch=5)
legend("right", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=0, lty='dashed', col='red')
dev.off()

pdf('var_gamma_ConcensuraConcov_unificado_100000.pdf', width=10, height=5)
par(mfrow=c(1, 2))
#interceptx2
plot(x=res1$n, y=res1$interceptx2, xlab="n", cex.axis=0.8, ylim=c(0, 7.5),
     type='b', las=1, ylab='Variance', main=expression(gamma[0]), pch=1)
lines(x=res2$n, y=res2$interceptx2, type="b", pch=2)
lines(x=res3$n, y=res3$interceptx2, type="b", pch=0)
lines(x=res4$n, y=res4$interceptx2, type="b", pch=5)
legend("right", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=0, lty='dashed', col='red')
#x2
plot(x=res1$n, y=res1$x2, xlab="n", cex.axis=0.8, ylim=c(0, 7.5),
     type='b', las=1, ylab='Variance', main=expression(gamma[1]), pch=1)
lines(x=res2$n, y=res2$x2, type="b", pch=2)
lines(x=res3$n, y=res3$x2, type="b", pch=0)
lines(x=res4$n, y=res4$x2, type="b", pch=5)
legend("right", legend=c("0%", "10%", "30%", "50%"), cex=1.2, bty="n", pch=c(1, 2, 0, 5))
abline(h=0, lty='dashed', col='red')
dev.off()