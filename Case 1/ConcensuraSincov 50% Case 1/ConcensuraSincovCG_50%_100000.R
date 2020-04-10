require(RelDists)
require(gamlss)
require(gamlss.cens)

setwd("your directory")

# The parameters ----------------------------------------------------------
true_mu    <- 1
true_sigma <- 1

# Se genera la familia de funciones de RW censurada por la izquierda llamada RWrc
gen.cens(family="RW", type="left")

# Useful functions to the simulation study --------------------------------

# Funcion para obtener mu_hat, sigma_hat y nu_hat para un valor fijo de n
simul_one <- function(size) {
  y <- rRW(n=size, mu=true_mu, sigma=true_sigma)
  
  #Se aplica censura reemplazando los valores mayores 
  corte <- quantile(y, probs=0.50)
  y <- ifelse(y <= corte, corte, y)
  status <- ifelse(y == corte, 1, 2)
  
  x <- Surv(y, status, type="left")
  
  
  mod <- NULL
  mod <- gamlss(x~1, sigma.fo=~1, family='RWlc',
                control=gamlss.control(n.cyc=100000, trace=FALSE), method=CG())
  res <- c(exp(coef(mod, what='mu')), 
           exp(coef(mod, what='sigma')))
  
  res
}

# Super function to simulate and write the estimated parameters
simul <- function(n) {
  result <- t(replicate(n=nrep, expr=simul_one(size=n)))
  result <- cbind(result, n)
  write(x=t(result), file='ConcensuraSincovCG_50%_100000_50_.txt', 
        ncol=3, append=TRUE)
}


# Code to generate the simulations given n --------------------------------

# Aqui se definen los valores de tamano muestral n
# Luego se define el numero de repeticiones
n <- seq(from=50, to=50, by=20)
nrep <- 100000

values <- expand.grid(n=n)
values
apply(values, 1, simul)



