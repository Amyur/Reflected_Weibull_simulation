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
  x1 <- runif(size, min=0.4, max=0.6)
  x2 <- runif(size, min=0.4, max=0.6)
  mu <- exp(1.5 - 1.5 * x1)
  sigma <- exp(2 - 2 * x2)
  y <- rRW(n=size, mu, sigma)
  
  #Se aplica censura reemplazando los valores mayores 
  corte <- quantile(y, probs=0.30)
  y <- ifelse(y <= corte, corte, y)
  status <- ifelse(y == corte, 1, 2)
  
  x <- Surv(y, status, type="left")
  
  mod <- gamlss(x~x1, sigma.fo=~x2, family=RWlc,
                control=gamlss.control(n.cyc=1000000, trace=FALSE), method=CG())
  
  res <- c(coef(mod, what='mu'), 
           coef(mod, what='sigma'))
  
  res
}

# Super function to simulate and write the estimated parameters
simul <- function(n) {
  result <- t(replicate(n=nrep, expr=simul_one(size=n)))
  result <- cbind(result, n)
  write(x=t(result), file='ConcensuraConcovCG_100000_950_1000.txt', 
        ncol=5, append=TRUE)
}


# Code to generate the simulations given n --------------------------------

# Aqui se definen los valores de tamano muestral n
# Luego se define el numero de repeticiones
n <- seq(from=950, to=1000, by=20)
nrep <- 100000

values <- expand.grid(n=n)
values
apply(values, 1, simul)



