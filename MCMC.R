library(MCMCpack)

# Définition de la fonction de densité de probabilité à simuler
f <- function(x) {
  return(cases$new_cases[x])
}

# Définition de l'algorithme de Metropolis
metropolis <- function(f, q_0, n_steps, sigma) {
  x <- q_0
  accepted <- 0
  samples <- array(0, dim=c(n_steps, 1))
  
  for (i in 1:n_steps) {
    x_star <- rnorm(1, x, sigma)
    rho <- min(1, f(x_star) / f(x))
    u <- runif(1, 0, 1)
    if (u < rho) {
      x <- x_star
      accepted <- accepted + 1
    }
    samples[i] <- x
  }
  
  accept_rate <- accepted / n_steps
  return(list(samples = samples, accept_rate = accept_rate))
}

# Simulation de la distribution de probabilité avec l'algorithme de Metropolis
set.seed(123)
n_steps <- 10000
sigma <- 0.2
samples <- metropolis(f, q_0=1, n_steps=n_steps, sigma=sigma)$samples

# Visualisation des résultats
hist(samples, prob=TRUE, main="Distribution de probabilité simulée avec l'algorithme de Metropolis", xlab="x")
curve(f, add=TRUE, col="red", lwd=2)

#########


logitfun <- function(beta, y, X){
  rev(death$deaths)[y]
}

x1 <- rnorm(1000)
x2 <- rnorm(1000)
Xdata <- cbind(1,x1,x2)
p <- exp(.5 - x1 + x2)/(1+exp(.5 - x1 + x2))
yvector <- rbinom(1000, 1, p)

post.samp <- MCMCmetrop1R(logitfun, theta.init= 0,
                          thin=1, mcmc=40000, burnin=500,
                          tune= 1.5,
                          verbose=500, logfun=TRUE)

raftery.diag(post.samp)
plot(post.samp)
summary(post.samp)


## ##################################################


