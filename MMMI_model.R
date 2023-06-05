#install.packages("deSolve")
library(deSolve) # using the "ode" function

sir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- alpha * R - beta * (IA + IP) * S
    dE <- beta * (IA + IP) * S - theta * E
    dIA <- theta * (1-p) * E - delta1 * IA
    dIP <- theta * p * E - delta2 * IP - delta3 * IP - delta4 * IP
    dH <- delta3 * IP - tau1 * H - tau2 * H
    dR <-  delta1 * IA + delta2 * IP + tau1 * H - alpha * R
    dD <- delta4 * IP + tau2 * H
    
    return(list(c(dS, dE, dIA, dIP, dH, dR, dD)))
  })
}

#parametre pas ok 
parameters_values <- c(
  alpha  = 20, #wailing rate (/day)
  beta   = 0.0001, # infectious contact rate (/person/day)
  theta  = 0.1, #incubation rate (/day)
  p = 0.05, #probability of being symp. 
  delta1 = 0.5, #sick rate (/day) asymp -> recov
  delta2 = 0.2, #symp-> recov
  delta3 = 1, #symp -> hosp
  delta4 = 0.01, #symp-> dead
  tau1   = 0.1, #time in the hospital (/people) -> recov
  tau2   = 0.07 #time in the hospital (/people) -> dead
)

initial_values <- c(
  S = N - 1,  # number of susceptibles at time = 0
  E = 0,  # number of exposed at time = 0
  IA = 1,  # number of infectious asymptomatic at time = 0
  IP = 0,  # number of infectious sympomatic at time = 0
  H = 0, # number of hospitalized at time = 0
  D = 0, #number of dead at time = 0
  R = 0   # number of recovered (and immune) at time = 0
)

time_values <- seq(0, 10) # days

#Solve the equations 
sir_values_1 <- ode(
  y = initial_values,
  times = time_values,
  func = sir_equations,
  parms = parameters_values 
)


plot(sir_values_1[,1], sir_values_1[,2], type = "l", col = "darkorange1",
     xlab = "time (days)", ylab = "number of people", lwd=3)
lines(sir_values_1[,1], sir_values_1[,3], col = "dodgerblue1", lwd=3)
lines(sir_values_1[,1], sir_values_1[,4], col = "pink1", lwd=3)
lines(sir_values_1[,1], sir_values_1[,5], col = "pink1", lty=2, lwd=3)
lines(sir_values_1[,1], sir_values_1[,6], col = "seashell4", lwd=3)
lines(sir_values_1[,1], sir_values_1[,7], col = "red", lwd=3)
lines(sir_values_1[,1], sir_values_1[,8], col = "green", lwd=3)


legend("topright", c("susceptibles", "exposed", "infected asymp","infected symp", "hospitalized", "dead", "recovered"),
       col = c("darkorange1", "dodgerblue1","pink1","pink1","seashell4", "red", "green"), lty = c(1,1,1,2,1,1,1), bty = "n", lwd=3)

#Calculating the R0 term
R0 <- N * parameters_values["beta"] / parameters_values["gamma"]
