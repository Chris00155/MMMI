library(deSolve) # using the "ode" function

N = 10000
burn_in = 500
pop = 100 #cumul_cases$population[1]

time_values <- seq(1,52)#switch(menu(c("Avant confinement", "Pendant confinement", "Deconfinement", "Reconfinement","Tout"),title = "Choix de la période") + 1,
       #cat("Nothing done\n"), seq(1,12), seq(12,18),seq(18,45),seq(45,53),seq(1,53))

byage <- 2#switch(menu(c("Oui","Non"),title = "Voulez diviser par tranche d'age ?") + 1,
          #           cat("Nothing done\n"), 1 ,2)

if (byage == 1){
  age <- switch(menu(c('<15yr', '15-34yr', '35-59yr', '60-79yr', '80+yr'),title = "Voulez diviser par tranche d'age ?") + 1,
             cat("Nothing done\n"), 2, 3, 4, 5, 6)
  dead <- newdeath[age,]
}
cas <- cumul_cases$weekly_count 
cas <- lapply(cas, function(cas) { ifelse(cas == 0, NA, cas) })
dead <- cumul_death$cumulative_count
dead <- lapply(dead, function(dead) { ifelse(dead == 0, NA, dead) })
beds <- c(rep(x=NA,13),bed$value[1:40])

sir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- alpha * R - beta * (IA + IS) * S 
    dE <- beta * (IA + IS) * S - theta * E
    dIA <- theta * (1-p) * E - delta1 * IA
    dIS <- theta * p * E - delta2 * IS - delta3 * IS - delta4 * IS
    dH <- delta3 * IS - tau1 * H - tau2 * H
    dR <-  delta1 * IA + delta2 * IS + tau1 * H - alpha * R
    dD <- delta4 * IS + tau2 * H
    
    return(list(c(dS, dE, dIA, dIS, dH, dR, dD)))
  })
}

parameters_values <- c(
  alpha  = 0, #0.7, #wailing rate (/day)
  beta   = 0.02,#3/pop, # infectious contact rate (/person/day)
  theta  = 0.090483741803596,#6,#0.7504383, #0.01117611, #0.005249621, #, #incubation rate (/day)
  p = 0.6, #probability of being symp. 
  delta1 = 0.01,#pop/10000, #sick rate (/day) asymp -> recov
  delta2 = 0.05,#pop/10000, #symp-> recov
  delta3 = 0.076098649976568,#0.008602710, #0.1, #symp -> hosp
  delta4 = 0.010610788180481	,#0.9080994/pop, #symp-> dead
  tau1   = 0.1, #time in the hospital (/people) -> recov
  tau2   = 0.00970733351719395 #time in the hospital (/people) -> dead
)

initial_values <- c(
  S = pop - 10,# 9.95025,#31.552809,#7.176168,#16.95702,#number of susceptibles at time = 0
  E = 0,  #0.9892063,#1.5946747,#1.264095,#48.307886,#  number of exposed at time = 0
  IA = 0, #14.519098,#3.468553,#5.311784,#9.52086087,# number of infectious asymptomatic at time = 0
  IS = 1,  #4.810332,#2.744644,#1.814296,#9.5334571,# number of infectious sympomatic at time = 0
  H = 0, #23.090997,#6.792122,#10.957224,#2.39469240,# beds[time_values[1]], # number of hospitalized at time = 0
  R = 0, #9.220140,#23.63181,#50.292499,#3.69773197,# number of recovered (and immune) at time = 0
  D = 0 #28.41998#21.21539#14.1839346#0.58835191#number of dead at time = 0
)

#Solve the equations 
sir_values <- ode(
  y = initial_values,
  times = time_values,
  func = sir_equations,
  parms = parameters_values 
)


# Matrice des échantillons
samples <- matrix(0,nrow = length(seq(burn_in,N,100)), ncol = length(parameters_values))
colnames(samples) <- c("alpha", "beta", "theta", "p", "delta1", "delta2", "delta3", "delta4", "tau1", "tau2")

j <- 1
samples[j, ] <- unlist(parameters_values)

# Boucle d'itération
for (i in 1:N) {
  # Génération d'une proposition
  new_param <- c(parameters_values[1], beta = dpois(1,parameters_values[2]),
                  theta  = dpois(1,parameters_values[3]),parameters_values[4],
                  parameters_values[5] , parameters_values[6],
                  delta3 = dpois(1,parameters_values[7]), delta4 = dpois(1,parameters_values[8]),
                  parameters_values[9], tau2  = dpois(1,parameters_values[10])) # que je dois modifier un petit peu 
  
  new_sir_values <- ode(
    y = initial_values,
    times = time_values,
    func = sir_equations,
    parms = new_param
  )
 
  # Calcul de la probabilité d'acceptation
  #p_accept <- dpois(S_prop, S0) * dpois(I_prop, I0) * dpois(R_prop, R0) * dpois(D_prop, D0) / dpois(S0, S_prop) / dpois(I0, I_prop) / dpois(R0, R_prop) / dpois(D0, D_prop)
  p_accept <- c(sum((new_sir_values[,5]-sir_values[,5])*log(unlist(cas[time_values])),na.rm = TRUE),
    sum((new_sir_values[,7]-sir_values[,7])*log(unlist(dead[time_values])),na.rm = TRUE),
    sum((new_sir_values[,6]-sir_values[,6])*log(beds[time_values]),na.rm = TRUE))
  
  # Acceptation ou rejet de la proposition
  if (log(runif(1)) < p_accept[1]){ 
    #parameters_values[3] <- new_param[3]
  }#else{
    #parameters_values[2] <- new_param[2]
  #}
  if (log(runif(1)) < p_accept[2]){
    parameters_values[8] <- new_param[8]
    parameters_values[10] <- new_param[10]
  } 
  if (log(runif(1)) < p_accept[3]){
    parameters_values[7] <- new_param[7]
  }
  
  # Stockage de l'échantillon
  if (i > burn_in && i%%100 == 0){
    samples[j+1, ] <- unlist(parameters_values)
    j <- j+1
  }
}

# Enregistrement des échantillons
write.csv(as.matrix(samples),file="FichierCSV/samples.csv")

# Affichage des échantillons
head(samples)

# The new model
sir_values <- ode(
  y = initial_values,
  times = time_values,
  func = sir_equations,
  parms = parameters_values
)

# Plot the results
plot(sir_values[,1], sir_values[,2], type = "l", col = "darkorange1",
     xlab = "temps (semaine)", ylab = "nombre de personne",main = 'Modèle épidémiologique en fonction du temps', lwd=3,xlim = c(time_values[1],time_values[length(time_values)]),ylim = c(0, pop))
lines(sir_values[,1], sir_values[,3], col = "dodgerblue1", lwd=3,xlim = c(time_values[1],time_values[length(time_values)]),ylim = c(0, pop))
lines(sir_values[,1], sir_values[,4], col = "pink1", lwd=3,xlim = c(time_values[1],time_values[length(time_values)]),ylim = c(0, pop))
lines(sir_values[,1], sir_values[,5], col = "pink1", lty=2, lwd=3,xlim = c(time_values[1],time_values[length(time_values)]),ylim = c(0, pop))
lines(sir_values[,1], sir_values[,6], col = "seashell4", lwd=3,xlim = c(time_values[1],time_values[length(time_values)]),ylim = c(0, pop))
lines(sir_values[,1], sir_values[,7], col = "green", lwd=3,xlim = c(time_values[1],time_values[length(time_values)]),ylim = c(0, pop))
lines(sir_values[,1], sir_values[,8], col = "red", lwd=3,xlim = c(time_values[1],time_values[length(time_values)]),ylim = c(0, pop))


legend("topright", c("susceptibles", "exposed", "infected asymp","infected symp", "hospitalized", "recovered", "dead"),
       col = c("darkorange1", "dodgerblue1","pink1","pink1","seashell4", "green", "red"), lty = c(1,1,1,2,1,1,1), bty = "n", lwd=3)

