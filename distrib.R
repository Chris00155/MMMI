#install.packages("deSolve")
library(deSolve) # using the "ode" function

pop = 100 #cumul_cases$population[1]

time_values <- seq(1,52)#switch(menu(c("Avant confinement", "Pendant confinement", "Deconfinement", "Reconfinement","Tout"),title = "Choix de la période") + 1,
#cat("Nothing done\n"), seq(1,12), seq(12,18),seq(18,45),seq(45,53),seq(1,53))

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
  theta  = 0.09,#6,#0.7504383, #0.01117611, #0.005249621, #, #incubation rate (/day)
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

sir_values <- ode(
  y = initial_values,
  times = time_values,
  func = sir_equations,
  parms = parameters_values 
)

plot(sir_values[,1], sir_values[,4]+sir_values[,3], col = "black", type = "l",xlim = c(time_values[1],time_values[length(time_values)]),ylim = c(0, pop),lwd=3,
     xlab = "temps (semaine)", ylab = "nombre de personne",main = 'Distribution du nombre d infectés en fonction du temps')

for (i in rnorm(n=20, mean = 0.09, sd = 0.05)){
  parameters_values[3] = i
  #Solve the equations 
  sir_values <- ode(
    y = initial_values,
    times = time_values,
    func = sir_equations,
    parms = parameters_values 
  )
  par(new = TRUE)
  lines(sir_values[,1], sir_values[,4]+sir_values[,3], type = "l", col = "azure4",cex=0.5,xlim = c(time_values[1],time_values[length(time_values)]),ylim = c(0, pop),lwd=1,
       )
}


