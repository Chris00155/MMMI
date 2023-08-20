library(dplyr)
library(lattice)
library(tibble)

## DATAS ##
variant <- read.table(file = "FichierCSV/variant.csv",header = TRUE)
doses <- read.table(file = "FichierCSV/doses.csv",header = TRUE)
bed <- read.table(file = "FichierCSV/bedOccup.csv",header = TRUE,sep = ",")
death <- read.table(file = "FichierCSV/death.csv",header = TRUE,sep=",")
cases <- read.table(file = "FichierCSV/caseByAge.csv", header = TRUE)
cumul_cases <- read.table(file = "FichierCSV/cumul_cases.csv", header = TRUE, sep = ";")
cumul_death <- read.table(file = "FichierCSV/cumul_death.csv", header = TRUE, sep = ";")
deathByAge <- read.table(file = "FichierCSV/deathByAge.csv", header = TRUE)

x = 1:54
cases[is.na(cases)] <- 0
for (i in seq(1,6)){
  plot(x,cases$new_cases[1 + (53*(i-1)):(53 * i)],ylim = c(0, 60000), 
       xlim = c(1, 53), type = "b", col = i, main = "", xlab = "", ylab = "", pch = i)
  par(new = TRUE)
}
title(main = "Nombre de cas par semaine et par âge en Allemagne en 2020",
      xlab = "Semaines", 
      ylab="Nombre de nouveaux cas",
      cex.main = 1.5,   # Title size
      cex.lab = 1.3,    # X-axis and Y-axis labels size
      cex.axis = 2)
legend("topleft", legend=c(cases$age_group[53*1],cases$age_group[53*2],cases$age_group[53*3],
                          cases$age_group[53*4],cases$age_group[53*5],cases$age_group[53*6]), 
                          col = c("1","2","3","4","5","6"), lty=1:2, cex=0.8, pch = c(1,2,3,4,5,6))

## DEATH BY AGE ##
newDeath <- data_frame(Semaine = seq(1,53), '<15yr' = rep(0,53), '15-34yr' = rep(0,53), '35-59yr' = rep(0,53), '60-79yr' = rep(0,53), '80+yr' = rep(0,53), 'Tot' = rep(0,53));
newDeath[1] <- seq(1:53)
i=1
for (j in seq(11,53)){ #i in seq(1,length(deathByAge$Fatale))
  newDeath$'<15yr'[j]  = deathByAge$Fatale[i] + deathByAge$Fatale[i+2] #<15 ans 
  i = i + 1
  newDeath$'Tot'[j] = deathByAge$Fatale[i]
  i = i + 2
  newDeath$'15-34yr'[j] = deathByAge$Fatale[i] #15 - 34 ans
  i = i + 1
  newDeath$'35-59yr'[j] = deathByAge$Fatale[i] #34 - 59 ans
  i = i + 1
  newDeath$'60-79yr'[j] = deathByAge$Fatale[i] #60 - 79 ans
  i = i + 1
  newDeath$'80+yr'[j] = deathByAge$Fatale[i] #80+ ans
  i = i + 1
}
  
plot(seq(1:53),newDeath$'<15yr',xlim = c(1, 53),ylim = c(1, 3400), type = "b", col = 1, main = "", xlab = "", ylab = "", pch = 1)
par(new = TRUE)
plot(seq(1:53),newDeath$'15-34yr',xlim = c(1, 53),ylim = c(1, 3400), type = "b", col = 2, main = "", xlab = "", ylab = "", pch = 2)
par(new = TRUE)
plot(seq(1:53),newDeath$'35-59yr',xlim = c(1, 53),ylim = c(1, 3400), type = "b", col = 3, main = "", xlab = "", ylab = "", pch = 3)
par(new = TRUE)
plot(seq(1:53),newDeath$'60-79yr',xlim = c(1, 53),ylim = c(1, 3400), type = "b", col = 4, main = "", xlab = "", ylab = "", pch = 4)
par(new = TRUE)
plot(seq(1:53),newDeath$'80+yr',xlim = c(1, 53),ylim = c(1, 3400), type = "b", col = 5, main = "", xlab = "", ylab = "", pch = 5)
par(new = TRUE)
title(main = "Nombre de mort par semaine et par âge en Allemagne en 2020",
      xlab = "Semaines", 
      ylab="Nombre de mort",
      cex.main = 1.5,   # Title size
      cex.lab = 1.3,    # X-axis and Y-axis labels size
      cex.axis = 2)
legend("topleft", legend=c('<15yr','15-34yr','35-59yr','60-79yr','80+yr'), 
       col = c("1","2","3","4","5"), lty=1:2, cex=0.8, pch = c(1,2,3,4,5,6))

## Pour seulement les < 60 ans 
plot(seq(1:53),newDeath$'<15yr',xlim = c(1, 53),ylim = c(1, 150), type = "b", col = 1, main = "", xlab = "", ylab = "", pch = 1)
par(new = TRUE)
plot(seq(1:53),newDeath$'15-34yr',xlim = c(1, 53),ylim = c(1, 150), type = "b", col = 2, main = "", xlab = "", ylab = "", pch = 2)
par(new = TRUE)
plot(seq(1:53),newDeath$'35-59yr',xlim = c(1, 53),ylim = c(1, 150), type = "b", col = 3, main = "", xlab = "", ylab = "", pch = 3)
title(main = "Nombre de mort par semaine et par âge en Allemagne en 2020",
      xlab = "Semaines", 
      ylab="Nombre de mort",
      cex.main = 1.5,   # Title size
      cex.lab = 1.3,    # X-axis and Y-axis labels size
      cex.axis = 2)
legend("topleft", legend=c('<15yr','15-34yr','35-59yr'), 
       col = c("1","2","3"), lty=1:2, cex=0.8, pch = c(1,2,3))

## CUMULATIVE CASES & DEATH ##
x = 1: length(cumul_cases$weekly_count)
cumul_cases$weekly_count[is.na(cumul_cases$weekly_count)] <- 0
cumul_death$weekly_count[is.na(cumul_death$weekly_count)] <- 0
plot(x,cumul_cases$weekly_count, col = 1, main = "", xlab = "", ylab = "",ylim = c(0,max(cumul_cases$weekly_count)+200))
par(new = TRUE)
plot(x,cumul_death$weekly_count, col = 2, main = "", xlab = "", ylab = "",ylim = c(0,max(cumul_cases$weekly_count)+200))
par(new = TRUE)
plot(rep(11,length(seq(-1,max(cumul_cases$weekly_count)+200,5))),seq(-1,max(cumul_cases$weekly_count)+200,5), col = 3, main = "", xlab = "", ylab = "",ylim = c(0,max(cumul_cases$weekly_count)+200), xlim = c(1,53),pch = 20,cex=0.7)
par(new = TRUE)
plot(rep(17,length(seq(-1,max(cumul_cases$weekly_count)+200,5))),seq(-1,max(cumul_cases$weekly_count)+200,5), col = 4, main = "", xlab = "", ylab = "",ylim = c(0,max(cumul_cases$weekly_count)+200), xlim = c(1,53),pch = 20,cex=0.7)
par(new = TRUE)
plot(rep(44,length(seq(-1,max(cumul_cases$weekly_count)+200,5))),seq(-1,max(cumul_cases$weekly_count)+200,5), col = 5, main = "", xlab = "", ylab = "",ylim = c(0,max(cumul_cases$weekly_count)+200), xlim = c(1,53),pch = 20,cex=0.7)

title(main = "Nombre de cas et de morts par semaine en Allemagne en 2020",
      xlab = "Semaines", 
      ylab="Nombre",
      cex.main = 1.5,   # Title size
      cex.lab = 1.3,    # X-axis and Y-axis labels size
      cex.axis = 2)

legend("topleft", legend=c("Cas cumulés", "Mort cumulés","Confinement","Déconfinement","Reconfinement"), 
       col = c("1","2","3","4","5"), lty=1:5, cex=0.6)

## CASES and DEATH BY PHASEs
## Avant confinement 
x = 1: length(cumul_cases$weekly_count)

plot(x[1:12],cumul_cases$weekly_count[1:12], col = 1, main = "", xlab = "", ylab = "",ylim = c(0,max(cumul_cases$weekly_count[1:12])+200))
par(new = TRUE)
plot(x[1:12],cumul_death$weekly_count[1:12], col = 2, main = "", xlab = "", ylab = "",ylim = c(0,max(cumul_cases$weekly_count[1:12])+200))
title(main = "Nombre de cas et de morts par semaine en Allemagne avant le confinement",
      xlab = "Semaines", 
      ylab="Nombre",
      cex.main = 1.5,   # Title size
      cex.lab = 1.3,    # X-axis and Y-axis labels size
      cex.axis = 2)
legend("topleft", legend=c("Cas cumulés", "Mort cumulés"), 
       col = c("1","2"), lty=1:2, cex = 1)

#Pendant le confinemement
plot(x[12:18],cumul_cases$weekly_count[12:18], col = 1, main = "", xlab = "", ylab = "",ylim = c(0,max(cumul_cases$weekly_count[12:18])+200))
par(new = TRUE)
plot(x[12:18],cumul_death$weekly_count[12:18], col = 2, main = "", xlab = "", ylab = "",ylim = c(0,max(cumul_cases$weekly_count[12:18])+200))
title(main = "Nombre de cas et de morts par semaine en Allemagne pendant le confinement",
      xlab = "Semaines", 
      ylab="Nombre",
      cex.main = 1.5,   # Title size
      cex.lab = 1.3,    # X-axis and Y-axis labels size
      cex.axis = 2)
legend("topleft", legend=c("Cas cumulés", "Mort cumulés"), 
       col = c("1","2"), lty=1:2, cex = 1)

#Avant le reconfinemement
plot(x[18:45],cumul_cases$weekly_count[18:45], col = 1, main = "", xlab = "", ylab = "",ylim = c(0,max(cumul_cases$weekly_count[18:45])+200))
par(new = TRUE)
plot(x[18:45],cumul_death$weekly_count[18:45], col = 2, main = "", xlab = "", ylab = "",ylim = c(0,max(cumul_cases$weekly_count[18:45])+200))
title(main = "Nombre de cas et de morts par semaine en Allemagne avant le re-confinement",
      xlab = "Semaines", 
      ylab="Nombre",
      cex.main = 1.5,   # Title size
      cex.lab = 1.3,    # X-axis and Y-axis labels size
      cex.axis = 2)
legend("topleft", legend=c("Cas cumulés", "Mort cumulés"), 
       col = c("1","2"), lty=1:2, cex = 1)

#Pendant le reconfinemement
plot(x[45:length(cumul_cases$weekly_count)],cumul_cases$weekly_count[45:length(cumul_cases$weekly_count)], col = 1, main = "", xlab = "", ylab = "",ylim = c(0,max(cumul_cases$weekly_count[45:53])+200))
par(new = TRUE)
plot(x[45:length(cumul_cases$weekly_count)],cumul_death$weekly_count[45:length(cumul_cases$weekly_count)], col = 2, main = "", xlab = "", ylab = "",ylim = c(0,max(cumul_cases$weekly_count[45:53])+200))
title(main = "Nombre de cas et de morts par semaine en Allemagne pendant le re-confinement",
      xlab = "Semaines", 
      ylab="Nombre",
      cex.main = 1.5,   # Title size
      cex.lab = 1.3,    # X-axis and Y-axis labels size
      cex.axis = 2)
legend("topleft", legend=c("Cas cumulés", "Mort cumulés"), 
       col = c("1","2"), lty=1:2, cex = 1)

## DEATHS ##
x = 1:length(death$deaths)
plot(x,rev(death$deaths),main = "Nombre de mort de Covid par jour en Allemagne en 2020", xlab = "Jour" ,xaxt = 'n', ylab = "Nombre de mort")
axis(1, at = seq(round(min(x)), round(max(x)), by = 1), labels = rev(death$dateRep))
N = cumul_death$population[1]


## BEDS ##
x = 13:52
plot(x,bed$value[1:40],xlab = "Semaine",ylab="Occupation des lits")
title("Occupation des lits par semaine en Allemagne en 2020")
axis(1,13:52)



