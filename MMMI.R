library(dplyr)
library(lattice)
library(tibble)

## DATAS ##
variant <- read.table(file = "FichierCSV/variant.csv",header = TRUE)
doses <- read.table(file = "FichierCSV/doses.csv",header = TRUE)
bed <- read.table(file = "FichierCSV/bedOccup.csv",header = TRUE)
death <- read.table(file = "FichierCSV/death.csv",header = TRUE)
cases <- read.table(file = "FichierCSV/caseByAge.csv", header = TRUE)
cumul_case <- read.table(file = "FichierCSV/cumul_case.csv", header = TRUE)


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
      cex.main = 2,   # Title size
      cex.lab = 1.7,    # X-axis and Y-axis labels size
      cex.axis = 2)
legend("topleft", legend=c(cases$age_group[53*1],cases$age_group[53*2],cases$age_group[53*3],
                          cases$age_group[53*4],cases$age_group[53*5],cases$age_group[53*6]), 
                          col = c("1","2","3","4","5","6"), lty=1:2, cex=0.8, pch = c(1,2,3,4,5,6))

## CUMULATIVE CASES ##
x = 1: length(cumul_case$Cumulative_number_for_14_days_of_COVID.19_cases_per_100000)
cumulatives <- gsub(",","",cumul_case$Cumulative_number_for_14_days_of_COVID.19_cases_per_100000)
cumulatives <- as.numeric(cumulatives)
cumulatives <- round(cumulatives %/% 100000000)
cumulatives = rev(cumulatives)
plot(x,cumulatives, main = "Nombre de cas cumulés calculés pour 14 jours en Allemangne en 2020",
     xlab = "Jours de l'année",
     ylab = "Nombre de cas")

## DEATHS ##
x = 1:length(death$deaths)
plot(x,rev(death$deaths),main = "Nombre de mort de Covid par jour en Allemagne en 2020", xlab = "Jour" ,xaxt = 'n', ylab = "Nombre de mort")
axis(1, at = seq(round(min(x)), round(max(x)), by = 1), labels = rev(death$dateRep))
N = death$popData2020[1]


## BEDS ##
x = 13:52
plot(x,bed$value[1:40],xlab = "Semaine",ylab="Occupation des lits")
title("Occupation des lits par semaine en Allemagne en 2020")
axis(1,13:52)



