# Install relevant packages

init <- function(){
  if(!require('dplyr')){
    install.packages("dplyr")
    library(dplyr)
  }
  if(!require('tidyr')) {
    install.packages('tidyr')
    library(tidyr)
  }
  if(!require('stringr')) {
    install.packages('stringr')
    library(stringr)
  }
  if(!require('readr')){
    install.packages("readr")
    library(readr)
  }
  if(!require('knitr')){
    install.packages("knitr")
    library(readr)
  }
  if(!require('here')){
    install.packages("here")
    library(here)
  }
  if(!require('ggplot2')){
    install.packages("ggplot2")
    library(ggplot2)
  }
  if(!require('fitdistrplus')){
    install.packages("fitdistrplus")
    library(fitdistrplus)
  }
}

init()



#################################################################################################################################
#     Main Task -> load relevant files 
#     1) Einzelteil -> Einzelteil_T06
#     2) Fahrzeuge -> Fahrzeuge_OEM1_Typ11
#################################################################################################################################

# Einzelteil_T06
setwd("~/Rproject_IDA/Data/Einzelteil")
einzelteil_t06 <- read.csv("Einzelteil_T06.csv")

#einzelteil_t06_fehlerhaft <- einzelteil_t06[which(einzelteil_t06$Fehlerhaft > 0), ]
print(str(einzelteil_t06, 10))


# Helper Function Bestandteile Fahrzeuge OEM11
in_dates <- function(x, y){
  Fahrzeuge_OEM1_Typ11[Fahrzeuge_OEM1_Typ11$Produktionsdatum >= x & Fahrzeuge_OEM1_Typ11$Produktionsdatum <= y,]
}

# Bestandteile Fahrzeug OEM11 
setwd("~/Rproject_IDA/Data/Fahrzeug")
Fahrzeuge_OEM1_Typ11 <- read.csv("Fahrzeuge_OEM1_Typ11.csv")


#21.09.2010 - 04.09.2012
#Fahrzeuge_OEM1_Typ11[Fahrzeuge_OEM1_Typ11$Produktionsdatum >= as.POSIXct("2010-09-21") & Fahrzeuge_OEM1_Typ11$Produktionsdatum <= as.POSIXct("2012-09-04"),]


#ress <- Fahrzeuge_OEM1_Typ11 %>%
#  filter(Fahrzeuge_OEM1_Typ11$Produktionsdatum >= as.POSIXct("2010-09-21")) %>%
#  filter(Fahrzeuge_OEM1_Typ11$Produktionsdatum <= as.POSIXct("2012-09-04"))

Fahrzeug_in_between <- in_dates(as.POSIXct("2010-09-21"), as.POSIXct("2012-09-04"))
tail(Fahrzeug_in_between, 10)


#Bestandteile_Fahrzeuge_OEM1_Typ11.csv
setwd("~/Rproject_IDA/Data/Fahrzeug")
Bestandteile_Fahrzeuge_OEM1_Typ11 <- read.csv2("Bestandteile_Fahrzeuge_OEM1_Typ11.csv")
head(Bestandteile_Fahrzeuge_OEM1_Typ11, 10)





# Load Geodaten
setwd("~/Rproject_IDA/Data/Geodaten")
Geodaten_Gemeinden <- read.csv2("Geodaten_Gemeinden_v1.2_2017-08-22_TrR.csv")

head(Geodaten_Gemeinden, 100)

distance_in_km <- function(vector1, vector2 ){
  # Die Vektoren enthalten laengen und breitengrad
  if(!is.atomic(character(vector1)) && !is.atomic(character(vektor2))){
    sprintf("Die Funktionen benötigt zwei Punkte mit jeweils Laengen und Breitengrad!")
  }else{
    
    
    #mit distance: Entfernung in km 
    dx = 71.5 * (vector1[1] - vector2[1]) #laengengrad
    dy = 111.3 * (vector2[2] - vector2[2]) # breitengrad
    distance <- sqrt(dx * dx + dy * dy)
    return(distance)
  }
  
  
}


test <- distance_in_km(c(13,8), c(13.02, 8.02))
################################################################################################################################
#   General Tasks 1. - 6.
################################################################################################################################
################################################################################################################################
# Aufgabe 1
# Create distribution for the logistics delay of component „K7”
#################################################################################################################################

# Import data which includes production date
komponenten_k7 <- read.csv2(here("Data", "Logistikverzug", "Komponente_K7.csv"))
nrow(komponenten_k7)

# Import data which includes receiving date
logistikverzug_k7 <- read.csv(here("Data", "Logistikverzug", "Logistikverzug_K7.csv"))
nrow(logistikverzug_k7)

# Merge tables by IDNummber (id number)
res <- merge(komponenten_k7, logistikverzug_k7, by = "IDNummer")


# Issued one day after production date -> production date + 1
res$VerspaetungInTagen <- as.integer((as.Date(res$Wareneingang) - (as.Date(res$Produktionsdatum)+1)))
# TO DO: There is a cleaner way of doing this (reduce amount of information)
logistics_delay <- data.frame(res$IDNummer, res$Produktionsdatum, res$Wareneingang, res$VerspaetungInTagen)
delay_in_days <- logistics_delay$res.VerspaetungInTagen

# TO REMOVE WEEKENDS
# Source: https://stackoverflow.com/questions/37150403/is-there-an-easy-way-to-tell-if-an-interval-overlaps-a-weekend-in-r
#' Check if a weekday is within an interval
#' 
#' @param wday Day of week (integer 1-7)
#' @param from Date. Can be a vector.
#' @param to Date. Same length as `from` and must be greater than `from`.
#' @param week_start 1 = Monday. 7 = Sunday
#' 
# UNCOMMENT BELOW
# wday_in_interval = function(wday, from, to, week_start = 1) {
#   if (wday < 1 | wday > 7) 
#     stop("wday must be an integer from 1 to 7.")
#   if (week_start)
#     wday = 1 + (((wday - 2) + week_start ) %% 7)  # Translate wday to week_start = 1 (ISO standard)
#   if (any(from > to, na.rm = TRUE))
#     stop("`from` must come before `to`")
#   
#   # If the interval is greater than a week, it trivially contains any weekday
#   over_a_week = difftime(from, to, units = "days") >= 7
#   
#   # Check if weekday is both smaller/greater than "from" and "to"
#   days_from = as.numeric(strftime(from, "%u"))
#   days_to = as.numeric(strftime(to, "%u"))
#   contains_weekday = ifelse(
#     strftime(from, "%V") == strftime(to, "%V"),  # Dates are in the same week?
#     yes = wday >= days_from & wday <= days_to,
#     no = wday >= days_from | wday <= days_to  # 
#   )
#   
#   return(over_a_week | contains_weekday)
# }
#
# logistics_delay <- logistics_delay %>%
#   mutate(overlaps_saturday = wday_in_interval(6, from = res.Produktionsdatum, to = res.Wareneingang) ) %>%
#   mutate(overlaps_sunday = wday_in_interval(7, from = res.Produktionsdatum, to = res.Wareneingang) ) %>%
#   mutate(overlaps_weekend = overlaps_saturday | overlaps_sunday)
# 
# logistics_delay <- logistics_delay %>%
#   mutate(VerspaetungKorrigiert = res.VerspaetungInTagen - as.integer(overlaps_saturday) - as.integer(overlaps_sunday))
# 
# delay_in_days <- logistics_delay$VerspaetungKorrigiert

# Draw histogram to see the distribution of how many components have been delivered with how much delay
hist(logistics_delay$res.VerspaetungInTagen,xlim=c(0,13), xlab="Anzahl der Tage", ylab="Summe der Komponenten", main="Histogramm des Logistikverzugs in Tagen", col="gray")
plot(table(delay_in_days))

# Specification of distribution from data: (https://www.r-project.org/conferences/useR-2009/slides/Delignette-Muller+Pouillot+Denis.pdf)
#   1. Choose among a family of distributions the best candidates
#   2. Estimate the distribution parameters and their uncertainty
#   3. Assess and compate the goodness-of-fit of several distributions
# Here: Discrete data (days are only measured in integers)

## Step 1: Generate skewness-kurtosis graph to choose distributions

descdist(delay_in_days, discrete = TRUE, boot = 100)

## Result: Negative Binomial and Poisson might fit best (Question: Negative Binomial even valid in theory? No binary data here.)

## Step 2: Fit given distributions (Always norm the data to min = 0 because distributions start with 0)

fnbinom <- fitdist(delay_in_days - min(delay_in_days), "nbinom")
plot(fnbinom)
summary(fnbinom)
# Fitting of the distribution ' nbinom ' by maximum likelihood 
# Parameters : 
#   estimate Std. Error
# size 9.838095e+06        NaN
# mu   3.080599e+00 0.00317045
# Loglikelihood:  -505362.1   AIC:  1010728   BIC:  1010750 
# Correlation matrix:
#   size  mu
# size    1 NaN
# mu    NaN   1

fpoisson <- fitdist(delay_in_days - min(delay_in_days), "pois")
plot(fpoisson)
summary(fpoisson)
# Fitting of the distribution ' pois ' by maximum likelihood 
# Parameters : 
#   estimate  Std. Error
# lambda 3.080437 0.003170282
# Loglikelihood:  -505362.1   AIC:  1010726   BIC:  1010737 

## Step 3: Assess goodness-of- fit

fitfnbinom <- gofstat(fnbinom, discrete = TRUE)
print(fitfnbinom)
# Chi-squared statistic:  117879.3 
# Degree of freedom of the Chi-squared distribution:  4 
# Chi-squared p-value:  0 
# Chi-squared table:
#   obscounts theocounts
# <= 1   8518.00   57445.14
# <= 2  81657.00   66798.94
# <= 3 124094.00   68593.56
# <= 4  67161.00   52827.31
# <= 5  19973.00   32547.95
# <= 6   4260.00   16711.20
# > 6     827.00   11565.90
# 
# Goodness-of-fit criteria
# 1-mle-nbinom
# Akaike's Information Criterion      1010728
# Bayesian Information Criterion      1010750

fitfpoisson <- gofstat(fpoisson, discrete = TRUE)
print(fitfpoisson)
# Chi-squared statistic:  117878 
# Degree of freedom of the Chi-squared distribution:  5 
# Chi-squared p-value:  0 
# Chi-squared table:
#   obscounts theocounts
# <= 1   8518.00   57452.16
# <= 2  81657.00   66802.74
# <= 3 124094.00   68593.86
# <= 4  67161.00   52824.76
# <= 5  19973.00   32544.67
# <= 6   4260.00   16708.63
# > 6     827.00   11563.19
# 
# Goodness-of-fit criteria
# 1-mle-pois
# Akaike's Information Criterion    1010726
# Bayesian Information Criterion    1010737

## Result: Both distributions fit equally well, but overall they don't really fit.
## Therefore, same process - now with continuous distributions (should work due to n being high enough)

## Step 1: Generate skewness-kurtosis graph to choose distributions

descdist(delay_in_days, discrete = FALSE, boot = 100)

# summary statistics
# ------
#   min:  0   max:  11 
# median:  3 
# mean:  3.080437 
# estimated sd:  1.012302 
# estimated skewness:  0.5674067 
# estimated kurtosis:  3.630055 

## Result: Pretty clearly lognormal, Weibull is close so we take it as comparison

## Step 2: Fit given distributions

flnorm <- fitdist(delay_in_days - 0.9, "lnorm")
plot(flnorm)
summary(flnorm)
# Fitting of the distribution ' lnorm ' by maximum likelihood 
# Parameters : 
#   estimate   Std. Error
# meanlog 1.7914898 0.0002973244
# sdlog   0.1646034 0.0002102052
# Loglikelihood:  -430989.8   AIC:  861983.7   BIC:  862004.9 
# Correlation matrix:
#   meanlog         sdlog
# meanlog  1.000000e+00 -3.183187e-12
# sdlog   -3.183187e-12  1.000000e+00

fweibull <- fitdist(delay_in_days, "weibull")
plot(fweibull)
summary(fweibull)
# Fitting of the distribution ' weibull ' by maximum likelihood 
# Parameters : 
#   estimate  Std. Error
# shape 5.979374 0.007563124
# scale 6.515010 0.002087809
# Loglikelihood:  -458880.8   AIC:  917765.6   BIC:  917786.9 
# Correlation matrix:
#   shape    scale
# shape 1.000000 0.333769
# scale 0.333769 1.000000

## Step 3: Assess goodness-of- fit

fitflnorm <- gofstat(flnorm)
print(fitflnorm)
# Goodness-of-fit statistics
# 1-mle-lnorm
# Kolmogorov-Smirnov statistic 2.064351e-01
# Cramer-von Mises statistic   2.536715e+03
# Anderson-Darling statistic   1.344775e+04
# 
# Goodness-of-fit criteria
# 1-mle-lnorm
# Akaike's Information Criterion    861983.7
# Bayesian Information Criterion    862004.9

fitfweibull <- gofstat(fweibull)
print(fitfweibull)
# Goodness-of-fit statistics
# 1-mle-weibull
# Kolmogorov-Smirnov statistic     0.2418271
# Cramer-von Mises statistic    2753.6562598
# Anderson-Darling statistic             Inf
# 
# Goodness-of-fit criteria
# 1-mle-weibull
# Akaike's Information Criterion      917765.6
# Bayesian Information Criterion      917786.9

## Result: Log-norm distribution fits much better than Weibull, but still not really good

################################################################################################################################
# Aufgabe 4
# Which data types do the attributes of the registration table “Zulassungen_aller_Fahrzeuge” have?
#################################################################################################################################

# Pfad setzen -> CSV einlesen alle Zulassungen
setwd("~/Rproject_IDA/Data/Zulassungen")
alle_zulassungen <- read.csv2("Zulassungen_alle_Fahrzeuge.csv")
print("Struktur der Table Zulassungen_alle_Fahrzeuge.csv ")
str(alle_zulassungen)


################################################################################################################################
# Aufgabe 6
# On 11.08.2010 there was a hit and run accident. 
# There is no trace of the license plate of the car involved in the accident. 
# The police asks for your help, as you work for the Federal Motor Transport Authority, and asks where the vehicle with the body part number “K5-112-1122-79” was registered.
#################################################################################################################################

# Startdatenpunkt
gesuchte_karosserie <- 'K5-112-1122-79'

# CSV einlesen alle Zulassungen
alle_zulassungen <- read.csv2(here("Data", "Zulassungen", "Zulassungen_alle_Fahrzeuge.csv"))

# CSV einlesen Bestandteile Fahrzeuge OEM1
Bestandteile_Fahrzeuge_OEM1_Typ12 <- read.csv2(here("Data", "Fahrzeug", "Bestandteile_Fahrzeuge_OEM1_Typ12.csv"))

# Filtern nach gesuchter Karosserie und weitere Suche in Zulassungen
result_row <- Bestandteile_Fahrzeuge_OEM1_Typ12 %>%
  filter(ID_Karosserie == gesuchte_karosserie) %>%
  inner_join(alle_zulassungen, by.x = "ID_Fahrzeug", by.y = "IDNummer")

sprintf("Das Fahrzeug [%s] mit der gesuchten Karosserie [%s] wurde %s in der Gemeinde %s zugelassen",result_row["IDNummer"], gesuchte_karosserie, result_row["Zulassung"], result_row["Gemeinden"] )


