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
  if(!require('actuar')){
    install.packages("actuar")
    library(actuar)
  }
}

init()


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

# Convert both rows to POSIXct for timediff calculation and rename columns
logistics_delay <- data.frame(IDNummer = res$IDNummer,Produktionsdatum = as.POSIXct(res$Produktionsdatum),Wareneingang = as.POSIXct(res$Wareneingang))

#Calculate Datediff without weekends -> help function
date_diff_excluding_wekeends <- function(x, y) {
  if(is.na(x) || is.na(y)) return(NA)
  return(sum(!format(seq(x, y-1, by = '1 day'), '%u') %in% 6:7))
}

#Vectorize function
date_diff_excluding_wekeends_V <- Vectorize(date_diff_excluding_wekeends)

#Mutate and calculate the Verzoegerung_in_Tagen ohne Wochenende plus ein Tag (Issued one day after production date)
logistics_delay <- logistics_delay %>%
  mutate(Verzoegerung_in_Tagen = as.integer(date_diff_excluding_wekeends_V(logistics_delay$Produktionsdatum, logistics_delay$Wareneingang) + 1))

delay_in_days <- logistics_delay$Verzoegerung_in_Tagen

# Draw histogram to see the distribution of how many components have been delivered with how much delay
hist(delay_in_days,xlim=c(0,13), xlab="Anzahl der Tage", ylab="Summe der Komponenten", main="Histogramm des Logistikverzugs in Tagen", col="gray")

# Specification of distribution from data: (https://www.r-project.org/conferences/useR-2009/slides/Delignette-Muller+Pouillot+Denis.pdf)
#   1. Choose among a family of distributions the best candidates
#   2. Estimate the distribution parameters and their uncertainty
#   3. Assess and compate the goodness-of-fit of several distributions
# Here: Discrete data (days are only measured in integers)

## Step 1: Generate skewness-kurtosis graph to choose distributions

descdist(delay_in_days, discrete = TRUE, boot = 100)

# summary statistics
# ------
#   min:  3   max:  12 
# median:  6 
# mean:  6.057121 
# estimated sd:  0.8257884 
# estimated skewness:  0.5623495 
# estimated kurtosis:  4.361576 

## Result: Negative Binomial and Poisson might fit best (Question: Negative Binomial even valid in theory? No binary data here.)

## Step 2: Fit given distributions (Always norm the data to min = 0 because distributions start with 0)

# https://www.youtube.com/watch?v=5klSpGC2puU

dists <- c("nbinom", "pois")
fit <- list()
for (i in 1:length(dists)) {
  if (dists[i] == "nbinom") {
    #fit by MLE
    fit[[i]] <- fitdist(delay_in_days - min(delay_in_days), dists[i])
  } else {
    fit[[i]] <- fitdist(delay_in_days - min(delay_in_days), dists[i], method = "mme")
  }
}

for (i in 1:length(dists)){
  print(summary(fit[[i]]))
}

#Plot the results
par(mfrow=c(2,2))
plot.legend <- dists
denscomp(fit, legendtext = plot.legend)
cdfcomp (fit, legendtext = plot.legend)
qqcomp  (fit, legendtext = plot.legend)
ppcomp  (fit, legendtext = plot.legend)

# Fitting of the distribution ' nbinom ' by maximum likelihood 
# Parameters : 
#   estimate  Std. Error
# size 1.862208e+06         NaN
# mu   3.056683e+00 0.003157812
# Loglikelihood:  -490149.5   AIC:  980303   BIC:  980324.3 
# Correlation matrix:
#   size  mu
# size    1 NaN
# mu    NaN   1


# Fitting of the distribution ' pois ' by matching moments 
# Parameters : 
#   estimate
# lambda 3.057121
# Loglikelihood:  -490149.3   AIC:  980300.6   BIC:  980311.3 

## Step 3: Assess goodness-of- fit

goodness_of_fit <- list()
for (i in 1:length(dists)) {
  goodness_of_fit[[i]] <- gofstat(fit[[i]], discrete = TRUE)
}

for (i in 1:length(dists)){
  print(goodness_of_fit[[i]])
}

# Chi-squared statistic:  236063 
# Degree of freedom of the Chi-squared distribution:  4 
# Chi-squared p-value:  0 
# Chi-squared table:
#   obscounts theocounts
# <= 1   4880.00   58490.77
# <= 2  60778.00   67357.58
# <= 3 170619.00   68630.21
# <= 4  55015.00   52445.20
# <= 5  12857.00   32061.69
# <= 6   2069.00   16333.75
# > 6     272.00   11170.79
# 
# Goodness-of-fit criteria
# 1-mle-nbinom
# Akaike's Information Criterion     980303.0
# Bayesian Information Criterion     980324.3

# Chi-squared statistic:  236065.1 
# Degree of freedom of the Chi-squared distribution:  5 
# Chi-squared p-value:  0 
# Chi-squared table:
#   obscounts theocounts
# <= 1   4880.00   58471.40
# <= 2  60778.00   67347.39
# <= 3 170619.00   68629.71
# <= 4  55015.00   52452.33
# <= 5  12857.00   32070.62
# <= 6   2069.00   16340.63
# > 6     272.00   11177.91
# 
# Goodness-of-fit criteria
# 1-mme-pois
# Akaike's Information Criterion   980300.6
# Bayesian Information Criterion   980311.3

## Result: Both distributions fit equally well, but overall they don't really fit.
## Therefore, same process - now with continuous distributions (should work due to n being high enough)

## Step 1: Generate skewness-kurtosis graph to choose distributions

par(mfrow=c(1,1))
descdist(delay_in_days, discrete = FALSE, boot = 100)

# summary statistics
# ------
#   min:  3   max:  12 
# median:  6 
# mean:  6.057121 
# estimated sd:  0.8257884 
# estimated skewness:  0.5623495 
# estimated kurtosis:  4.361576 

## Result: Pretty clearly lognormal, Weibull is close so we take it as comparison.

## Step 2: Fit given distributions

dists <- c("lnorm", "weibull")
fit <- list()
for (i in 1:length(dists)) {
  if(dists[i] == "weibull") {
    # https://stats.stackexchange.com/questions/441516/how-to-fit-weibull-distribution-using-mme-method-and-find-the-estimates-in-r
    #function to calculate sample raw moment
    memp  <-  function(x, order) {
      mean(x^order)
    }
    
    #fit by MME
    fit[[i]] <- fitdist(delay_in_days, dists[i], method = "mme", order=c(1, 2), memp=memp, 
            start=list(shape=6, scale=6), lower=0, upper=Inf)
  } else {
    fit[[i]] <- fitdist(delay_in_days, dists[i], method = "mme")
  }
}

for (i in 1:length(dists)){
  print(summary(fit[[i]]))
}

#Plot the results
par(mfrow=c(2,2))
plot.legend <- dists
denscomp(fit, legendtext = plot.legend)
cdfcomp (fit, legendtext = plot.legend)
qqcomp  (fit, legendtext = plot.legend)
ppcomp  (fit, legendtext = plot.legend)

# Fitting of the distribution ' lnorm ' by matching moments 
# Parameters : 
#   estimate
# meanlog 1.7920265
# sdlog   0.1357061
# Loglikelihood:  -371077.4   AIC:  742158.7   BIC:  742180 

# Fitting of the distribution ' weibull ' by matching moments 
# Parameters : 
#   estimate
# shape 8.756372
# scale 6.404379
# Loglikelihood:  -427915.9   AIC:  855835.8   BIC:  855857.1 

## Step 3: Assess goodness-of- fit

goodness_of_fit <- list()
for (i in 1:length(dists)) {
  goodness_of_fit[[i]] <- gofstat(fit[[i]])
}

for (i in 1:length(dists)){
  print(goodness_of_fit[[i]])
}

# Goodness-of-fit statistics
# 1-mme-lnorm
# Kolmogorov-Smirnov statistic 2.849893e-01
# Cramer-von Mises statistic   4.811740e+03
# Anderson-Darling statistic   2.244218e+04
# 
# Goodness-of-fit criteria
# 1-mme-lnorm
# Akaike's Information Criterion    742158.7
# Bayesian Information Criterion    742180.0

# Goodness-of-fit statistics
# 1-mme-weibull
# Kolmogorov-Smirnov statistic     0.3393315
# Cramer-von Mises statistic    5440.4870071
# Anderson-Darling statistic             Inf
# 
# Goodness-of-fit criteria
# 1-mme-weibull
# Akaike's Information Criterion      855835.8
# Bayesian Information Criterion      855857.1

## Result: Log-norm distribution fits much better than Weibull, but still not really good

################################################################################################################################
# Aufgabe 2
# Why does it make sense to store the available data in separate files instead of saving everything in a huge table? 
# Name at least four benefits. The available tables represent a typical data base structure. How is it called?
#################################################################################################################################

#Benefit 1 
# Accessibility
# Structuring and storing data in separate files allows easier access and thus easier readability of the files. 
# Compared to large data sets, the relevant information would have to be extracted. 
# In the case of small files with adequate designations, the user has the possibility to read out the desired information 
# in a targeted manner and to process it directly. This leads to the next benefit of performance. 

# Benefit 2 
# Performance / Operating time
# By storing the files in small separate files, the access time to the files is significantly reduced. 
# Reading or opening large files requires a lot of memory and operations of the computer. 
# By dividing them into small files, it is possible to work with the files faster. 

#Benefit 3 
# Saving memory / space
# As announced in performance, reducing a large dataset to small seperate ones can also reduce storage space. 
# Data sets that are not necessary or sensibly grouped can be swapped out or made accessible elsewhere, 
# thus avoiding unnecessary memory consumption. 

#Benefit 4
# data accuracy and integrity.
# Data integrity is the overall accuracy, completeness, and consistency of data. 
# If unforeseen events or errors damage the integrity of the data, it is very costly to fix this in a large file. 
# If separate files are used and integrity is violated there, the area to be processed is significantly limited. 
# The small separate files can thus be recovered more quickly without the risk of violating new integrity in other data fields. 


#The typical database structure?
# Not relational because we have no information about relations between the tables. No information about foreign/primary keys. 

################################################################################################################################
# Aufgabe 3
# How many of the parts T16 ended up in vehicles registered in Adelshofen?
#################################################################################################################################

# Pull IDs for relevant parts
txt_t16 <- readLines(here("Data", "Einzelteil", "Einzelteil_T16.txt"))
txt_t16 <- paste('"ID" | |', txt_t16)
txt_t16 <- gsub(x = txt_t16, pattern = "\\s\\|\\s\\|\\s", replacement = ",")
txt_t16 <- gsub(x = txt_t16, pattern = "\\s", replacement = "\n")
txt_t16 <- substring(txt_t16, 1, nchar(txt_t16) - 1)
df_t16 <- read_delim(I(txt_t16), delim = ",", trim_ws = TRUE)
df_t16 <- dplyr::select(df_t16, -1)
rm(txt_t16)
ids_t16 <- df_t16$ID_T16.x
rm(df_t16)

# Pull data about relevant cars
alle_zulassungen <- read.csv2(here("Data", "Zulassungen", "Zulassungen_alle_Fahrzeuge.csv"))
adelshofen_zulassungen <- alle_zulassungen %>%
  filter(alle_zulassungen$Gemeinden == "ADELSHOFEN")

# Pull data about which cars contain which components
Bestandteile_Fahrzeuge_OEM1_Typ11 <- read.csv2(here("Data", "Fahrzeug", "Bestandteile_Fahrzeuge_OEM1_Typ11.csv")) %>% dplyr::select(-1)
Bestandteile_Fahrzeuge_OEM1_Typ12 <- read.csv2(here("Data", "Fahrzeug", "Bestandteile_Fahrzeuge_OEM1_Typ12.csv")) %>% dplyr::select(-1)
Bestandteile_Fahrzeuge_OEM2_Typ21 <- read.csv2(here("Data", "Fahrzeug", "Bestandteile_Fahrzeuge_OEM2_Typ21.csv")) %>% dplyr::select(-1)
Bestandteile_Fahrzeuge_OEM2_Typ22 <- read.csv2(here("Data", "Fahrzeug", "Bestandteile_Fahrzeuge_OEM2_Typ22.csv")) %>% dplyr::select(-1)
Bestandteil_Fahrzeuge <- rbind(Bestandteile_Fahrzeuge_OEM1_Typ11, Bestandteile_Fahrzeuge_OEM1_Typ12, Bestandteile_Fahrzeuge_OEM2_Typ21, Bestandteile_Fahrzeuge_OEM2_Typ22)
rm(Bestandteile_Fahrzeuge_OEM1_Typ11, Bestandteile_Fahrzeuge_OEM1_Typ12, Bestandteile_Fahrzeuge_OEM2_Typ21, Bestandteile_Fahrzeuge_OEM2_Typ22)

# First filter components that containt part T16 by looking at datasets
# Then importing relevant datasets
Bestandteile_Komponente_K2LE2 <- read.csv2(here("Data", "Komponente", "Bestandteile_Komponente_K2LE2.csv"))
Bestandteile_Komponente_K2ST2 <- read.csv2(here("Data", "Komponente", "Bestandteile_Komponente_K2ST2.csv"))
Bestandteile_Komponente_K2LE2 <- Bestandteile_Komponente_K2LE2 %>% dplyr::select(c("ID_T16", "ID_K2LE2")) %>% rename(ID_Komponente = ID_K2LE2)
Bestandteile_Komponente_K2ST2 <- Bestandteile_Komponente_K2ST2 %>% dplyr::select(c("ID_T16", "ID_K2ST2")) %>% rename(ID_Komponente = ID_K2ST2)
Bestandteile_Komponenten <- rbind(Bestandteile_Komponente_K2LE2, Bestandteile_Komponente_K2ST2)

# (Seem to have same data)
#Komponente_K2LE2 <- readLines(here("Data", "Komponente", "KOmponente_K2LE2.txt"))
#Komponente_K2LE2[1] <- paste0('"ID"\\', Komponente_K2LE2[1])
#Komponente_K2LE2 <- gsub(x = Komponente_K2LE2, pattern = "\\s", replacement = "")
#Komponente_K2LE2 <- read_delim(I(Komponente_K2LE2), delim = "\\", trim_ws = TRUE)
#Komponente_K2ST2 <- read.csv2(here("Data", "Komponente", "Bestandteile_Komponente_K2ST2.csv"))

Autos_mit_T16 <- adelshofen_zulassungen %>%
  merge(Bestandteil_Fahrzeuge, by.x = "IDNummer", by.y = "ID_Fahrzeug") %>%
  merge(Bestandteile_Komponenten, by.x = "ID_Sitze", by.y = "ID_Komponente") %>%
  filter(ID_T16 %in% ids_t16)

Unique_Autos_mit_T16 <- unique(Autos_mit_T16$IDNummer)

Anzahl_Unique_Autos_mit_T16 <- length(Unique_Autos_mit_T16)
#  Ang. Fahrzeug 1:1 Komponente in Fahrzeug 1:1 Einzelteil in Komponente


################################################################################################################################
# Aufgabe 4
# Which data types do the attributes of the registration table “Zulassungen_aller_Fahrzeuge” have?
# Put your answers into a table which is integrated into your Markdown document and describe the characteristics of the data type(s).
#################################################################################################################################

# Pfad setzen -> CSV einlesen alle Zulassungen
setwd("~/Rproject_IDA/Data/Zulassungen")
alle_zulassungen <- read.csv2("Zulassungen_alle_Fahrzeuge.csv")
print("Struktur der Table Zulassungen_alle_Fahrzeuge.csv ")
str(alle_zulassungen)


################################################################################################################################
# Aufgabe 5
# You want to publish your application. 
# Why does it make sense to store the records on the database of a server? 
# Why can’t you store the records on your personal computer? 
# What is an easy way to make your application available to your customers? 
# Please name 4 aspects.
#################################################################################################################################

## 1

# Servers are optimized to make resources available remotely via the internet.
# Thus, people from all over the world can access the data if it's on a server.
# A server is also usually running non-stop. The data is thus available any time.
# Nowadays usually servers are used that are managed by specialized companies.
# A lot of challenges, like security aspects, are therefore already taken care of.

## 2

# Storing records on one's own computer might work for a small project where no one else is involved.
# When other people need to have access to the data, though, we face several problems:
# - Usually our computer does not run all the time. But maybe the data should be accessible any time.
# - We do other things with our computer. This might impact the performance if we tried to use our computer as a server.
# - Usually our internet connection is not that great. Just as our computer, it is not optimized to serve large amount of data to remote clients.
# - We would need to set up security measures etc., which would probably quickly become a problem.

## 3

# An easy way to provide an application to customers would be to host it on a cloud service, aka a server.
# This way, the application would be available to any customer with an internet connection over the world wide web.
# By serving an application this way, we can scale the infrastructure up and down depending on how many users we have / expect.
# Furthermore, we are taking advantage of the specialization of hosting companies, so that we can focus on developing our application.

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


