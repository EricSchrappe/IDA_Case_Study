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
  if(!require('purrr')){
    install.packages("purrr")
    library(purrr)
  }
  if(!require('plotly')){
    install.packages("plotly")
    library(plotly)
  }
  if(!require('lubridate')){
    install.packages("lubridate")
    library(lubridate)
  }
}


init()



################################################################################################################################
#   General Tasks 1. - 6.
################################################################################################################################
################################################################################################################################
# Aufgabe 1
#a)  How is the logistics delay distributed? Justify your choice with statistical tests and briefly describe your approach.
#################################################################################################################################


# Import data which includes production date
komponenten_k7 <- read.csv2(here("Data", "Logistikverzug", "Komponente_K7.csv"))

# Import data which includes receiving date
logistikverzug_k7 <- read.csv(here("Data", "Logistikverzug", "Logistikverzug_K7.csv"))

# Merge tables by IDNummber (id number) if rows equal
if(!nrow(komponenten_k7) == nrow(logistikverzug_k7)){
  print("Amount of rows unequal in Komponenten K7 und Logistikverzug K7")
  stop()
}else{
  big_table <- merge(komponenten_k7, logistikverzug_k7, by = "IDNummer")
  print("Merged 'Komponente_K7.csv' and 'Logistikverzug_K7.csv'")
}

# Convert both rows to POSIXct for timediff calculation and rename columns
logistics_delay <- data.frame(IDNummer=big_table$IDNummer,Produktionsdatum= as.POSIXct(big_table$Produktionsdatum),Wareneingang= as.POSIXct(big_table$Wareneingang))


#Add one day to Produktionsdatum <- "You can assume that produced goods are issued one day after production date"
logistics_delay$Produktionsdatum <- logistics_delay$Produktionsdatum + lubridate::days(1)


#Calculate Datediff without weekends -> help function
date_diff_excluding_wekeends <- function(x, y) {
  if(is.na(x) || is.na(y)) return(NA)
  return(sum(!format(seq(x, y-1, by = '1 day'), '%u') %in% 6:7))
}

#Vectorize function
date_diff_excluding_wekeends_V <- Vectorize(date_diff_excluding_wekeends)

#Mutate and calculate the Verzoegerung_in_Tagen ohne Wochenende
logistics_delay <- logistics_delay %>%
  mutate(Verzoegerung_in_Tagen=date_diff_excluding_wekeends_V(logistics_delay$Produktionsdatum, logistics_delay$Wareneingang))

#Check structure
print(head(logistics_delay, 10))


#Plot the Table 
fig <- plot_ly(x = logistics_delay$Verzoegerung_in_Tagen, type = "histogram", nbinsx = 25, alpha=0.8) %>%
    layout(yaxis = list(title = "Anzahl der Teile"),
           xaxis = list(title = "Verspaetung in Tagen", tickmode='linear'),
           title="Plot: Verteilung der Verspaetung in Tagen") 

fig


# Specification of distribution from data: (https://www.r-project.org/conferences/useR-2009/slides/Delignette-Muller+Pouillot+Denis.pdf)
#   1. Choose among a family of distributions the best candidates
#   2. Estimate the distribution parameters and their uncertainty
#   3. Assess and compate the goodness-of-fit of several distributions
# Here: Discrete data (days are only measured in integers)

## Step 1: Generate skewness-kurtosis graph to choose distributions

descdist(logistics_delay$Verzoegerung_in_Tagen, discrete = TRUE, boot = 100)

## Result: Negative Binomial and Poisson might fit best (Question: Negative Binomial even valid in theory? No binary data here.)

## Step 2: Fit given distributions (Always norm the data to min = 0 because distributions start with 0)

fnbinom <- fitdist(logistics_delay$Verzoegerung_in_Tagen - min(logistics_delay$Verzoegerung_in_Tagen), "nbinom")
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

fpoisson <- fitdist(logistics_delay$Verzoegerung_in_Tagen - min(logistics_delay$Verzoegerung_in_Tagen), "pois")
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

descdist(logistics_delay$Verzoegerung_in_Tagen, discrete = FALSE, boot = 100)

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

flnorm <- fitdist(logistics_delay$Verzoegerung_in_Tagen - 0.9, "lnorm")
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

fweibull <- fitdist(logistics_delay$Verzoegerung_in_Tagen, "weibull")
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
# Aufgabe 1
#b)  Determine the mean of the logistics delay (watch out for weekends). Please interpret this number and discuss possible alternatives.
#################################################################################################################################
mean_logistic_dealy <- mean(logistics_delay$Verzoegerung_in_Tagen)
sprintf("Die durschnittliche logistische Verzoegerung beträgt %s Tage", format(round(mean_logistic_dealy, 2), nsmall = 2))

discussion <- "Im Durchschnitt beträgt die Verzoegerung blabla. TODO"









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


# Import data  Einzelteil_T16
txt_t1 <- readLines(here("Data", "Einzelteil", "Einzelteil_T16.txt"))
txtNew_t1 <- gsub(x = txt_t1, pattern = "\\s\\|\\s\\|\\s", replacement = ",") 
txtNew_t1 <- gsub(x = txtNew_t1, pattern = "\\s", replacement = "\n") 
txtNew_t1 <- substring(txtNew_t1, 1, nchar(txtNew_t1)-1) 
rval_part_t1 <- read_delim(I(txtNew_t1), delim = ",", trim_ws = TRUE)


head(rval_part_t1, 10)
head(rval_part_t1$Produktionsdatum.x, 2)
colnames(rval_part_t1)



# Load dataset for registrations in Adelshofen
alle_zulassungen <- read.csv2(here("Data", "Zulassungen", "Zulassungen_alle_Fahrzeuge.csv"))

adelshofen_zulassungen <- alle_zulassungen %>%
  filter(alle_zulassungen$Gemeinden == "ADELSHOFEN")

typeof(adelshofen_zulassungen$IDNummer)
head(adelshofen_zulassungen, 10)


shl <- adelshofen_zulassungen %>% 
  filter(str_detect(adelshofen_zulassungen$IDNummer, "^12-"))

shl



#   16-212-2121-7

################################################################################################################################
# Aufgabe 4
# Which data types do the attributes of the registration table “Zulassungen_aller_Fahrzeuge” have?
# Put your answers into a table which is integrated into your Markdown document and describe the characteristics of the data type(s).
#################################################################################################################################

# Pfad setzen -> CSV einlesen alle Zulassungen
alle_zulassungen <- read.csv2(here("Data", "Zulassungen", "Zulassungen_alle_Fahrzeuge.csv"))

s <- alle_zulassungen %>%
  filter(alle_zulassungen$Gemeinden == "ADELSHOFEN")
nrow(s)

#typeof(alle_zulassungen$IDNummer)
descrip_int = "The Integer data type is used for integer values.To store a value as an integer, we need to specify it as such. The integer data type is commonly used for discrete only values like unique ids.We can store as well as convert a value into an integer type using the as.integer() function.If the data consists of only numbers, like decimals, whole numbers, then we call it numeric data. In numeric data, the numbers can be positive or negative. If the data consists only of whole numbers, it is called as integer. Integers too may take negative or positive values. In the present example, the integer number 408097 serves as an example of an integer. -408097,52 would be an nummeric datatype for example."


descrip_char = "The character data type stores character values or strings. 
              Strings in R can contain the alphabet, numbers, and symbols. 
              The easiest way to denote that a value is of character type in R is to wrap the value inside single or double inverted commas."
help_df <- data.frame(Row_Name = colnames(alle_zulassungen),
                      Data_Types=c(typeof(alle_zulassungen$X),typeof(alle_zulassungen$IDNummer),typeof(alle_zulassungen$Gemeinden),typeof(alle_zulassungen$Gemeinden) ),
                      Examples=c(alle_zulassungen$X[1], alle_zulassungen$IDNummer[1], alle_zulassungen$Gemeinden[1], alle_zulassungen$Zulassung[1]),
                      Characteristics=c(descrip_int,NA))


#help_df <- table(help_df)
help_df


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
# Furthermore storing your data on a server and making it available for everyone, 
# you have possibility to easily manage the performance by scaling up or down the server if necessary. 
# Storing the data locally leads to a limitation of the performance to the given technical units of the computer.
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
# There are different providers like Heroku which allows to easily make your Application f.e. R Shiny App worldwide available. 

################################################################################################################################
# Aufgabe 6
# On 11.08.2010 there was a hit and run accident. 
# There is no trace of the license plate of the car involved in the accident. 
# The police asks for your help, as you work for the Federal Motor Transport Authority, 
# and asks where the vehicle with the body part number “K5-112-1122-79” was registered.
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

