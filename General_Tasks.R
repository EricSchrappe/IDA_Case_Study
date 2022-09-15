# Install relevant packages

init <- function() {
  if (!require('dplyr')) {
    install.packages("dplyr")
    library(dplyr)
  }
  if (!require('tidyr')) {
    install.packages('tidyr')
    library(tidyr)
  }
  if (!require('stringr')) {
    install.packages('stringr')
    library(stringr)
  }
  if (!require('readr')) {
    install.packages("readr")
    library(readr)
  }
  if (!require('knitr')) {
    install.packages("knitr")
    library(readr)
  }
  if (!require('here')) {
    install.packages("here")
    library(here)
  }
  if (!require('ggplot2')) {
    install.packages("ggplot2")
    library(ggplot2)
  }
  if (!require('fitdistrplus')) {
    install.packages("fitdistrplus")
    library(fitdistrplus)
  }
  if (!require('actuar')) {
    install.packages("actuar")
    library(actuar)
  }
  if (!require('purrr')) {
    install.packages("purrr")
    library(purrr)
  }
  if (!require('plotly')) {
    install.packages("plotly")
    library(plotly)
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
komponenten_k7 <-
  read.csv2(here("Data", "Logistikverzug", "Komponente_K7.csv"))

# Import data which includes receiving date
logistikverzug_k7 <-
  read.csv(here("Data", "Logistikverzug", "Logistikverzug_K7.csv"))

# Merge tables by IDNummber (id number)
res <- merge(komponenten_k7, logistikverzug_k7, by = "IDNummer")

# Convert to dates in order to be able to calculate difference in days. Plus determination of weekday to consider weekends.
logistics_delay <-
  data.frame(
    IDNummer = res$IDNummer,
    Produktionsdatum = as.Date(res$Produktionsdatum),
    Wareneingang = as.Date(res$Wareneingang),
    PWD = as.POSIXlt(res$Produktionsdatum)$wday
  )

# Calculate date difference without weekends -> help function
date_diff_excluding_wekeends <- function(x, y, z) {
  if (is.na(x) || is.na(y) || is.na(z))
    return(NA)
  diff <- difftime(y, x, units = "days")
  # Because logistics does not work during the weekend, all parts produced during the weekend are shipped on Mondays.
  # For Saturdays, we thus need to reduce the delay by two days.
  if (z == 6) {
    return (diff - 2)
  } else {
    # For Sundays and all other days, we reduce the delay by one day.
    return (diff - 1)
  }
}

# Vectorize function
date_diff_excluding_wekeends_V <-
  Vectorize(date_diff_excluding_wekeends)

# Mutate and calculate the delay
logistics_delay <- logistics_delay %>%
  mutate(Verzoegerung_in_Tagen = as.integer(
    date_diff_excluding_wekeends_V(
      logistics_delay$Produktionsdatum,
      logistics_delay$Wareneingang,
      logistics_delay$PWD
    )
  ))

delay_in_days <- logistics_delay$Verzoegerung_in_Tagen

# Draw histogram to see the distribution of how many components have been delivered with how much delay

fig <-
  plot_ly(
    x = logistics_delay$Verzoegerung_in_Tagen,
    type = "histogram",
    nbinsx = 25,
    alpha = 0.8
  ) %>%
  layout(
    yaxis = list(title = "Anzahl der Teile"),
    xaxis = list(title = "Verspaetung in Tagen", tickmode = 'linear'),
    title = "Plot: Verteilung der Verspaetung in Tagen"
  )

fig

# Specification of distribution from data: (https://www.r-project.org/conferences/useR-2009/slides/Delignette-Muller+Pouillot+Denis.pdf)
#   1. Choose among a family of distributions the best candidates
#   2. Estimate the distribution parameters and their uncertainty
#   3. Assess and compate the goodness-of-fit of several distributions
# Here: Discrete data (days are only measured in integers)

## Step 1: Generate skewness-kurtosis graph to choose distributions

descdist(delay_in_days, discrete = TRUE, boot = 100)

# summary statistics
# ------
#   min:  2   max:  14
# median:  6
# mean:  5.937319
# estimated sd:  1.072196
# estimated skewness:  0.4049263
# estimated kurtosis:  3.521412

## Result: Negative Binomial and Poisson might fit best (Question: Negative Binomial even valid in theory? No binary data here.)

## Step 2: Fit given distributions (Always norm the data to min = 0 because distributions start with 0)

# https://www.youtube.com/watch?v=5klSpGC2puU

dists <- c("nbinom", "pois", "norm")
fit <- list()
for (i in 1:length(dists)) {
  if (dists[i] == "nbinom") {
    #fit by MLE
    fit[[i]] <-
      fitdist(delay_in_days - min(delay_in_days), dists[i])
  } else {
    fit[[i]] <-
      fitdist(delay_in_days - min(delay_in_days), dists[i], method = "mme")
  }
}

for (i in 1:length(dists)) {
  print(summary(fit[[i]]))
}

#Plot the results
par(mfrow = c(2, 2))
plot.legend <- dists
denscomp(fit, legendtext = plot.legend)
cdfcomp (fit, legendtext = plot.legend)
qqcomp  (fit, legendtext = plot.legend)
ppcomp  (fit, legendtext = plot.legend)

# Fitting of the distribution ' nbinom ' by maximum likelihood
# Parameters :
#   estimate  Std. Error
# size 1.424912e+07         NaN
# mu   3.936833e+00 0.003583757
# Loglikelihood:  -537638   AIC:  1075280   BIC:  1075301
# Correlation matrix:
#   size  mu
# size    1 NaN
# mu    NaN   1


# Fitting of the distribution ' pois ' by matching moments
# Parameters :
#   estimate
# lambda 3.937319
# Loglikelihood:  -537638   AIC:  1075278   BIC:  1075289

# Fitting of the distribution ' norm ' by matching moments
# Parameters :
#   estimate
# mean 3.937319
# sd   1.072194
# Loglikelihood:  -456255   AIC:  912513.9   BIC:  912535.2

## Step 3: Assess goodness-of- fit

goodness_of_fit <- list()
for (i in 1:length(dists)) {
  goodness_of_fit[[i]] <- gofstat(fit[[i]], discrete = TRUE)
}

for (i in 1:length(dists)) {
  print(goodness_of_fit[[i]])
}

# Chi-squared statistic:  140426.3
# Degree of freedom of the Chi-squared distribution:  5
# Chi-squared p-value:  0
# Chi-squared table:
#   obscounts theocounts
# <= 1   1251.00   29520.26
# <= 2  19057.00   46337.81
# <= 3  87698.00   60808.06
# <= 4 115701.00   59847.79
# <= 5  60561.00   47122.15
# <= 6  17757.00   30918.67
# <= 7   3729.00   17388.81
# > 7     736.00   14546.45
#
# Goodness-of-fit criteria
# 1-mle-nbinom
# Akaike's Information Criterion      1075280
# Bayesian Information Criterion      1075301

# Chi-squared statistic:  140427.9
# Degree of freedom of the Chi-squared distribution:  6
# Chi-squared p-value:  0
# Chi-squared table:
#   obscounts theocounts
# <= 1   1251.00   29508.79
# <= 2  19057.00   46326.72
# <= 3  87698.00   60801.03
# <= 4 115701.00   59848.27
# <= 5  60561.00   47128.35
# <= 6  17757.00   30926.56
# <= 7   3729.00   17395.39
# > 7     736.00   14554.90
#
# Goodness-of-fit criteria
# 1-mme-pois
# Akaike's Information Criterion    1075278
# Bayesian Information Criterion    1075289

# Chi-squared statistic:  72752.61
# Degree of freedom of the Chi-squared distribution:  5
# Chi-squared p-value:  0
# Chi-squared table:
#   obscounts  theocounts
# <= 1   1251.0000    942.8401
# <= 2  19057.0000   9904.1360
# <= 3  87698.0000  47693.4700
# <= 4 115701.0000 101848.5199
# <= 5  60561.0000  96813.8532
# <= 6  17757.0000  40953.6629
# <= 7   3729.0000   7677.0286
# > 7     736.0000    656.4892
#
# Goodness-of-fit criteria
# 1-mme-norm
# Akaike's Information Criterion   912513.9
# Bayesian Information Criterion   912535.2

## Result: Both distributions fit equally well, but overall they don't really fit.
## Therefore, same process - now with continuous distributions (should work due to n being high enough)

## Step 1: Generate skewness-kurtosis graph to choose distributions

par(mfrow = c(1, 1))
descdist(delay_in_days, discrete = FALSE, boot = 100)

# summary statistics
# ------
#   min:  2   max:  14
# median:  6
# mean:  5.937319
# estimated sd:  1.072196
# estimated skewness:  0.4049263
# estimated kurtosis:  3.521412

## Result: Pretty clearly lognormal, Weibull is close so we take it as comparison.

## Step 2: Fit given distributions

dists <- c("lnorm", "weibull", "norm")
fit <- list()
for (i in 1:length(dists)) {
  if (dists[i] == "weibull") {
    # https://stats.stackexchange.com/questions/441516/how-to-fit-weibull-distribution-using-mme-method-and-find-the-estimates-in-r
    #function to calculate sample raw moment
    memp  <-  function(x, order) {
      mean(x ^ order)
    }
    
    #fit by MME
    fit[[i]] <-
      fitdist(
        delay_in_days,
        dists[i],
        method = "mme",
        order = c(1, 2),
        memp = memp,
        start = list(shape = 6, scale = 6),
        lower = 0,
        upper = Inf
      )
  } else {
    fit[[i]] <- fitdist(delay_in_days, dists[i], method = "mme")
  }
}

for (i in 1:length(dists)) {
  print(summary(fit[[i]]))
}

#Plot the results
par(mfrow = c(2, 2))
plot.legend <- dists
denscomp(fit, legendtext = plot.legend)
cdfcomp (fit, legendtext = plot.legend)
qqcomp  (fit, legendtext = plot.legend)
ppcomp  (fit, legendtext = plot.legend)

# Fitting of the distribution ' lnorm ' by matching moments
# Parameters :
#   estimate
# meanlog 1.7652124
# sdlog   0.1791387
# Loglikelihood:  -453814.5   AIC:  907633   BIC:  907654.3

# Fitting of the distribution ' weibull ' by matching moments
# Parameters :
#   estimate
# shape 6.475965
# scale 6.373290
# Loglikelihood:  -478722.3   AIC:  957448.7   BIC:  957469.9

# Fitting of the distribution ' norm ' by matching moments
# Parameters :
#   estimate
# mean 5.937319
# sd   1.072194
# Loglikelihood:  -456255   AIC:  912513.9   BIC:  912535.2

## Step 3: Assess goodness-of- fit

goodness_of_fit <- list()
for (i in 1:length(dists)) {
  goodness_of_fit[[i]] <- gofstat(fit[[i]])
}

for (i in 1:length(dists)) {
  print(goodness_of_fit[[i]])
}

# Goodness-of-fit statistics
# 1-mme-lnorm
# Kolmogorov-Smirnov statistic 2.065082e-01
# Cramer-von Mises statistic   2.262175e+03
# Anderson-Darling statistic   1.185802e+04
#
# Goodness-of-fit criteria
# 1-mme-lnorm
# Akaike's Information Criterion    907633.0
# Bayesian Information Criterion    907654.3

# Goodness-of-fit statistics
# 1-mme-weibull
# Kolmogorov-Smirnov statistic     0.2383077
# Cramer-von Mises statistic    2534.8045909
# Anderson-Darling statistic             Inf
#
# Goodness-of-fit criteria
# 1-mme-weibull
# Akaike's Information Criterion      957448.7
# Bayesian Information Criterion      957469.9

# Goodness-of-fit statistics
# 1-mme-norm
# Kolmogorov-Smirnov statistic 2.065909e-01
# Cramer-von Mises statistic   2.258418e+03
# Anderson-Darling statistic   1.173375e+04
#
# Goodness-of-fit criteria
# 1-mme-norm
# Akaike's Information Criterion   912513.9
# Bayesian Information Criterion   912535.2

## Result: Log-norm distribution fits much better than Weibull, but still not really good

################################################################################################################################
# Aufgabe 1
# b)  Determine the mean of the logistics delay (watch out for weekends). Please interpret this number and discuss possible alternatives.
#################################################################################################################################

################################################################################################################################
# Aufgabe 1
# c)  Visualize the distribution in an appropriate way by displaying the histogram and the density function using “plotly”.
#     Please describe how you selected the size of the bins.
#################################################################################################################################

delay_density <- density(delay_in_days, bw = 0.5)

plot_ly(x = delay_in_days, type = "histogram", name = "Histogram") %>%
  add_trace(
    x = delay_density$x,
    y = delay_density$y,
    type = "scatter",
    mode = "lines",
    fill = "tozeroy",
    yaxis = "y2",
    name = "Density"
  ) %>%
  layout(yaxis2 = list(overlaying = "y", side = "right"))

################################################################################################################################
# Aufgabe 1
# d)  Please describe how you proceed, if you have to create a decision tree, which is describing the classification problem to classify whether the component (K7) is defective (Fehlerhaft) or not?
#     (Hint: You might want to work with visualizations.)
#################################################################################################################################

txt_k7 <- readLines(here("Data", "Komponente", "Komponente_K7.txt"))
txt_k7[1] <- paste0('"ID"\t', txt_k7)
df_k7 <- read_delim(I(txt_k7), delim = "\t", trim_ws = TRUE)
df_b_k7 <-
  read.csv2(here("Data", "Komponente", "Bestandteile_Komponente_K7.csv"))

ggplot(data = df_k7, aes(x = Herstellernummer, fill = as.factor(Fehlerhaft))) +
  geom_bar(stat = "bin", binwidth = 1) +
  theme_minimal()
# Only Manufacturers #112 and #114

ggplot(data = df_k7, aes(x = Werksnummer, fill = as.factor(Fehlerhaft))) +
  geom_bar(stat = "bin", binwidth = 1) +
  theme_minimal()
# Only Plants #1132 and #1142

df_k7_f_only <- df_k7 %>%
  filter(Fehlerhaft == 1)

ggplot(data = df_k7_f_only, aes(x = Fehlerhaft_Datum)) +
  geom_freqpoly() +
  theme_minimal()

ggplot(data = df_k7_f_only, aes(x = Fehlerhaft_Fahrleistung)) +
  geom_freqpoly() +
  theme_minimal()

txt_t34 <-
  readLines(here("Data", "Einzelteil", "Einzelteil_T34.txt"))
txt_t34 <- paste('"ID" | |', txt_t34)
txt_t34 <-
  gsub(x = txt_t34,
       pattern = "\\s\\|\\s\\|\\s",
       replacement = ",")
txt_t34 <-
  gsub(x = txt_t34,
       pattern = '\"\"',
       replacement = '\"\n\"')
txt_t34 <- substring(txt_t34, 1, nchar(txt_t34) - 1)
df_t34 <- read_delim(I(txt_t34), delim = ",", trim_ws = TRUE)
df_t34 <- df_t34 %>% select("ID_T34", "Fehlerhaft")

txt_t35 <-
  readLines(here("Data", "Einzelteil", "Einzelteil_T35.txt"))
txt_t35 <- paste0('"ID"\\', txt_t35)
txt_t35 <- gsub(x = txt_t35,
                pattern = '\"\"',
                replacement = '\"\n\"')
txt_t35 <- gsub(x = txt_t35,
                pattern = 'NA\"',
                replacement = 'NA\n\"')
txt_t35 <- substring(txt_t35, 1, nchar(txt_t35) - 1)
df_t35 <- read_delim(I(txt_t35), delim = "\\", trim_ws = TRUE)
df_t35 <- df_t35 %>% select("ID_T35.x", "Fehlerhaft.x")

df_k7_merge <- df_k7 %>%
  select("ID_Karosserie", "Fehlerhaft") %>%
  merge(df_b_k7, by.x = "ID_Karosserie", by.y = "ID_K7") %>%
  merge(df_t34, by = "ID_T34") %>%
  filter(Fehlerhaft.x == Fehlerhaft.y)

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
txt_t16 <-
  readLines(here("Data", "Einzelteil", "Einzelteil_T16.txt"))
txt_t16 <- paste('"ID" | |', txt_t16)
txt_t16 <-
  gsub(x = txt_t16,
       pattern = "\\s\\|\\s\\|\\s",
       replacement = ",")
txt_t16 <- gsub(x = txt_t16,
                pattern = "\\s",
                replacement = "\n")
txt_t16 <- substring(txt_t16, 1, nchar(txt_t16) - 1)
df_t16 <- read_delim(I(txt_t16), delim = ",", trim_ws = TRUE)
df_t16 <- dplyr::select(df_t16, -1)
rm(txt_t16)
ids_t16 <- df_t16$ID_T16.x
rm(df_t16)

# Pull data about relevant cars
alle_zulassungen <-
  read.csv2(here("Data", "Zulassungen", "Zulassungen_alle_Fahrzeuge.csv"))
adelshofen_zulassungen <- alle_zulassungen %>%
  filter(alle_zulassungen$Gemeinden == "ADELSHOFEN")

# Pull data about which cars contain which components
Bestandteile_Fahrzeuge_OEM1_Typ11 <-
  read.csv2(here("Data", "Fahrzeug", "Bestandteile_Fahrzeuge_OEM1_Typ11.csv")) %>% dplyr::select(-1)
Bestandteile_Fahrzeuge_OEM1_Typ12 <-
  read.csv2(here("Data", "Fahrzeug", "Bestandteile_Fahrzeuge_OEM1_Typ12.csv")) %>% dplyr::select(-1)
Bestandteile_Fahrzeuge_OEM2_Typ21 <-
  read.csv2(here("Data", "Fahrzeug", "Bestandteile_Fahrzeuge_OEM2_Typ21.csv")) %>% dplyr::select(-1)
Bestandteile_Fahrzeuge_OEM2_Typ22 <-
  read.csv2(here("Data", "Fahrzeug", "Bestandteile_Fahrzeuge_OEM2_Typ22.csv")) %>% dplyr::select(-1)
Bestandteil_Fahrzeuge <-
  rbind(
    Bestandteile_Fahrzeuge_OEM1_Typ11,
    Bestandteile_Fahrzeuge_OEM1_Typ12,
    Bestandteile_Fahrzeuge_OEM2_Typ21,
    Bestandteile_Fahrzeuge_OEM2_Typ22
  )
rm(
  Bestandteile_Fahrzeuge_OEM1_Typ11,
  Bestandteile_Fahrzeuge_OEM1_Typ12,
  Bestandteile_Fahrzeuge_OEM2_Typ21,
  Bestandteile_Fahrzeuge_OEM2_Typ22
)

# First filter components that containt part T16 by looking at datasets
# Then importing relevant datasets
Bestandteile_Komponente_K2LE2 <-
  read.csv2(here("Data", "Komponente", "Bestandteile_Komponente_K2LE2.csv"))
Bestandteile_Komponente_K2ST2 <-
  read.csv2(here("Data", "Komponente", "Bestandteile_Komponente_K2ST2.csv"))
Bestandteile_Komponente_K2LE2 <-
  Bestandteile_Komponente_K2LE2 %>% dplyr::select(c("ID_T16", "ID_K2LE2")) %>% rename(ID_Komponente = ID_K2LE2)
Bestandteile_Komponente_K2ST2 <-
  Bestandteile_Komponente_K2ST2 %>% dplyr::select(c("ID_T16", "ID_K2ST2")) %>% rename(ID_Komponente = ID_K2ST2)
Bestandteile_Komponenten <-
  rbind(Bestandteile_Komponente_K2LE2,
        Bestandteile_Komponente_K2ST2)

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
alle_zulassungen <-
  read.csv2(here("Data", "Zulassungen", "Zulassungen_alle_Fahrzeuge.csv"))

s <- alle_zulassungen %>%
  filter(alle_zulassungen$Gemeinden == "ADELSHOFEN")
nrow(s)

#typeof(alle_zulassungen$IDNummer)
descrip_int = "The Integer data type is used for integer values.To store a value as an integer, we need to specify it as such. The integer data type is commonly used for discrete only values like unique ids.We can store as well as convert a value into an integer type using the as.integer() function.If the data consists of only numbers, like decimals, whole numbers, then we call it numeric data. In numeric data, the numbers can be positive or negative. If the data consists only of whole numbers, it is called as integer. Integers too may take negative or positive values. In the present example, the integer number 408097 serves as an example of an integer. -408097,52 would be an nummeric datatype for example."


descrip_char = "The character data type stores character values or strings.
              Strings in R can contain the alphabet, numbers, and symbols.
              The easiest way to denote that a value is of character type in R is to wrap the value inside single or double inverted commas."
help_df <- data.frame(
  Row_Name = colnames(alle_zulassungen),
  Data_Types = c(
    typeof(alle_zulassungen$X),
    typeof(alle_zulassungen$IDNummer),
    typeof(alle_zulassungen$Gemeinden),
    typeof(alle_zulassungen$Gemeinden)
  ),
  Examples = c(
    alle_zulassungen$X[1],
    alle_zulassungen$IDNummer[1],
    alle_zulassungen$Gemeinden[1],
    alle_zulassungen$Zulassung[1]
  ),
  Characteristics = c(descrip_int, NA)
)


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
# A lot of challenges, like security aspects, are therefore already taken care of.
# Furthermore storing your data on a server and making it available for everyone,
# you have possibility to easily manage the performance by scaling up or down the server if necessary.
# Storing the data locally leads to a limitation of the performance to the given technical units of the computer.

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
alle_zulassungen <-
  read.csv2(here("Data", "Zulassungen", "Zulassungen_alle_Fahrzeuge.csv"))

# CSV einlesen Bestandteile Fahrzeuge OEM1
Bestandteile_Fahrzeuge_OEM1_Typ12 <-
  read.csv2(here("Data", "Fahrzeug", "Bestandteile_Fahrzeuge_OEM1_Typ12.csv"))

# Filtern nach gesuchter Karosserie und weitere Suche in Zulassungen
result_row <- Bestandteile_Fahrzeuge_OEM1_Typ12 %>%
  filter(ID_Karosserie == gesuchte_karosserie) %>%
  inner_join(alle_zulassungen, by.x = "ID_Fahrzeug", by.y = "IDNummer")

sprintf(
  "Das Fahrzeug [%s] mit der gesuchten Karosserie [%s] wurde %s in der Gemeinde %s zugelassen",
  result_row["IDNummer"],
  gesuchte_karosserie,
  result_row["Zulassung"],
  result_row["Gemeinden"]
)
