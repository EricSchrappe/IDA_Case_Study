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
setwd("~/Rproject_IDA/Data/Logistikverzug")
komponenten_k7 <- read.csv2("Komponente_K7.csv")
nrow(komponenten_k7)


logistikverzug_k7 <- read.csv("Logistikverzug_K7.csv")
nrow(logistikverzug_k7)


res <- merge(komponenten_k7, logistikverzug_k7, by = "IDNummer")
# Issued one day after production date -> production date + 1
res$verspaetung_in_tagen <- as.integer((as.Date(res$Wareneingang) - (as.Date(res$Produktionsdatum)+1)))
head(res,20)
logistics_delay <- data.frame(res$IDNummer, res$Produktionsdatum, res$Wareneingang, res$verspaetung_in_tagen)
hist(logistics_delay$res.verspaetung_in_tagen,xlim=c(0,13), xlab="Anzahl der Tage", ylab="Summe der Komponenten", main="Histogramm des Logistikverzugs in Tagen", col="gray")


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
# How many of the components T16 ended up in vehicles registered in Adelshofen?6.
#################################################################################################################################


setwd("~/Rproject_IDA/Data/Einzelteil")
x <- read.table("Einzelteil_T02.txt", header = FALSE, sep = "|")         
# Aufgabe 6
gesuchte_karosserie <- 'K5-112-1122-79'

# Pfad setzen -> CSV einlesen alle Zulassungen
setwd("~/Rproject_IDA/Data/Zulassungen")
alle_zulassungen <- read.csv2("Zulassungen_alle_Fahrzeuge.csv")

# Pfad setzen -> CSV einlesen Bestandteile Fahrzeuge OEM1
setwd("~/Rproject_IDA/Data/Fahrzeug")
Bestandteile_Fahrzeuge_OEM1_Typ12 <- read.csv2("Bestandteile_Fahrzeuge_OEM1_Typ12.csv")

# In Bestandteile Fahrzeuge OEM1 gesuchte Karosserie suchen
found <- Bestandteile_Fahrzeuge_OEM1_Typ12 %>%
  filter(Bestandteile_Fahrzeuge_OEM1_Typ12$ID_Karosserie == gesuchte_karosserie)

# checken ob empty -> function erstellen zum abfragen -> TODO: umcoden
if(found!=0){
  filter_function_karosserie <- function(suche){
    Bestandteile_Fahrzeuge_OEM1_Typ12 %>%
      filter(Bestandteile_Fahrzeuge_OEM1_Typ12$ID_Karosserie == suche)
  }
}else{
  sprintf("Die gesuchte Karosserie [%s] wurde in den Datensätzen nicht gefunden", gesuchte_karosserie)
}

found <- filter_function_karosserie(Bestandteile_Fahrzeuge_OEM1_Typ12)
search_IDNummer <- noquote(found$ID_Fahrzeug)

search_in_zulassung <- function(x){
  alle_zulassungen %>%
    filter(alle_zulassungen$IDNummer == x)
}

result_row <- search_in_zulassung(search_IDNummer)
sprintf("Die gesuchte Karosserie [%s] wurde %s in der Gemeinde %s zugelassen",gesuchte_karosserie,result_row$Zulassung, result_row$Gemeinden )


