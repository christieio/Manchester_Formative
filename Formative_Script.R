#######################FORMATIVE ASSESSMENT#####################################

install.packages("haven")
library(haven)

#Read in data sets
London_Dist <- read.csv("London District codes.csv")
London_Demo <- read.table("London ward data demographics.dat", header = TRUE, sep = '\t')
London_Envi <- read.csv("London ward data environment.csv")
London_Health <- read_sas("london ward data health.sas7bdat")
London_socio <- read_sav("London ward data socioeconomic.sav")



names(London_Demo)[names(London_Demo) == "ï..Wardname"] <- "Wardname"

total <- merge(London_Health, London_Demo, 
               by = "Wardname")

#2 have ward name, 2 have ward code, district just has district and district code. 

#Compare datasets
install.packages("arsenal")
library(arsenal)
comparedf(London_Dist, London_Health)

