#######################FORMATIVE ASSESSMENT#####################################

#install.packages("haven")
library(haven)

#Read in data sets
London_Dist <- read.csv("London District codes.csv")
London_Demo <- read.table("London ward data demographics.dat", header = TRUE, sep = '\t')
London_Envi <- read.csv("London ward data environment.csv")
London_Health <- read_sas("london ward data health.sas7bdat")
London_socio <- read_sav("London ward data socioeconomic.sav")
#2 have ward name, 2 have ward code, district just has district and district code. 

# Changing column name for ease
names(London_Demo)[names(London_Demo) == "ï..Wardname"] <- "Wardname"

#Merge health and demo on wardname
total <- merge(London_Health, London_Demo, 
               by = "Wardname")

London_socio$Districtcode <- substr(London_socio$Wardcode, 0, 4)

# Removing empty rows
London_socio <- London_socio[-c(622:657),]

# Adding district name to health
London_Health$District <- sapply(strsplit(London_Health$Wardname, "-", fixed = T), function(x) (x[1]))

#Changing district name for intuitiveness 
names(London_Dist)[names(London_Dist) == "Dist"] <- "District"

#Get rid of spaces before district code 
London_Dist$Districtcode <- gsub('\\s+', '', London_Dist$Districtcode)
London_Dist

#Join london dist to london socio
library(dplyr)
df= London_Dist %>% left_join(London_socio,by="Districtcode")
df

names(London_Envi)[names(London_Envi) == "ï..Wardcode"] <- "Wardcode"

