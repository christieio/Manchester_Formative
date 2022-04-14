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

# Merging London_Health and London_Demo

#Change name of col for intuitiveness
names(London_Demo)[names(London_Demo) == "ï..Wardname"] <- "Wardname"

#Merge health and demo on wardname
total <- merge(London_Health, London_Demo, 
               by = "Wardname")

London_socio$Districtcode <- substr(London_socio$Wardcode, 0, 4)

# Removing empty rows
London_socio <- London_socio[-c(622:657),]

# Adding district name to health
London_Health$District <- sapply(strsplit(London_Health$Wardname, "-", fixed = T), function(x) (x[1]))


names(London_Dist)[names(London_Dist) == "Dist"] <- "District"

#install.packages("dplyr")
library(dplyr)
#full join health onto dist
df= London_Dist %>% full_join(London_Health,by="District")
df
