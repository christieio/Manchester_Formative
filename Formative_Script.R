#######################FORMATIVE ASSESSMENT#####################################

#install.packages("haven")
library(haven)
library(dplyr) #for my way of merging 
library(stringr) #for str_count
library(tidyr)
#install.packages("reshape")
library(reshape)

#Read in data sets
London_Dist <- read.csv("London District codes.csv")
London_Demo <- read.table("London ward data demographics.dat", header = TRUE, sep = '\t')
London_Envi <- read.csv("London ward data environment.csv")
London_Health <- read_sas("london ward data health.sas7bdat")
London_socio <- read_sav("London ward data socioeconomic.sav")

# Checking column names
colnames(London_Dist)
colnames(London_Demo)
colnames(London_Envi)
colnames(London_Health)
colnames(London_socio)


# Changing column name for ease
names(London_Demo)[names(London_Demo) == "ï..Wardname"] <- "Wardname"
names(London_Dist)[names(London_Dist) == "ï..District"] <- "District"
names(London_Envi)[names(London_Envi) == "ï..Wardcode"] <- "Wardcode"


# Checking missing data
sum(is.na(London_Demo))
sum(is.na(London_Dist))
sum(is.na(London_Envi))
sum(is.na(London_Health))
sum(is.na(London_socio))

# Removing empty rows
which(is.na(London_socio$hhSocialRented))
London_socio <- London_socio[-c(622:657),]
sum(is.na(London_socio))

# Changing & to and
London_Demo$Wardname <- gsub("&", "and", London_Demo$Wardname)

# Extracting district code from Wardcode
London_socio$Districtcode <- substr(London_socio$Wardcode, 0, 4)
London_Envi$Districtcode <- substr(London_Envi$Wardcode, 0, 4)


# Adding district name to health
London_Health$District <- sapply(strsplit(London_Health$Wardname, "-", fixed = T), function(x) (x[1]))
London_Demo$District <- sapply(strsplit(London_Demo$Wardname, "-", fixed = T), function(x) (x[1]))
#Get rid of spaces after district 
London_Health$District <- trimws(London_Health$District, "r")
London_Demo$District <- trimws(London_Demo$District, "r")
unique(London_Health$District)
unique(London_Demo$District)

#Merge health and demo on wardname
#Health_Demo <- merge(London_Health, London_Demo, by = c("Wardname", "District"), all = TRUE)
#@Eleanor sorry, I merge differently to you but I've left your way in but commented out for when you come back to it. 
#I tend to get fewer errors this way.
Health_Demo = London_Health %>% left_join(London_Demo,by=c("Wardname", "District"))

# Merge Socio and Envi on Wardcode
#Socio_Envi <- merge(London_socio, London_Envi, by = "Wardcode", all = TRUE)
Socio_Envi = London_socio %>% left_join(London_Envi,by=c("Wardcode", "Districtcode"))

#Get rid of spaces before district code 
London_Dist$Districtcode <- gsub('\\s+', '', London_Dist$Districtcode)
London_Envi$Districtcode <- gsub('\\s+', '', London_Envi$Districtcode)
London_Dist$Districtcode


# Merging Socioeconomic, Environment and District codes by Districtcode
#Merged <- merge(x = Socio_Envi, y = London_Dist, by = "Districtcode", all.x = TRUE)
Merged = Socio_Envi %>% left_join(London_Dist, by=c("Districtcode"))

# Merging all data together by district name
#All_data <- merge(x = Merged, y = total, by = c("District", "Population2011Census"), all = TRUE)
All_data = Merged %>% left_join(Health_Demo, by=c("District", "Population2011Census"))

# Trying to split data in Demo (THIS IS NOT FINISHED)
#Find out how many characters are in the wardnames:
London_Demo$Count <- str_count(London_Demo$Wardname, "\\w+")
#If the characters are 10, it indicates that there's some extra data that's ended up in that column 
which(London_Demo$Count > 10)
#Create an data frame of the "errors" aka those cols found above that have extra data:
London_Error <- London_Demo[c(44,  48,  52,  77,  80,  90, 111, 126, 195, 275, 280, 297, 305),]

# Separating based on areas
sep <- str_split_fixed(London_Error$Wardname, "\n", n = 72)
sep <- melt(sep)
#Separating areas
sep <- str_split_fixed(sep$value, "\t", n = 6)
#Okay so what you've done here seems to have worked, I think there needs to be column names which I know how to do
# using dplyr but not this way.
#Do we know what data each column contains bc I am baffled but lets forget that for now. 

#Removing empty rows
data <- sep[!apply(sep == "", 1, all),]
#Finding missing values
which(data[,2] =="")
#Okay so I guess what we're saying is we can impute missing values when we know what the columns actually are? 
#I've looked through the other datasets to try and identify this but I've had no luck. 

#setting data matrix created above as a data frame
data = as.data.frame(data)
#now it is a data frame, can change the column name to wardname 
names(data)[names(data) == "V1"] <- "Wardname"
#Now can merge on wardname to the rest of the data
#My thought process here was maybe having it merged would help us figure out what to do next
#I don't think it works like that though. 
#What I figured out from doing this is that (maybe):
#V2 = Children
#V3 = Greaterthan65
#V4 = nonwhite
#V5 = NotBorninUK
#V6 = NotEnglishspeaking
data
All_data2 = All_data %>% left_join(data, by=c("Wardname"))


