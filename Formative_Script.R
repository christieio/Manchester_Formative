#######################FORMATIVE ASSESSMENT#####################################

#install.packages("haven")
library(haven)
library(dplyr) #for my way of merging 
library(stringr) #for str_count
library(tidyr)
#install.packages("reshape")
library(reshape)
#install.packages("plyr")
library(plyr) #used for rbindfill 
#install.packages("mice")
library(mice)
#install.packages("corrplot")
library(corrplot)
#install.packages("VIM")
library(VIM)

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

# Trying to split data in Demo (THIS IS NOT FINISHED)
#Find out how many characters are in the wardnames:
London_Demo$Count <- str_count(London_Demo$Wardname, "\\w+")
#If the characters are 10, it indicates that there's some extra data that's ended up in that column 
which(London_Demo$Count > 10)
#Create an data frame of the "errors" aka those cols found above that have extra data:
London_Error <- London_Demo[c(44,  48,  52,  77,  80,  90, 111, 126, 195, 275, 280, 297, 305),]


London_Error$Wardname <- str_c(London_Error$Wardname, '\t', London_Error$Children, '\t', London_Error$Greaterthan65, 
                               '\t', London_Error$nonwhite, '\t', London_Error$NotBorninUK, '\t', London_Error$NotEnglishspeaking)

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
# Adding prepared data back into the dataset
London_Demo <- London_Demo[-c(44,  48,  52,  77,  80,  90, 111, 126, 195, 275, 280, 297, 305),]

# Changing into dataframe
Data <- as.data.frame(data)
# Renaming columns
names(Data)[1] <- "Wardname"
names(Data)[2] <- "Children"
names(Data)[3] <- "Greaterthan65"
names(Data)[4] <- "nonwhite"
names(Data)[5] <- "NotBorninUK"
names(Data)[6] <- "NotEnglishspeaking"

# Merging changed data
#London_Demo <- rbind(London_Demo, Data) #this doesn't work bc London_Demo has a dif number of cols to data.
#Instead use rbind fill to join data which has dif nums of cols. 
#However, this has created a fair few missing vals but should be fine in the end. 
London_Demo = rbind.fill(London_Demo, Data)

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

# Removing amostphes in Health
London_Health$Wardname <- gsub("'", '', London_Health$Wardname)

#Merge health and demo on wardname
#Health_Demo <- merge(London_Health, London_Demo, by = c("Wardname", "District"), all = TRUE)
#@Eleanor sorry, I merge differently to you but I've left your way in but commented out for when you come back to it. 
#I tend to get fewer errors this way.
Health_Demo = London_Health %>% right_join(London_Demo,by=c("Wardname", "District"))

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


#I think we can now drop that count column bc that's got a lot of NAs bc we only did it to one dataset. 
All_data = subset(All_data, select = -c(Count))

#Checking missing data
sum(is.na(All_data))
md.pattern(All_data)
colSums(is.na(All_data))
which(colSums(is.na(All_data))>0)
names(which(colSums(is.na(All_data))>0))

#Gonna try mean imputation on a couple cols:
All_data$Population2011Census[is.na(All_data$Population2011Census)] <- mean(All_data$Population2011Census, na.rm = TRUE)
All_data$Crimerate[is.na(All_data$Crimerate)] <- mean(All_data$Crimerate, na.rm = TRUE)
All_data$Openspace[is.na(All_data$Openspace)] <- mean(All_data$Openspace, na.rm = TRUE)
All_data$GeneralFertilityRate[is.na(All_data$GeneralFertilityRate)] <- mean(All_data$GeneralFertilityRate, na.rm = TRUE)
All_data$Malelifeexpectancy[is.na(All_data$Malelifeexpectancy)] <- mean(All_data$Malelifeexpectancy, na.rm = TRUE)
All_data$Femalelifeexpectancy[is.na(All_data$Femalelifeexpectancy)] <- mean(All_data$Femalelifeexpectancy, na.rm = TRUE)
All_data$Children[is.na(All_data$Children)] <- mean(All_data$Children, na.rm = TRUE)
All_data$Greaterthan65[is.na(All_data$Greaterthan65)] <- mean(All_data$Greaterthan65, na.rm = TRUE)
All_data$nonwhite[is.na(All_data$nonwhite)] <- mean(All_data$nonwhite, na.rm = TRUE)
All_data$NotBorninUK[is.na(All_data$NotBorninUK)] <- mean(All_data$NotBorninUK, na.rm = TRUE)
All_data$NotEnglishspeaking[is.na(All_data$NotEnglishspeaking)] <- mean(All_data$NotEnglishspeaking, na.rm = TRUE)
#Mean imputation got it down from 219 to 165 NAs
sum(is.na(All_data))

#Multiple Imputation 
All_DataNONA <- mice(All_data,m=10,maxit=50,meth='pmm')
summary(All_DataNONA)
#Checking missing values on multiple imputation, reduced from 165 to 138
# This makes sense to me bc some of the NAs are like place names and shit which can't be imputed. 
completeData <- complete(All_DataNONA,2)
sum(is.na(completeData))

#Turns out we have 23 rows of missing data left, Tom had 10 rows left but Tom was better than us so it makes sense. 
sum(!complete.cases(All_data))
#Below tells you which rows are missing
which(rowSums(is.na(All_data))>0)

#Which rows:
#55,56,117, 251, 294, 352, 353, 372, 385, 386, 401, 402, 443, 480, 492, 500, 535, 541, 556, 557, 594, 616

#Now all that's left is to build the predictive model (unless you wanna try getting NAs down more but idk):

