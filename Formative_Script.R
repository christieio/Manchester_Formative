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
Health_Demo <- merge(London_Health, London_Demo, 
               by = c("Wardname", "District"), all = TRUE)

# Merge Socio and Envi on Wardcode
Socio_Envi <- merge(London_socio, London_Envi, 
                    by = "Wardcode", all = TRUE)


#Get rid of spaces before district code 
London_Dist$Districtcode <- gsub('\\s+', '', London_Dist$Districtcode)
London_Envi$Districtcode <- gsub('\\s+', '', London_Envi$Districtcode)
London_Dist$Districtcode

# Merging Socioeconomic, Environment and District codes by Districtcode
Merged <- merge(x = Socio_Envi, y = London_Dist, by = "Districtcode", all.x = TRUE)

# Merging all data together by district name
All_data <- merge(x = Merged, y = total, by = c("District", "Population2011Census"), all = TRUE)


# Trying to split data in Demo (THIS IS NOT FINISHED)
London_Demo$Count <- str_count(London_Demo$Wardname, "\\w+")
which(London_Demo$Count > 10)
London_Error <- London_Demo[c(44,  48,  52,  77,  80,  90, 111, 126, 195, 275, 280, 297, 305),]

sep <- separate(London_Error[1], col=Wardname, into=c('One', 'Two', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', 
                                                      '15', '16', '17', '18', '19', '20', '21', '22', '23', '24', '25', '26', '27', '28',
                                                      '29', '30', '31', '32', '33', '34', '35', '36', '37', '38', '39', '40',
                                                      '41', '42', '43', '44', '45', '46', '47', '48', '49', '50', '51', '52',
                                                      '53', '54', '55', '56', '57', '58', '59', '60', '61', '62', '63', '64', '65'), sep='\n')
sep <- str_split_fixed(London_Error$Wardname, "\n", n = 72)
sep_2 <- str_split(sep[[1]], "\t")
