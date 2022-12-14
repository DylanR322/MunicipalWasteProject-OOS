#### OOS Waste Data Project
## Merging Data from Different States
## Dylan Radley


library(rio)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(pdftools)

rm(list = ls())

`%notin%` <- Negate(`%in%`)

setwd("~/Desktop/GitHub/MunicipalWasteProject-OOS/Data/CleanStateData/")
# NOTE: If running this code on a different computer or with different file structure, adjust your working directory accordingly.

### MA + RI --------------------

ma <- import("ma.csv")
ri <- import("ri.csv")

dta <- bind_rows(ma, ri)

dta <- dta[, -1] # drop V1

### (MA, RI) + ME --------------------

me <- import("me.csv")
me <- me[, -1] # drop v1

dta <- bind_rows(dta, me)

### (MA, RI, ME) + Census Data --------------------

setwd("~/Desktop/GitHub/MunicipalWasteProject-OOS/Data/Census Data/")

marime <- import("MARIMECityData.csv")

# drop extra cols 
marime <- marime[, -c(1:3, 5:51, 53:55)] # these are totally extra, have like no info
marime <- marime[, -c(4, 6, 8:15)] # these are extra cols for pop density and for number of households

# rename cols
names(marime)
names(marime)[1] <- "state"
names(marime)[2] <- "Municipality"
names(marime)[3] <- "Population"
names(marime)[4] <- "PopulationDensity"
names(marime)[5] <- "Households"
names(marime)[6] <- "PopOver25"
names(marime)[7] <- "LessHS"
names(marime)[8] <- "HS"
names(marime)[9] <- "BachelororMore"
names(marime)[10] <- "MedIncome"
names(marime)[11] <- "AvgIncome"
names(marime)[12] <- "Gini"

# calculate the percentages for different educational attainments then drop extra cols for that

marime$PctLessHS <- marime$LessHS / marime$PopOver25
marime$PctHS <- marime$HS / marime$PopOver25
marime$PctBachelororMore <- marime$BachelororMore / marime$PopOver25

marime <- marime[, -c(6:9)]

# start making a common id by removing extra words and combining with state

marime$Municipality <- gsub(" town", "", marime$Municipality)
marime$Municipality <- gsub(" Town", "", marime$Municipality)# this is a little quick and dirty, but I think it works
marime$Municipality <- gsub(" city", "", marime$Municipality)
marime$Municipality <- gsub(" plantation", "", marime$Municipality)

marime$state <- toupper(marime$state)

marime <- marime %>% unite("id", 2:1, sep = "---", remove = TRUE)

dta <- dta %>% unite("id", 1:2, sep = "---", remove = TRUE)

# see what ones will not get merged then fix them NOTE: I will be leaving most of the maine ones right now, until I
# have updated ME data and know that I may actually be using it.

dta$id[dta$id %notin% marime$id]

marime$id[marime$id == "Manchester-by-the-Sea---MA"] <- "Manchester---MA"

dta <- merge(dta, marime, by = "id")

dta <- separate(dta, id,
                c("Municipality", "State"),
                sep = "---")


### (MA, RI, ME) + FL --------------------

setwd("~/Desktop/GitHub/MunicipalWasteProject-OOS/Data/CleanStateData/")

fl <- import("fl.csv")
fl <- fl[, -1] # drop V1

names(dta)[1] <- "GovUnit" # something that can be standard between the municipalities of most of our data and the 
# counties in Florida
names(fl)[1] <- "GovUnit"

# drop the .y populations and households, cause we want to use what the municipalities reported, then rename them

names(dta)[3] <- "Households"
names(dta)[35] <- "Population"

dta <- dta[, -c(39,41)]

# merge

dta <- bind_rows(dta, fl)

### (MA, RI, ME, FL) + NC --------------------

nc <- import("nc.csv")
nc <- nc[, -1] # drop V1

# merge

dta <- bind_rows(dta, nc)

### Final Cleaning + Export --------------------

# Add info to all of the rows about if it is a municipality or a county

names(dta)[47] <- "Type"
dta$Type <- "Municipality" # all are municipalities except for the Florida counties
dta$Type[dta$State == "FL"] <- "County"

# Drop Maine since the data has not been updated as of writing. I will also drop the columns that come only from it,
# such as the plastic bag ban and the polystyrene ban.

dta <- dta[dta$State != "ME", ]
dta <- dta[, -c(36:38)]

# Take states that only have info on HH served overall and assume that trash and recycling is covered in those cases.

dta$HHServedTrash[dta$State == "RI" | dta$State == "FL"] <- dta$HHServed[dta$State == "RI" | dta$State == "FL"]
dta$HHServedRecycling[dta$State == "RI" | dta$State == "FL"] <- dta$HHServed[dta$State == "RI" | dta$State == "FL"]

# same idea but for yardwaste!

dta$HHServedYard[dta$State == "RI" | dta$State == "FL" | dta$State == "MA"] <- 
  dta$HHServed[dta$State == "RI" | dta$State == "FL" | dta$State == "MA"]

# Now to just correct things a bit: if NA's are found for a service type, the HHServed for that type should be NA, and
# if the service type is none then the HHServed should be zero.

dta$HHServedTrash[dta$TrashServiceType == "None"] <- 0
dta$HHServedTrash[is.na(dta$TrashServiceType)] <- NA

dta$HHServedRecycling[dta$RecyclingServiceType == "None"] <- 0
dta$HHServedRecycling[is.na(dta$RecyclingServiceType)] <- NA

dta$HHServedYard[dta$YardWasteServiceType == "None"] <- 0
dta$HHServedYard[is.na(dta$YardWasteServiceType)] <- NA

dta <- dta[, -31] # Drop HHServed since it has been broken out into types

# Also, if service type is none, then trash and recycling tonnage should be NA, not zero

dta$SolidWasteTons[dta$TrashServiceType == "None"] <- NA
dta$RecyclingTons[dta$RecyclingServiceType == "None"] <- NA

# drop the food, municipal, school, and business service variables. 

dta <- dta[, -c(11:14)]

# drop 'is leaf / yard waste accepted' because we lack enough info to categorize it in RI

dta <- dta[, -29]

names(dta)[23] <- "FoodWaste"
names(dta)[24] <- "YardWaste"

# now to reorder the data a bit to make it more legible

dta <- dta[, c(2, 1, 38, 21, 25, 24, 41:43, 23, 3:5, 40, 6, 8, 26, 10, 7, 9, 20, 37, 16:19, 11:15, 22, 27:28, 39, 29:36)]

# put the 'yard waste' from most states in 'Uncat' yard waste

dta$UncatYardWaste[dta$State == "RI" | dta$State == "FL" | dta$State == "MA"] <- 
  dta$YardWaste[dta$State == "RI" | dta$State == "FL" | dta$State == "MA"]

# Now drop that extra yard waste column

dta <- dta[, -6]

# Convert all of the variables to numeric

dta$TrashServiceType[dta$TrashServiceType == "None"] <- NA
dta$TrashServiceType[dta$TrashServiceType == "Both"] <- 0
dta$TrashServiceType[dta$TrashServiceType == "Curbside"] <- -1
dta$TrashServiceType[dta$TrashServiceType == "Drop-off"] <- 1
dta$TrashServiceType <- as.numeric(dta$TrashServiceType)

dta$RecyclingServiceType[dta$RecyclingServiceType == "Dropoff"] <- "Drop-off"
dta$RecyclingServiceType[dta$RecyclingServiceType == "None"] <- NA
dta$RecyclingServiceType[dta$RecyclingServiceType == "Both"] <- 0
dta$RecyclingServiceType[dta$RecyclingServiceType == "Curbside"] <- -1
dta$RecyclingServiceType[dta$RecyclingServiceType == "Drop-off"] <- 1
dta$RecyclingServiceType <- as.numeric(dta$RecyclingServiceType)

dta$YardWasteServiceType[dta$YardWasteServiceType == "Dropoff"] <- "Drop-off"
dta$YardWasteServiceType[dta$YardWasteServiceType == "None"] <- NA
dta$YardWasteServiceType[dta$YardWasteServiceType == "Both"] <- 0
dta$YardWasteServiceType[dta$YardWasteServiceType == "Curbside"] <- -1
dta$YardWasteServiceType[dta$YardWasteServiceType == "Drop-off"] <- 1
dta$YardWasteServiceType <- as.numeric(dta$YardWasteServiceType)

dta$FoodServiceType[dta$FoodServiceType == "No" | dta$FoodServiceType == "None" | dta$FoodServiceType == "Private"] <- 0
dta$FoodServiceType[dta$FoodServiceType == "Backyard" | dta$FoodServiceType == "Both" | 
                      dta$FoodServiceType == "Curbside" | dta$FoodServiceType == "Drop-off"] <- 1
dta$FoodServiceType <- as.numeric(dta$FoodServiceType)
names(dta)[17] <- "FoodService"

recodeYNbin <- function(variable){
  variable[variable == "Yes"] <- 1
  variable[variable == "No"] <- 0
  variable <- as.numeric(variable)
  
  return(variable)
}

binarycols <- c(18:34)
dta[, binarycols] <- apply(dta[, binarycols], 2, function(x) recodeYNbin(x))

# Drop SingleorMulti because there is not much data for it, so it being recoded wrong is not an issue

dta <- dta[, -21]

# Multiply pct bachelor or more and the others by 100 in states that are not NC

dta$PctBachelororMore[dta$State != "NC"] <- dta$PctBachelororMore * 100
dta$PctLessHS[dta$State != "NC"] <- dta$PctLessHS * 100
dta$PctHS[dta$State != "NC"] <- dta$PctHS * 100

# Export it!

write.csv(dta, "~/Desktop/GitHub/Trash Data Project OOS/Files/dta.csv")

