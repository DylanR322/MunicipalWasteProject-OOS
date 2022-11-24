#### OOS Waste Data Project
## NC Municipal Data
## Dylan Radley

library(rio)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

rm(list = ls())

`%notin%` <- Negate(`%in%`)

setwd("~/Desktop/GitHub/Trash Data Project OOS/Data/NC/")
# NOTE: If running this code on a different computer or with different file structure, adjust your working directory accordingly.

### Waste Data --------------------

nc <- import("NC FY2020-21 Recycling Data.xlsx")
nc.orig <- nc

# get the colnames in the right place

colnms <- nc[1, ]
names(nc) <- colnms

# drop the extra row

nc <- nc[-1, ]

# let's try to make the columns that we are interested in, by combining several together. First, type of recycling

nc$RecyclingServiceType <- NA
nc$RecyclingServiceType[nc$`14. No Recycling Program` == 1 | nc$`14. Recycling Program` == 0] <- "None"
nc$RecyclingServiceType[nc$`15. Curbside Recycling Program` == 1 & nc$`25. Dropoff Recycling Program` == 0] <- "Curbside"
nc$RecyclingServiceType[nc$`25. Dropoff Recycling Program` == 1 & nc$`15. Curbside Recycling Program` == 0] <- "Dropoff"
nc$RecyclingServiceType[nc$`25. Dropoff Recycling Program` == 1 & nc$`15. Curbside Recycling Program` == 1] <- "Both"

# now to drop a lot of the recycling cols, including tonnages for the different materials

nc <- nc[, -c(18:27, 37:41, 50:59, 66:101)]

# now to do the same for solid waste collection.

nc$TrashServiceType <- NA
nc$TrashServiceType[nc$`5. Solid Waste Services` == 0] <- "None"
nc$TrashServiceType[grepl("at household", nc$`Frequency of primary collection`) | 
                      grepl("at household", nc$`Frequency of secondary collection`)] <- "Curbside"
nc$TrashServiceType[grepl("convenience", nc$`Frequency of primary collection`) | 
                      grepl("convenience", nc$`Frequency of secondary collection`)] <- "Dropoff"
nc$TrashServiceType[(grepl("convenience", nc$`Frequency of primary collection`) & 
                      grepl("at household", nc$`Frequency of secondary collection`)) |
                      (grepl("at household", nc$`Frequency of primary collection`) & 
                         grepl("convenience", nc$`Frequency of secondary collection`))] <- "Both"
nc$TrashServiceType[is.na(nc$TrashServiceType) & (nc$`5. Solid Waste Services` == 1 & nc$Curbside == 1)] <- "Curbside"

# drop the extra cols now

nc <- nc[, -c(6, 70:71, 87:88)]

# food waste service type. 

nc$FoodServiceType <- NA

nc$FoodServiceType[nc$`43. Curbside Food Waste Program` == 1] <- "Curbside"
nc$FoodServiceType[nc$`43. Dropoff Food Waste Program` == 1] <- "Dropoff"
nc$FoodServiceType[nc$`43. Organics/ Food Waste Recycling Program` == 0] <- "None"
nc$FoodServiceType[nc$`43. Pilot Food Waste Program` == 1] <- "Pilot"
nc$FoodServiceType[nc$`9. Backyard Composting Program (BYC)` == 1] <- "Backyard"

nc <- nc[, -c(7, 34:39)] # drop the extra columns

# now yard waste

nc$YardWasteServiceType <- NA

nc$YardWasteServiceType[nc$`49. Yard Waste Program` == 0] <- "None"
nc$YardWasteServiceType[nc$`49. Yard Waste Collected at Convenience Center` == 1 | 
                          nc$`49. Yard Waste Received at Yard Waste, Compost, or LCID Facility` == 1] <- "Dropoff"
nc$YardWasteServiceType[nc$`49. Yard Waste Collected Curbside` == 1] <- "Curbside"

nc <- nc[, -c(36:39)] # drop the extra columns

# now to get rid of household fee and budget columns that I do not need

nc <- nc[, -c(85:102, 133:137)]

# now if they provide trash, recycling, and compost bins

nc$ProvideTrashCart <- NA
nc$ProvRecycleBinorCart <- NA
nc$ProvideCompostBins <- NA

nc$ProvideTrashCart[nc$`Government- provided carts` == 1] <- "Yes"
nc$ProvideTrashCart[nc$`Government- provided carts` == 0] <- "No"

nc$ProvideCompostBins[nc$`10. BYC Bin Distribution/sales` == 1] <- "Yes"
nc$ProvideCompostBins[nc$`10. BYC Bin Distribution/sales` == 0] <- "No"

nc <- nc[, -c(9,10,73:75)] # drop extra columns

# now the total tonnages for recycling and for trash

nc$SolidWasteTons <- NA
nc$RecyclingTons <- NA
nc$YardWasteTons <- NA

# first, recode them as numeric!
numcols <- c(31, 35, 38, 41, 44, 47, 50, 88, 94, 100, 105, 117:119)
nc[, numcols] <- apply(nc[, numcols], 2, function(x) as.numeric(as.character(x)))

# First, yard waste needs to be split into different categories

divyardcols <- c(35, 38, 41, 44)
nc$DivertedYardWaste <- NA
nc$DivertedYardWaste <- apply(nc[, divyardcols], 1, function(x) sum(x, na.rm = TRUE))


trashyardcols <- c(47, 50)
nc$DisposedYardWaste <- apply(nc[, trashyardcols], 1, function(x) sum(x, na.rm = TRUE))

# now to make the uncategorized yard waste column

nc$UncatYardWaste <- nc$`Tons Collected.2` - (nc$DivertedYardWaste + nc$DisposedYardWaste)

# now to look at trash and recycling

nc$RecyclingTons <- nc$`Tons Collected.1` # this one is more accurate because it has NA's
nc$SolidWasteTons <- nc$`Tons Collected` # this is the trash column

nc <- nc[, -c(31, 35, 38, 41, 44, 47, 50, 88, 94, 100, 105, 119)]# now to drop the extra columns

# now the households served

names(nc)[14] <- "Households"

# to look at the households served, I will break them out into the different programs

names(nc)[80] <- "HHServedTrash"
names(nc)[85] <- "HHServedRecycling"
names(nc)[90] <- "HHServedYard"

nc <- nc[, -c(14:16, 27)]

# now payment: PropertyTax, AnnualFee, TransferStationAccessFee, PerVisitFee, PAYT/SMART, TipFee, DropoffFee, CurbsideFee

names(nc)[61] <- "TipFee"
names(nc)[62] <- "PAYT/SMART"
names(nc)[64] <- "PropertyTax"
names(nc)[67] <- "PerHouseCharge"

# recode the PAYT columns: if one has PAYT, I will have both say PAYT

nc$`PAYT/SMART`[nc$`PAYT/SMART` == 1 | nc$`62. PAYT` == 1] <- "Yes"
nc$`PAYT/SMART`[nc$`PAYT/SMART` == 0] <- "No"

nc <- nc[, -c(63, 65:66, 68:69)]

# now EnforceRecycling, RecyclingEnforcePersonnel

names(nc)[5] <- "RecyclingEnforcePersonnel"

# now to drop extra cols

nc <- nc[, -c(3, 6:60, 65:70, 72:75, 77:80, 82:89)]

# rename county/municipality column, and others and add a state column

names(nc)[1] <- "GovUnit"
names(nc)[3] <- "Population"
nc$State <- "NC"

### Population Density + Census Data --------------------

setwd("~/Desktop/Trash Data Project OOS/Data/Census Data/")

# load in the data that contains the land areas of each of the municipalities

areas <- import("NCMuncArea.csv")

areas <- areas[, c(1,2)] # only need to keep the land area

areas$`Municipal Name` <- toupper(areas$`Municipal Name`)

# now to merge the areas with the rest of the nc data!

nc <- merge(nc, areas, by.x = "GovUnit", by.y = "Municipal Name") # this merge will only keep the ones for which there's data

nc$PopulationDensity <- as.numeric(nc$Population) / as.numeric(nc$LANDAREA)

# now to start adding in census info, specifically from the selected economic and social characteristics.

econ <- import("NCPlaceEconChara/ACSDP5Y2020.DP03_data_with_overlays_2022-06-22T130104.csv")

names(econ)
econvars <- econ[, c(201, 245, 249, 550)] # select these columns of interest
names(econvars) <- econvars[1, ]

names(econvars)[1] <- "Households"
names(econvars)[2] <- "MedIncome"
names(econvars)[3] <- "AvgIncome"
names(econvars)[4] <- "GovUnit"

econvars <- econvars[-1, ] # this is just the colnames repeated

# standardize the place names for merging
econvars$GovUnit <- gsub(", North Carolina", "", econvars$GovUnit)
econvars$GovUnit <- gsub(" town", "", econvars$GovUnit)
econvars$GovUnit <- gsub(" CDP", "", econvars$GovUnit)
econvars$GovUnit <- gsub(" city", "", econvars$GovUnit)
econvars$GovUnit <- toupper(econvars$GovUnit)

# make the variables numeric
econvars$MedIncome <- as.numeric(econvars$MedIncome)
econvars$AvgIncome <- as.numeric(econvars$AvgIncome)
econvars$Households <- as.numeric(econvars$Households)

nc <- merge(nc, econvars, by = "GovUnit", all.x = TRUE) # merge, but ensure that all of the NC mun's are kept

# now to get educational attainment numbers

social <- import("NCPlaceSocialChara/ACSDP5Y2020.DP02_data_with_overlays_2022-07-19T151708.csv")

names(social)
socvars <- social[, c(239, 243, 247, 251, 255, 259, 263, 618)] # select these columns of interest
names(socvars) <- socvars[1, ]

socvars$PctLessHS <- NA
socvars$PctHS <- NA
socvars$PctBachelororMore <- NA

names(socvars)[8] <- "GovUnit"

socvars <- socvars[-1, ] # this is just the colnames repeated

# standardize place names for merging
socvars$GovUnit <- gsub(", North Carolina", "", socvars$GovUnit)
socvars$GovUnit <- gsub(" town", "", socvars$GovUnit)
socvars$GovUnit <- gsub(" CDP", "", socvars$GovUnit)
socvars$GovUnit <- gsub(" city", "", socvars$GovUnit)
socvars$GovUnit <- toupper(socvars$GovUnit)

# calculate the pct's and make things numeric

numcols <- c(1:7)
socvars[, numcols] <- apply(socvars[, numcols], 2, function(x) as.numeric(as.character(x)))

socvars$PctLessHS <- socvars$`Percent!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Less than 9th grade` +
  socvars$`Percent!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!9th to 12th grade, no diploma`
socvars$PctHS <- socvars$`Percent!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!High school graduate (includes equivalency)` +
  socvars$`Percent!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Some college, no degree` +
  socvars$`Percent!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Associate's degree`
socvars$PctBachelororMore <- socvars$`Percent!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Bachelor's degree` +
  socvars$`Percent!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!Graduate or professional degree`

socvars <- socvars[, 8:11] # keep just the new calculated values

nc <- merge(nc, socvars, by = "GovUnit", all.x = TRUE) # merge, again keeping all of the municipalities!

# finally, now to add in the Gini Index Data

gini <- import("NCPlaceGini/ACSDT5Y2020.B19083_data_with_overlays_2022-07-19T161707.csv")

names(gini)[1] <- "Gini"
names(gini)[4] <- "GovUnit"

gini <- gini[-1, ] # this is just the colnames repeated

gini$GovUnit <- gsub(", North Carolina", "", gini$GovUnit) # get the place names standardized for merging
gini$GovUnit <- gsub(" town", "", gini$GovUnit)
gini$GovUnit <- gsub(" CDP", "", gini$GovUnit)
gini$GovUnit <- gsub(" city", "", gini$GovUnit)
gini$GovUnit <- toupper(gini$GovUnit)

gini$Gini <- as.numeric(gini$Gini) # make it numeric

gini <- gini[, -c(2:3)] # drop the two unneeded columns

nc <- merge(nc, gini, by = "GovUnit", all.x = TRUE) # merge, but ensure that all of the NC mun's are kept

### Final Edits + Exporting --------------------

nc <- nc[, -25] # drop the land area column

# change the 0 - 1 binary variables to Yes/No

binarycols <- names(nc)[c(4:5, 7:8)]

for(i in binarycols) {
  nc[, i][nc[, i] == 1] <- "Yes"
  nc[, i][nc[, i] == 0] <- "No"
  print("tick")
}

# fix how some drop-off's are written as dropoff

nc$TrashServiceType[nc$TrashServiceType == "Dropoff"] <- "Drop-off"

# If both disposed and diverted yard waste are zero, code it as NA

nc$DisposedYardWaste[nc$DisposedYardWaste == 0 & nc$DivertedYardWaste == 0] <- NA
nc$DivertedYardWaste[nc$DivertedYardWaste == 0 & is.na(nc$DisposedYardWaste)] <- NA

# Export!

write.csv(nc, "~/Desktop/GitHub/Trash Data Project OOS/Data/CleanStateData/nc.csv")

