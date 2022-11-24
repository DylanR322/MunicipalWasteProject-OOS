#### OOS Waste Data Project
## MA Municipal Data
## Dylan Radley

library(rio)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

rm(list = ls())

setwd("~/Desktop/GitHub/Trash Data Project OOS/Data/MA/")
# NOTE: If running this code on a different computer or with different file structure, adjust your working directory accordingly.


## Import the data

ma <- import("madata20.xlsx", which = 2)
ma.orig <- ma

### Basic Cleaning --------------------

# get a vector of all the col names, which are the third row. Then, make any needed tweaks and replace colnames in dataframe with them

columns <- ma[2, ]
columns[1] <- "Municipality"

names(ma) <- columns
ma <- ma[-(1:2),] # remove extra columns now

# examine and then get rid of the 'did not report' municipalities

didnotreport <- ma[ma$`Contact Name` == "- - DID NOT REPORT - -", ]
ma <- ma[ma$`Contact Name` != "- - DID NOT REPORT - -", ]

### Separate Yard Waste, Consolidate Trash and Recycling --------------------

# Separate the yard waste columns for later!

Yard <- ma[, c(1, 14, 15)]

# drop cols about yard waste pickup freq, and how often they collect specific kinds of waste

ma <- ma[, -c(14, 15, 72:100)]

# consolidate all the questions about bulky waste, school waste, etc to include in the trash tonnage

# first, include bulky waste tonnage in waste tonnage

ma$`Bulky waste tonnage`[is.na(ma$`Bulky waste tonnage`)] <- 0 # I looked and it is only cases where already counted
ma$TrashTonnage <- as.numeric(ma$`Trash Disposal Tonnage`) + as.numeric(ma$`Bulky waste tonnage`) # calc trash tonnage

ma <- ma[, c(1:49, 70, 50:69)] # changing the order for ease of viewing

# same process for schools

ma$`School trash tonnage (if separate)`[is.na(ma$`School trash tonnage (if separate)`)] <- 0 # I looked and it is only cases where already counted
ma$TrashTonnage <- ma$TrashTonnage + as.numeric(ma$`School trash tonnage (if separate)`)

# now to consolidate recycling

recyclingcols <- c(56:66, 69:70)
ma[, recyclingcols] <- apply(ma[, recyclingcols], 2, function(x) as.numeric(as.character(x)))
ma$RecyclingTonnage <- apply(ma[, recyclingcols], 1, function(x) sum(x, na.rm = TRUE))

# drop the extra trash and recycling columns now

ma <- ma[, -c(47:49, 53:54, 56:66, 69:70)] # kept food waste and yard waste which are NOT included in total

### Policy Questions --------------------

## now to start looking at policy stuff, and narrow down to what is actually needed

names(ma)

# HHServed with trash and recycling pickup are separate. 

names(ma)[4] <- "HHServedTrash"
names(ma)[5] <- "HHServedRecycling"

# Drop unneeded columns
names(ma)
ma <- ma[, -c(2, 8, 10, 12, 23:32, 34:38, 40, 42:44, 48:49)]

# recode tip fee

names(ma)[25] <- "TipFee"
ma$TipFee[is.na(ma$TipFee)] <- "No"
ma$TipFee[!is.na(ma$TipFee)] <- "Yes"


# rename some columns

names(ma)
names(ma)[2] <- "Households"
names(ma)[24] <- "SolidWasteTons"
names(ma)[28] <- "RecyclingTons"
names(ma)[6] <- "ProvideTrashCart"
names(ma)[8] <- "ProvRecycleBinorCart"
names(ma)[10] <- "MunicipalService"
names(ma)[11] <- "SchoolService"
names(ma)[12] <- "BusinessService"
names(ma)[14] <- "PropertyTax"
names(ma)[15] <- "AnnualFee"
names(ma)[16] <- "TransferStationAccessFee"
names(ma)[17] <- "Per-VisitFee"
names(ma)[18] <- "PAYT/SMART"

# add a state column and move it to the front

ma$state <- "MA"
ma <- ma[, c(29, 1:28)]


# standardize colnames with other states

names(ma)[6] <- "TrashServiceType"
names(ma)[8] <- "RecyclingServiceType"
names(ma)[10] <- "FoodServiceType"
names(ma)[20] <- "EnforceRecycling"
names(ma)[24] <- "ProvideCompostBins"
names(ma)[23] <- "SwapShop"

# Many columns code 'No' as NA, this should be fixed.

nacols <- names(ma)[c(7, 9, 15:24)]

for(i in nacols) {
  ma[, i][is.na(ma[, i])] <- "No"
  print("tick") # shows anywhere the loop broke
}

# Now to categorize yard service

Yard$YardWasteServiceType <- NA

Yard$YardWasteServiceType[Yard$`Weeks the drop-off center open to residents` != 0 & 
                             !is.na(Yard$`Weeks the drop-off center open to residents`)] <- "Dropoff"
Yard$YardWasteServiceType[Yard$`# of weeks collected curbside` != 0 & 
                             !is.na(Yard$`# of weeks collected curbside` != 0)] <- "Curbside"
Yard$YardWasteServiceType[(Yard$`Weeks the drop-off center open to residents` != 0 & 
                             Yard$`# of weeks collected curbside` == 0)] <- "None"
Yard$YardWasteServiceType[(!is.na(Yard$`Weeks the drop-off center open to residents`) & !is.na(Yard$`# of weeks collected curbside`)) &
                            (Yard$`Weeks the drop-off center open to residents` != 0 & Yard$`# of weeks collected curbside` != 0)] <- "Both"

ma <- merge(ma, Yard, by = "Municipality") # merge them together
ma <- ma[, -c(30, 31)] # drop the two extra yard waste cols

names(ma)[21] <- "RecyclingEnforcePersonnel"

# only keep those municipalities that are not accepting trash or recycling from outside the municipality; doing so would
# impact results

matest <- ma[ma$`Non-resident Trash and Recycling Service` != "Neither", ]
ma <- ma[ma$`Non-resident Trash and Recycling Service` == "Neither", ]


### Export --------------------

write.csv(ma, "~/Desktop/GitHub/Trash Data Project OOS/Data/CleanStateData/ma.csv")

