#### OOS Waste Data Project
## RI Municipal Data
## Dylan Radley

library(rio)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(pdftools)

rm(list = ls())

setwd("~/Desktop/GitHub/Trash Data Project OOS/Data/RI/")
# NOTE: If running this code on a different computer or with different file structure, adjust your working directory accordingly.

### Trash Numbers --------------------

# Write the function to scan through the table! 

scrable <- function(pdf_file,
                    pageoftable,
                    first_val, 
                    last_val,
                    number_columns,
                    column_names) {
  file <- pdf_text(pdf_file)
  file <- strsplit(file, "\n")
  page <- file[[pageoftable]] # look at a particular page
  page <- trimws(page) # get rid of extra spaces
  thetable <- page[grep(first_val, page): grep(last_val, page)]
  thetable <- str_split_fixed(thetable, " {2,}", number_columns) 
  dataframe <- data.frame(thetable)
  names(dataframe) <- column_names
  
  return(dataframe)
}

# Use it on RI data

RIdta <- scrable("RIMunicipalData.pdf",
                    1,
                    "Barrington",
                    "Woonsocket",
                   16,
        c("Municipality",
          "2020 Pop Projection",
          "HHServed",
          "Solid Waste (Tons)",
          "MRF Recycling",
          "Composted Material",
          "Scrap Metal",
          "Clothing",
          "Other Recycling",
          "Total Waste Generated (Tons)",
          "Tons of Trash Landfilled per HH",
          "MRF Recycling Rate",
          "Mandatory Recycling Rate",
          "Overall Diversion Rate",
          "Pounds Rejected Recycling per HH Served",
          "Rejected Recycling (Tons)"))

# Handcode some errors in the table produced by empty spaces

RIdta[4, c(8:16)] <- c(0, RIdta[4, 8:15]) # central falls
RIdta[12, c(8:16)] <- c(0, RIdta[12, 8:15]) # Foster
RIdta[15, c(8:16)] <- c(0, RIdta[15, 8:15]) # jamestown
RIdta[16, c(7:16)] <- c(0, 0, RIdta[16, 7:14]) # johnston
RIdta[17, c(7:16)] <- c(0, 0, RIdta[17, 7:14]) # lincoln
RIdta[28, c(7:16)] <- c(0, RIdta[28, 7:15]) # providence
RIdta[30, c(8:16)] <- c(0, RIdta[30, 8:15]) # scituate
RIdta[34, c(8:16)] <- c(0, RIdta[34, 8:15]) # warren
RIdta[35, c(8:16)] <- c(0, RIdta[35, 8:15]) # warwick

# get rid of the commas in the numbers and also make them numbers

RIdta.orig <- RIdta

numbercols <- c(2:16) # the cols that are numbers
RIdta[, numbercols] <- apply(RIdta[, numbercols], 2, function(x) gsub(",", "", x)) # no periods
RIdta[, numbercols] <- apply(RIdta[, numbercols], 2, function(x) gsub("%", "", x)) # no %'s
RIdta[, numbercols] <- apply(RIdta[, numbercols], 2, function(x) as.numeric(as.character(x))) # as numeric

# remove the numbers near some of the town names, which were superscripts for notes in the table

RIdta$Municipality <- gsub('[[:digit:]]+', '', RIdta$Municipality)


### Policy Data --------------------

policy <- import("RIMunicipalPolicies.xlsx")
policy.orig <- policy

# get a vector of all the col names, which are the second row. Then, make any needed tweaks and make them actual colnames

columns <- policy[2, ]

names(policy) <- columns
policy <- policy[-(1:2),] # get rid of the extra first two rows now

# get rid of the initial extra columns

names(policy)
policy <- policy[, -c(3:9, 12:20, 22:23, 25:33, 35:48, 51:52, 57:68, 70, 73:74, 77:78, 81:82, 83:93, 95:96, 98:101, 106:130, 
                      132:137, 139, 141:152, 154:161, 165:167, 170, 172:174, 178:203, 205:221)]

names(policy)

# rename some columns
names(policy)[5] <- "hhservCurbTrash"
names(policy)[7] <- "hhservCurbRecycling"
names(policy)[18] <- "hhservDropTrash"
names(policy)[20] <- "hhservDropRecycling"

# Consolidate hhserved, take the value served for Curb Recycling first, and if it is NA, then take dropoff recycling

policy$hhserved <- ifelse(is.na(policy$hhservCurbRecycling), policy$hhservDropRecycling, policy$hhservCurbRecycling)

# now drop the extra columns for hh served

policy <- policy[, -c(5, 7, 18, 20)]

# rename columns that talk about funding 

names(policy)[6] <- "CurbsideFee"
names(policy)[13] <- "DropoffFee"
names(policy)[26] <- "PAYT/SMART"

policy <- policy[, -c(3,7,14)] # drop extra columns

# recode some comuns that deal with different fee types

unique(policy$CurbsideFee)
policy$CurbsideFee[policy$CurbsideFee == "Yes, annual user fee"] <- "Yes"

unique(policy$DropoffFee)
policy$DropoffFee[policy$DropoffFee == "Yes, free of charge" | 
                    policy$DropoffFee == "Yes, permit issued once and does not expire"] <- "No"
policy$DropoffFee[policy$DropoffFee == "Yes, annual user fee" | policy$DropoffFee == "Yes, biennial user fee"] <- "Yes"

# consolidate the curbside and dropoff services

policy$TrashServiceType <- NA
unique(policy$`Is there drop-off for trash?`)
policy$TrashServiceType[policy$`Is there curbside trash pick up?` == "Yes" & 
                          grepl("At", policy$`Is there drop-off for trash?`)] <- "Both"
policy$TrashServiceType[policy$`Is there curbside trash pick up?` == "Yes" & 
                          grepl("No", policy$`Is there drop-off for trash?`)] <- "Curbside"
policy$TrashServiceType[policy$`Is there curbside trash pick up?` == "No" & 
                          grepl("At", policy$`Is there drop-off for trash?`)] <- "Drop-off"

policy$RecyclingServiceType <- NA
unique(policy$`Is there drop-off for recycling?`)
policy$RecyclingServiceType[policy$`Is there curbside recycling pick up?` == "Yes" & 
                              grepl("At", policy$`Is there drop-off for recycling?`)] <- "Both"
policy$RecyclingServiceType[policy$`Is there curbside recycling pick up?` == "Yes" & 
                              grepl("No", policy$`Is there drop-off for recycling?`)] <- "Curbside"
policy$RecyclingServiceType[policy$`Is there curbside recycling pick up?` == "No" & 
                              grepl("At", policy$`Is there drop-off for recycling?`)] <- "Drop-off"

# drop the old ones, and also collection type

policy <- policy[, -c(2, 3:4, 12:13)]

# get rid of extra rows

policy <- policy[-c(39:52), ]

# create variable for enforcement, and code it using the relavent variables 

unique(policy$`Materials enforced`)

policy$enforcement <- NA
policy$enforcement[policy$`Materials enforced` == "Both trash and recycling"] <- "Both"
policy$enforcement[policy$`Materials enforced` == "Mattresses only"] <- "No"
policy$enforcement[policy$`Materials enforced` == "Trash only"] <- "Trash"
policy$enforcement[policy$`Materials enforced` == "Recycling only"] <- "Recycling"
policy$enforcement[policy$`Is curbside enforcement conducted?` == "No"] <- "No"

policy <- policy[, -c(3:6)] # drop the extra columns

# cart provision, where no will refer to no recycling or trash carts

# trash
policy$ProvideTrashCart <- NA

unique(policy$`Does the municipality distribute carts for trash or recycling?`)
unique(policy$`For which materials?`)

policy$ProvideTrashCart[policy$`Does the municipality distribute carts for trash or recycling?` == "Yes" & 
                          policy$`For which materials?` == "Both trash and recycling"] <- "Yes"
policy$ProvideTrashCart[policy$`Does the municipality distribute carts for trash or recycling?` == "Yes" & 
                          policy$`For which materials?` == "Recycling only"] <- "No"
policy$ProvideTrashCart[policy$`Does the municipality distribute carts for trash or recycling?` == "No"] <- "No"

# recycling carts
policy$ProvRecycleBinorCart <- NA
policy$ProvRecycleBinorCart[(policy$`Does the municipality distribute carts for trash or recycling?` == "Yes" & 
                               policy$`For which materials?` == "Both trash and recycling") |
                               policy$`For which materials?` == "Recycling only" |
                               policy$`Are 22-gallon recycling bins distributed?` == "Yes"] <- "Yes"
policy$ProvRecycleBinorCart[policy$`Does the municipality distribute carts for trash or recycling?` == "No" &
                              policy$`Are 22-gallon recycling bins distributed?` == "No"] <- "Yes"

policy <- policy[, -c(9:11)] # drop the extra

# let's look at yard waste,composting, and food waste (we want to know compost bins and food scrap service)

names(policy)[9] <- "ProvideCompostBins"

# Food scrap service provision is written out, so must be actually read and interpreted. 
# Note: An important note, if they generally have compost service and they stopped cause of covid, I will still mark
# yes under the assumption that the habits built by the program may still affect residents.

compost <- c(0, NA, 0, 0, 0, 0, 0, 1, 0, NA, 0, NA, 0, 1, NA, 0, 0, 0, 1, 1, 3, 0, NA, 1, 0, 2, 0, NA, 0, 0, 0, 0, 0, NA, 0, 0, NA, 0)

policy$FoodServiceType <- compost
policy$FoodServiceType[policy$FoodServiceType == 0] <- "No"
policy$FoodServiceType[policy$FoodServiceType == 1] <- "Drop-off"
policy$FoodServiceType[policy$FoodServiceType == 2] <- "Curbside"
policy$FoodServiceType[policy$FoodServiceType == 3] <- "Both"

names(policy)
policy <- policy[, -c(6:8, 10, 21)] # get rid of extra

# now to deal with the notes about if the schools and businesses, etc are included. Will keep the info about what materials
# are included

names(policy)[9] <- "SchoolService"
names(policy)[12] <- "MunicipalService"
names(policy)[15] <- "BusinessService"

policy$SchoolService[policy$`Are schools included?` == "No"] <- "No"
policy$MunicipalService[policy$`Are municipal buildings included?` == "No"] <- "No"
policy$BusinessService[policy$`Are businesses included?` == "No"] <- "No"

policy <- policy[, -c(8, 10:11, 13:14, 16)]

# Change the swapshop col name and city, and also simplify it as well as the PAYT one

names(policy)[1] <- "Municipality"
names(policy)[3] <- "SwapShop"
unique(policy$SwapShop)
policy$SwapShop[grepl("Yes", policy$SwapShop)] <- "Yes"
policy$SwapShop[policy$SwapShop == "There is not a designated area but we do allow that for the day."] <- "No"

unique(policy$`PAYT/SMART`) # will include the partial ones
policy$`PAYT/SMART`[grepl("Yes", policy$`PAYT/SMART`)] <- "Yes"

### Combine Policy Info w/numbers! --------------------

# clean the variable that we will use to merge
RIdta$Municipality <- trimws(RIdta$Municipality)
policy$Municipality <- trimws(policy$Municipality)

ri <- merge(RIdta, policy, by = "Municipality") 

# hopkinton gets dropped but their data is mostly missing anyways

# add state col and move it to the front

ri$state <- "RI"

ri <- ri[, c(33, 1:32)]

# now to preemptively standardize

names(ri)[5] <- "SolidWasteTons"

# fix recycling by adding it all together

ri$RecyclingTons <- ri$`MRF Recycling` + ri$`Composted Material` + ri$Clothing + ri$`Other Recycling` + ri$`Scrap Metal`

# drop extra columns

ri <- ri[, -c(3, 6:10, 11:17)]

# drop enforcement because I do not think that it reflects 'mandatory recycling' because it is mostly checking for contamination

ri <- ri[, -17]

# Drop hhserved, which was calculated from the policy data, in favor of HHserved, which was with the waste tonnage.
ri <- ri[, -14]

# Export!

write.csv(ri, "~/Desktop/GitHub/Trash Data Project OOS/Data/CleanStateData/ri.csv")

