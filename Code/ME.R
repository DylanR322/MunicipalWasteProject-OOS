#### OOS Waste Data Project
## ME Municipal Data
## Dylan Radley

library(rio)
library(dplyr)
library(tidyr)
library(stringr)

rm(list = ls())

setwd("~/Desktop/GitHub/MunicipalWasteProject-OOS/Data/ME/")
# NOTE: If running this code on a different computer or with different file structure, adjust your working directory accordingly.
# NOTE: The Maine data is not included in the final analysis. This script is included to allow the other scripts to run, and also
# as a resource if the Maine data is ever updated. 

# I'll be looking at ecomaine's community comparison data, this is going to be originally written for FY 2019 data, but can be updated
# later for FY 2021 data.

me <- import("FY20 Community Comparisons.csv")

me$state <- "ME"
me$state[grepl("(NH)", me$Municipality)] <- "NH"

names(me)

# drop extra columns

me <- me[, -c(2:3, 7:8, 18)]

# now to change the way that the trash and recycling services are coded. yes will = curbside, and no will = drop-off,

names(me)[5] <- "RecyclingServiceType"
names(me)[6] <- "TrashServiceType"

me$TrashServiceType[me$TrashServiceType == "Yes"] <- "Curbside"
me$TrashServiceType[me$TrashServiceType == "No"] <- "Drop-off"

me$RecyclingServiceType[me$RecyclingServiceType == "Yes"] <- "Curbside"
me$RecyclingServiceType[me$RecyclingServiceType == "No"] <- "Drop-off"

# other changes to align with the other datasets

names(me)
names(me)[3] <- "SolidWasteTons"
names(me)[4] <- "RecyclingTons"
names(me)[7] <- "EnforceRecycling"
names(me)[8] <- "PAYT/SMART"
names(me)[8] <- "YardWasteServiceType"
names(me)[10] <- "FoodServiceType" 
names(me)[13] <- "SwapShop"

# Change the cols with numbers to numeric and remove the commas

numbercols <- c(2:4) # the cols that are numbers
me[, numbercols] <- apply(me[, numbercols], 2, function(x) gsub(",", "", x)) # no commas
me[, numbercols] <- apply(me[, numbercols], 2, function(x) as.numeric(as.character(x))) # as numeric


# now to export it!
write.csv(me, "~/Desktop/GitHub/Trash Data Project OOS/Data/CleanStateData/me.csv")




