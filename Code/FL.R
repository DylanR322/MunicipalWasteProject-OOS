#### OOS Waste Data Project
## FL County Data
## Dylan Radley

library(rio)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(pdftools)

rm(list = ls())

setwd("~/Desktop/GitHub/Trash Data Project OOS/Data/FL/")
# NOTE: If running this code on a different computer or with different file structure, adjust your working directory accordingly.


### Trash Numbers --------------------

# Write a function to scan through the table! 

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

# Use the function on two of the Florida PDFs.

singlefam <- scrable("2020 Single-Family Participation in Recycling-1.pdf",
                     1,
                     "Miami-Dade",
                     "Liberty",
                     11,
                     c("County",
                       "Population1",
                       "Total SF Units in County",
                       "Residents per Unit2",
                       "Units With Service Available",
                       "% Total Units With Service Available3",
                       "Units Participating In Service",
                       "% of Units Participating In Service4",
                       "% Total Units Participating In County5",
                       "Population Participating In Service6",
                       "% Total Pop. Participating In Service7"))


multifam <- scrable("2020 Multi-Family Participation in Recycling.pdf",
                     1,
                     "Miami-Dade",
                     "Liberty",
                     11,
                     c("County",
                       "Population1",
                       "Total MF Units in County",
                       "Residents per Unit2",
                       "Units With Service Available",
                       "% Total Units With Service Available3",
                       "Units Participating In Service",
                       "% of Units Participating In Service4",
                       "% Total Units Participating In County5",
                       "Population Participating In Service6",
                       "% Total Pop. Participating In Service7"))


# Tweak the function for the final PDF, so that it can be read.

scrable2 <- function(pdf_file,
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
  thetable <- str_split_fixed(thetable, " {1,}", number_columns) # the change is here, in the number of spaces that make new columns
  dataframe <- data.frame(thetable)
  names(dataframe) <- column_names
  
  return(dataframe)
}

# run the function on the last FL data PDF!
wastedta <- scrable2("2020 Final Disposition of MSW_2.pdf",
                 1,
                 "Miami-Dade",
                 "Liberty",
                 12,
                 c("County",
                   "Population",
                   "SolidWasteTons",
                   "Certified MSW Tons Recycled",
                   "Non-Certified MSW Tons Recycled",
                   "RecyclingTons",
                   "Unadjusted Recycling Rate",
                   "Adjusted Recycling Rate",
                   "MSW Tons Landfilled",
                   "%",
                   "MSW Tons Combusted",
                   "%"))

# Five counties will have to be manually fixed

wastedta[4, 1] <- "Palm Beach"
wastedta[4, 2:10] <- wastedta[4, 3:11]
wastedta[4,11] <- "1,084,872"
wastedta[4,12] <- "33%"

wastedta[21, 1] <- "St. Lucie"
wastedta[21, 2:10] <- wastedta[21, 3:11]
wastedta[21,11] <- "0"
wastedta[21,12] <- "0%"

wastedta[24, 1] <- "St. Johns"
wastedta[24, 2:10] <- wastedta[24, 3:11]
wastedta[24,11] <- "0"
wastedta[24,12] <- "0%"

wastedta[29, 1] <- "Santa Rosa"
wastedta[29, 2:10] <- wastedta[29, 3:11]
wastedta[29,11] <- "0"
wastedta[29,12] <- "0%"

wastedta[32, 1] <- "Indian River"
wastedta[32, 2:10] <- wastedta[32, 3:11]
wastedta[32,11] <- "0"
wastedta[32,12] <- "0%"

# Drop the population column in the single and multifamily dataframes, then merge all three together!

multifam <- multifam[, -2]
singlefam <- singlefam[, -2]

waste <- merge(wastedta, singlefam, by = "County")
waste <- merge(waste, multifam, by = "County")

# Drop a bunch of the extra columns, such as the percentages, percent of population served, etc.

names(waste)
waste <- waste[, -c(4:5, 7:8, 10, 12, 14:16, 18, 19:21, 23, 24:25, 27:30 )]

# Recode the columns that are supposed to be numeric as such.

numbercols <- c(2:10) # the cols that are numbers
waste[, numbercols] <- apply(waste[, numbercols], 2, function(x) gsub(",", "", x)) # no commas
waste[, numbercols] <- apply(waste[, numbercols], 2, function(x) as.numeric(as.character(x))) # as numeric

# Calculate Solid Waste Tons as the combination of landfilled and combusted waste

waste$SolidWasteTons <- waste$`MSW Tons Landfilled` + waste$`MSW Tons Combusted`

waste <- waste[, -c(5:6)] # drop the landfill and combusted cols, which have been aggregated.

waste$Households <- waste$`Total SF Units in County` + waste$`Total MF Units in County` # combine hh

waste$HHServed <- waste$`Units Participating In Service.x` + waste$`Units Participating In Service.y` # combine hh served

waste <- waste[, -c(5:8)] # drop the extra hh columns

### Policy Data --------------------

policy <- import("HandEntryFLPolicy.csv", na.strings = "NA") # import the policy data

names(policy)

# drop the notes and extra notes columns

policy <- policy[, -c(3, 23)]

# combine with the waste data

fldta <- merge(waste, policy, by = "County")

### Census Data --------------------
# now to add the census data, which is separate because it is at the county level

setwd("~/Desktop/Trash Data Project OOS/Data/Census Data/")

census <- import("FLCountyData.csv")
census.orig <- census

# drop extra cols 
census <- census[, -c(1:51, 53:55)] # have no meaningful info
census <- census[, -c(3, 5, 7:14)] # extra cols for pop density and for number of households

# rename cols
names(census)
names(census)[1] <- "County"
names(census)[2] <- "Population"
names(census)[3] <- "PopulationDensity"
names(census)[4] <- "Households"
names(census)[5] <- "PopOver25"
names(census)[6] <- "LessHS"
names(census)[7] <- "HS"
names(census)[8] <- "BachelororMore"
names(census)[9] <- "MedIncome"
names(census)[10] <- "AvgIncome"
names(census)[11] <- "Gini"

# calculate the percentages for different educational attainments then drop extra cols for that

census$PctLessHS <- census$LessHS / census$PopOver25
census$PctHS <- census$HS / census$PopOver25
census$PctBachelororMore <- census$BachelororMore / census$PopOver25

census <- census[, -c(5:8)]

# merge census data with the county data, first by standardizing the identifier

census$County <- gsub(" County", "", census$County) 
census$County[census$County == "DeSoto"] <- "Desoto"

# drop the population column and the households columns from census because we want to calculate with the same internal FL data
census <- census[, -c(2,4)]

fldta <- merge(fldta, census, by = "County")

names(fldta)[7] <- "State"

# Remove all the counties for which I do not have any policy data

fldta <- fldta[!is.na(fldta$TrashServiceType), ]


# now to export it!
write.csv(fldta, "~/Desktop/GitHub/Trash Data Project OOS/Data/CleanStateData/fl.csv")







