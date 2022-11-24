#### OOS Waste Data Project
## Analysis
## Dylan Radley

library(rio)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(pdftools)
library(lmtest)
library(car)
library(stargazer)
library(gridExtra)
library(reactable)
library(RColorBrewer)
library(gghighlight)

rm(list = ls())

options(scipen = 50) #prevent scientific notation

`%notin%` <- Negate(`%in%`)

setwd("~/Desktop/GitHub/MunicipalWasteProject-OOS//Files/")
# NOTE: If running this code on a different computer or with different file structure, adjust your working directory accordingly.

### Creating Variables --------------------

data <- import("dta.csv")
data <- data[, -1] # remove V1

# now to calculate total diverted waste
divertedcols <- c(5, 6, 9) # Only yard waste categorized as diverted will be counted
data$DivertedWaste <- apply(data[, divertedcols], 1, function(x) sum (x, na.rm = TRUE)) # sum them up!
data$DivertedWaste[is.na(data$RecyclingTons) & is.na(data$FoodWaste) & is.na(data$DivertedYardWaste)] <- NA

disposedcols <- c(4, 7)
data$DisposedWaste <- apply(data[, disposedcols], 1, function(x) sum (x, na.rm = TRUE))

# 0 trash will be interpreted as not collecting any, not as there being literally zero tons of trash produced
data$DisposedWaste[data$DisposedWaste == 0] <- NA 

data$divrate <- data$DivertedWaste / (data$DisposedWaste + data$DivertedWaste)

# if there is no recycling or solid waste data, code the divrate as missing
data$divrate[is.na(data$RecyclingTons) | is.na(data$SolidWasteTons)] <- NA 

data$divrate <- data$divrate*100 # convert to percent for ease of interpretation


data$wasteperhh <- data$DisposedWaste / data$HHServedTrash
data$wasteperhh[data$HHServedTrash == 0] <- NA
data$wasteperhh[is.na(data$SolidWasteTons)] <- NA # Code as missing if there is no solid waste data

# Get rid of Tip Fees because they are not usually paid by residents but by companies. 

data <- data[, -30]

# Code rate type such that 1 = usage-based fee, -1 = flat fee, and 0 = both.
data$RateType <- NA
data$RateType[data$TransferStationAccessFee == 1 | data$`Per-VisitFee` == 1 | 
                data$`PAYT/SMART` == 1] <- 1
data$RateType[data$AnnualFee == 1 | data$PropertyTax == 1 | data$PerHouseCharge == 1 | data$DropoffFee == 1 | 
                data$CurbsideFee == 1] <- -1
data$RateType[(data$TransferStationAccessFee == 1 | data$`Per-VisitFee` == 1 | data$`PAYT/SMART` == 1) & 
                (data$AnnualFee == 1 | data$PropertyTax == 1 | data$DropoffFee == 1 | data$CurbsideFee == 1 | 
                   data$PerHouseCharge == 1)] <- 0

### Handle Outliers --------------------

# Find the quartiles and IQR of the diversion rate
summary(data$divrate)
quartilesdiv <- quantile(data$divrate, probs = c(0.25, 0.75), na.rm = TRUE)
iqrdiv <- IQR(data$divrate, na.rm = TRUE)

# define the boundaries for potential outliers in diversion rate. 
upper <- quartilesdiv[2] + 1.5*iqrdiv
extupper <- quartilesdiv[2] + 3*iqrdiv
lower <- quartilesdiv[1] - 1.5*iqrdiv # you can't have a negative diversion rate, so 0 will be our boundary.

# subset the data to include these outliers
outliersdiv <- data[(data$divrate > upper | data$divrate == 0) & !is.na(data$divrate), ]

# Remove the  towns with diversion above 95% because they are extreme outliers, remove Rutherfordton and Pilot Mountain
# b/c unusual amount of Yard Waste. Remove Spencer Mountain because it is empty

data <- data[data$GovUnit != "Williamstown" & data$GovUnit != "Adams" & data$GovUnit != "East Longmeadow" & 
               data$GovUnit != "RUTHERFORDTON" & data$GovUnit != "PILOT MOUNTAIN" & data$GovUnit != "SPENCER MOUNTAIN", ]

# Find the quartiles and IQR of the diversion rate for waste per household, then follow the same process as diversion
summary(data$wasteperhh)

quartileswph <- quantile(data$wasteperhh, probs = c(0.25, 0.75), na.rm = TRUE)
iqrwph <- IQR(data$wasteperhh, na.rm = TRUE)
upperwph <- quartileswph[2] + 1.5*iqrwph
extupperwph <- quartileswph[2] + 3*iqrwph
lowerwph <- quartileswph[1] - 1.5*iqrwph

outlierswph <- data[(data$wasteperhh > upperwph | data$wasteperhh < lowerwph) & !is.na(data$wasteperhh), ]

# Since 12/17 Florida Counties in the data are outliers, we will do this process again having removed Florida.

datanofl <- data[data$State != "FL", ]


quartileswph <- quantile(datanofl$wasteperhh, probs = c(0.25, 0.75), na.rm = TRUE)
iqrwph <- IQR(datanofl$wasteperhh, na.rm = TRUE)
upperwph <- quartileswph[2] + 1.5*iqrwph
extupperwph <- quartileswph[2] + 3*iqrwph
lowerwph <- quartileswph[1] - 1.5*iqrwph
outlierswph <- datanofl[(datanofl$wasteperhh > upperwph | datanofl$wasteperhh < lowerwph) & !is.na(datanofl$wasteperhh), ]


# Remove Robbins, Pembroke, and Manteo, which all seem to be particularly extreme outliers

datanofl <- datanofl[datanofl$GovUnit != "ROBBINS" & datanofl$GovUnit != "PEMBROKE" & 
                       datanofl$GovUnit !=  "MANTEO", ]

# Remove Nags head and Oak Island, which much larger hhserved than hh, a significant discrepancy for this calculation

datanofl$wasteperhh[datanofl$GovUnit == "NAGS HEAD"] <- NA
datanofl$wasteperhh[datanofl$GovUnit == "OAK ISLAND"] <- NA


### Fitting Models --------------------

# First, a model of all of the variables of interest
fulltest <- lm(divrate ~ TrashServiceType + RecyclingServiceType + FoodService + ProvRecycleBinorCart + 
                 EnforceRecycling + ProvideCompostBins + RecyclingEnforcePersonnel + SwapShop + RateType + PopulationDensity +
                 MedIncome + PctBachelororMore, data)
summary(fulltest)

# now to find the Variance Inflation Factor of every variable. This will help identify multicollinearity, and what 
# variables are causing it. Values higher than 4 will be considered concerning in this case. 
vif(fulltest)
# Get a closer look at how variables are correlated with each other. 
cor(data[, c(14, 15, 17, 19, 20, 21, 22, 24, 34, 35, 40, 45 )], method = "pearson", use = "complete.obs")

# Will Recycling Service Type, which is highly correlated with and has fewer observations than Trash Service Type. 

# Fit another model with trash and recycling removed
fulltest2 <- lm(divrate ~ TrashServiceType + FoodService + ProvRecycleBinorCart + EnforceRecycling + ProvideCompostBins + 
                  RecyclingEnforcePersonnel + SwapShop + RateType + PopulationDensity + MedIncome + PctBachelororMore, data)
summary(fulltest2)
vif(fulltest2)
cor(data[, c(14, 17, 19, 20, 21, 22, 24, 34, 35, 40, 45 )], method = "pearson", use = "complete.obs")

# The main issue now is the model removes 620 municipalities because it can only use complete data. Thus, run through
# each variable, see how many cities there is data for it, and then remove those with fewer than 300 cities with data.
sum(table(data$divrate))
sum(table(data$TrashServiceType))
sum(table(data$FoodService))
sum(table(data$ProvRecycleBinorCart))
sum(table(data$EnforceRecycling))
sum(table(data$ProvideCompostBins))
sum(table(data$RecyclingEnforcePersonnel))
sum(table(data$SwapShop))
sum(table(data$RateType))

completetest <- lm(divrate ~ TrashServiceType + FoodService + ProvideCompostBins + RecyclingEnforcePersonnel + 
                     RateType + log(PopulationDensity) + MedIncome + PctBachelororMore, data)
summary(completetest)
vif(completetest)
cor(data[, c(14, 17, 20, 22, 34, 35, 40, 45 )], method = "pearson", use = "complete.obs")

# This model does not seem to have multicollinearity that is too concerning to leave, so let's run other tests. 
plot(completetest) # A series of plots that check the model for statistical validity
bptest(completetest) # fails, thus we will have to keep in mind that heteroskedascity is present 

# Now to follow pretty much the same process of testing for Waste Per Household. 

fulltestwph <- lm(wasteperhh ~ TrashServiceType + RecyclingServiceType + FoodService + ProvRecycleBinorCart + 
                 EnforceRecycling + ProvideCompostBins + RecyclingEnforcePersonnel + SwapShop + RateType + PopulationDensity +
                 MedIncome + PctBachelororMore, datanofl)
summary(fulltestwph)
vif(fulltestwph)
cor(datanofl[, c(14, 15, 17, 19, 20, 21, 22, 24, 34, 35, 40, 45 )], method = "pearson", use = "complete.obs")

# Again remove Recycling Service type, for the same reasons

fulltestwph2 <- lm(wasteperhh ~ TrashServiceType + FoodService + ProvRecycleBinorCart + EnforceRecycling + ProvideCompostBins + 
                  RecyclingEnforcePersonnel + SwapShop + RateType + PopulationDensity + MedIncome + PctBachelororMore, datanofl)
summary(fulltestwph2)
vif(fulltestwph2)
cor(datanofl[, c(14, 17, 19, 20, 21, 22, 24, 34, 35, 40, 45 )], method = "pearson", use = "complete.obs")

sum(table(datanofl$wasteperhh))
sum(table(datanofl$TrashServiceType))
sum(table(datanofl$FoodService))
sum(table(datanofl$ProvRecycleBinorCart))
sum(table(datanofl$EnforceRecycling))
sum(table(datanofl$ProvideCompostBins))
sum(table(datanofl$RecyclingEnforcePersonnel))
sum(table(datanofl$SwapShop))
sum(table(datanofl$RateType))

completewph <- lm(wasteperhh ~ TrashServiceType + FoodService + ProvideCompostBins + RecyclingEnforcePersonnel + 
                     RateType + PopulationDensity + MedIncome + PctBachelororMore, datanofl)
summary(completewph)
vif(completewph)
cor(datanofl[, c(14, 17, 20, 22, 34, 35, 40, 45 )], method = "pearson", use = "complete.obs")
plot(completewph)
bptest(completewph) # passes

# now to take some looks at the variables that did not make it, namely providing a bin or cart, enforcing recycling, and
# swapshops

provcartdiv <- lm(divrate ~ ProvRecycleBinorCart + PopulationDensity + MedIncome + PctBachelororMore, data)
summary(provcartdiv)
plot(provcartdiv)
vif(provcartdiv)
bptest(provcartdiv) # passes

enforcediv <- lm(divrate ~ EnforceRecycling + PopulationDensity + MedIncome + PctBachelororMore, data)
summary(enforcediv)
plot(enforcediv)
vif(enforcediv)
bptest(enforcediv) # fails

swapdiv <- lm(divrate ~ SwapShop + PopulationDensity + MedIncome + PctBachelororMore, data)
summary(swapdiv)
plot(swapdiv)
vif(swapdiv)
bptest(swapdiv) # passes

provcartwph <- lm(wasteperhh ~ ProvRecycleBinorCart + PopulationDensity + MedIncome + PctBachelororMore, datanofl)
summary(provcartwph)
plot(provcartwph)
vif(provcartwph)
bptest(provcartwph) # passes

enforcewph <- lm(wasteperhh ~ EnforceRecycling + PopulationDensity + MedIncome + PctBachelororMore, datanofl)
summary(enforcewph)
plot(enforcewph)
vif(enforcewph)
bptest(enforcewph) # passes

swapwph <- lm(wasteperhh ~ SwapShop + PopulationDensity + MedIncome + PctBachelororMore, datanofl)
summary(swapwph)
plot(swapwph)
vif(swapwph)
bptest(swapwph) # passes

### Analysis --------------------

# change median income to be by 10k changes

data$MedIncome10k <- data$MedIncome / 10000

# diversion rate model

divfull <- lm(divrate ~ TrashServiceType + FoodService + ProvideCompostBins + RecyclingEnforcePersonnel + 
                     RateType + log(PopulationDensity) + MedIncome10k + PctBachelororMore, data)
summary(divfull)

# calculate the effect sizes

divfulleffects <- lm(scale(divrate) ~ TrashServiceType + FoodService + ProvideCompostBins + RecyclingEnforcePersonnel + 
                       RateType + log(PopulationDensity) + MedIncome10k + PctBachelororMore, data)
summary(divfulleffects)

# Produce a table of this regression!

stargazer(divfull, divfulleffects, type = "html", 
          dep.var.labels = c("Diversion Rate", "Effect Sizes"),
          covariate.labels = c("Trash Pickup: Curbside, Both, or Dropoff", "Offer Composting", "Provide Compost Bins", 
                               "Recycling Enforcement Personnel", "Fees Based on Usage", "Population Density (logged)",
                               "Median Household Income (10k)", "% Bachelor's Degree or Higher"), 
          out = "divfull.html", report = ('vc*p'))

# waste per household

datanofl$MedIncome10k <- datanofl$MedIncome / 10000

wphfull <- lm(wasteperhh ~ TrashServiceType + FoodService + ProvideCompostBins + RecyclingEnforcePersonnel + 
                RateType + log(PopulationDensity) + MedIncome10k + PctBachelororMore, datanofl)
summary(wphfull)

# find effect sizes of this model

wphfulleffects <- lm(scale(wasteperhh) ~ TrashServiceType + FoodService + ProvideCompostBins + RecyclingEnforcePersonnel + 
                       RateType + log(PopulationDensity) + MedIncome10k + PctBachelororMore, datanofl)
summary(wphfulleffects)

# produce a table of this model
stargazer(wphfull, wphfulleffects, type = "html", 
          dep.var.labels = c("Waste Per Household", "Effect Sizes"),
          covariate.labels = c("Trash Pickup: Curbside, Both, or Dropoff", "Offer Composting", "Provide Compost Bins", 
                               "Recycling Enforcement Personnel", "Fees Based on Usage", "Population Density (logged)",
                               "Median Household Income (10k)", "% Bachelor's Degree or Higher"), 
          out = "wphfull.html", report = ('vc*p'))

# Also make a variable of pounds per person per day

data$wasteperhhppd <- data$wasteperhh * 2000 / 365
datanofl$wasteperhhppd <- datanofl$wasteperhh * 2000 / 365


# Models of just the controls

controlsdiv <- lm(divrate ~ log(PopulationDensity) + MedIncome10k + PctBachelororMore, data)
summary(controlsdiv)

controlswph <- lm(wasteperhh ~ log(PopulationDensity) + MedIncome10k + PctBachelororMore, data)
summary(controlswph)


### Figures --------------------

# First, the table that summarizes the regression results for diversion rate

baseline <- c("Fees: Both", "Fees: Both",
              "Current % w/ a Bachelor's Degree or higher")
change <- c("Flat Fee Only", "PAYT or Access Fees Only", "10%-point Increase")
effectdiv <- c(-5.3, 5.3, 3) 
effectwpht <- c(0.13, -0.13, -0.06)
effectwphppd <- effectwpht * 2000 / 365

regtbl <- cbind.data.frame(baseline, change, effectdiv, effectwpht, effectwphppd)

# now to make a color scale!

GrnWhiRedP <- function(x) rgb(colorRamp(c("#fc032c", "#ffffff", "#42f548"))(x), maxColorValue = 255)
GrnWhiRedN <- function(x) rgb(colorRamp(c("#42f548", "#ffffff", "#fc032c"))(x), maxColorValue = 255)


# now to make the actual table

reactable(regtbl,
          columns = list(
            baseline = colDef(name = "Baseline Policy"), 
            change = colDef(name = "Change"), 
            effectdiv = colDef(style = function(value) {
              if (!is.numeric(value)) return()
              normalized <- (value - -10) / (10 - -10)
              color <- GrnWhiRedP(normalized)
              list(background = color)
            }, format = colFormat(digits = 2), name = "Effect on Diversion Rate (in %-points)"),
            effectwpht = colDef(style = function(value) {
              if (!is.numeric(value)) return()
              normalized <- (value - -0.3) / (0.3 - -0.3) 
              color <- GrnWhiRedN(normalized)
              list(background = color)
            }, format = colFormat(digits = 2), name = "Effect on Waste Per Household (Tons/year)"),
            effectwphppd = colDef(style = function(value) {
              if (!is.numeric(value)) return()
              normalized <- (value - -1.5) / (1.5 - -1.5) 
              color <- GrnWhiRedN(normalized)
              list(background = color)
            }, format = colFormat(digits = 2), name = "Effect on Waste Per Household (Lbs per Household/day)")),
          compact = TRUE, pagination = FALSE, bordered = TRUE)

reactable(regtbl,
          columns = list(
            baseline = colDef(name = "Baseline Policy"), 
            change = colDef(name = "Change"),
            effectdiv = colDef(name = "Effect on Diversion Rate (in %-Points)"),
            effectwpht = colDef(name = "Effect on Diversion Rate (in %-Points)"),
            effectwphppd = colDef(name = "Effect on Diversion Rate (in %-Points)")),
          compact = TRUE, pagination = FALSE, bordered = TRUE)

#I'll export this table to put it in the report by hitting export on the viewer

## Summary Statistics

# municipalities in each state in the dataset

statefreq <- cbind.data.frame(table(data$State))
total <- sum(statefreq$Freq)
statefreq[5, 2] <- total
statefreq$Var1 <- as.character(statefreq$Var1)
statefreq$Var1[5] <- "Total"

reactable(statefreq,
          columns = list(
            Var1 = colDef(name = "State"),
            Freq = colDef(name = "Frequency")), compact = TRUE, bordered = TRUE, pagination = FALSE)

# For each variable, the number with data

NforVars <- c(sum(table(data$ProvideCompostBins)), sum(table(data$FoodService)), sum(table(data$RecyclingEnforcePersonnel)),
              sum(table(data$TrashServiceType)), sum(table(data$RateType)), sum(table(data$ProvRecycleBinorCart)),
              sum(table(data$SwapShop)), sum(table(data$EnforceRecycling)), sum(table(data$divrate)), sum(table(datanofl$wasteperhh)))
VarsNames <- c("Provide Compost Bins?", "Provide Food Waste Management?", "Have Recycling Enforecment Personnel?",
               "Trash Service type offered?", "Type of Fees?", "Provide Recycle Bins or Carts?",
               "Have a swap shop?", "Have a Mandatory Recycling Ordinance?", "Diversion Rate", "Waste per Household")
Varstbl <- cbind.data.frame(VarsNames, NforVars)

reactable(Varstbl,
          columns = list(
            VarsNames = colDef(name = "Variable/Question"),
            NforVars = colDef(name = "Number of Municipalities with Data")), compact = TRUE, bordered = TRUE, pagination = FALSE)              

# Descriptive statistics of the important variables

data$id <- str_c(data$State, "---", data$GovUnit)
datanofl$id <- str_c(datanofl$State, "---", datanofl$GovUnit)

continvars1 <- data[, c(40, 43, 48)]
continvars2 <- datanofl[, c(44, 47, 48)]

continvars <- merge(continvars1, continvars2, by = "id")

stargazer(continvars, type = "text", summary.stat = c("n", "mean", "median", "p25", "p75", "sd", "min", "max"))


stargazer(continvars, type='html', summary.stat = c("mean", "median", "p25", "p75", "sd", "min", "max"),
          title='Descriptive Statistics for Significant Dependent & Independent Variables', align = TRUE,
          covariate.labels = c("Percent with a Bachelor's Degree or Higher", 
                               "Diversion Rate",
                               "Waste per Household (Tons/Year)",
                               "Waste per Household (Lbs/Day)"), digits = 1, out = "descstats.html")

# make a table for the rate structures used

Rates <- cbind.data.frame(table(data$RateType))
Rates[, 1] <- c("Flat Fee Only", "Flat Fee & Usage-Based Fee", "Usage-Based Fee Only")

reactable(Rates,
          columns = list(
            Var1 = colDef(name = "Fee Type"),
            Freq = colDef(name = "Number of Municipalities")), compact = TRUE, bordered = TRUE, pagination = FALSE)              

# That's all!

