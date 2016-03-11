library(data.table)
library(countrycode)
library(faosws)
library(faoswsUtil)

#' This script reads in agricultural productivity data and fills in missing values by imputing them based on regional averages.
#' INPUTS:
#' 
#' class.csv                  - WB income groups in which countries belong
#' FaostatAreaGroupLists.csv  - Used to get Small Island Developing States
#' wdi-productivity_6-12.csv  - World Bank agricultural worker productivity

# Read in token for code conversions
if (CheckDebug()) {
  SetClientFiles("~/certificates/production")
  #SetClientFiles("~/certificates/qa")
  GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws", "c63d8cf2-3e70-409d-a05b-7800aadaea44")
}

## Agricultural productivity data ##
#rawWdi = data.table(read.csv("data-raw/IR_factor/wdi-productivity_6-12.csv", stringsAsFactors = FALSE))
# updated File:
rawWdi = data.table(read.csv("data-raw/IR_factor/wdiLaborProductivityUpdated.csv", stringsAsFactors = FALSE))
#Remove useless metadata at bottom of csv by keeping rows with country names
rawWdi = rawWdi[Country.Name != "",]

# Correction for updated file
setnames(rawWdi, c("Indicator.Name", "Indicator.Code"), c("Series.Name", "Series.Code"))
#Make years all one column
wdi = melt(rawWdi, c("Country.Name", "Country.Code", "Series.Name", "Series.Code"), 
            variable.name = "timePointYears", value.name = "productivity")
#Remove 'y' prefix
#wdi[, timePointYears := sub("y", "", timePointYears)]
wdi[, timePointYears := sub("X", "", timePointYears)]
#Change names to M49
wdi[, geographicAreaM49 := as.character(countrycode(Country.Code, "wb", "iso3n"))]
#Remove any countries that aren't converted (probably Kosovo and Channel Islands)
wdi = wdi[!is.na(geographicAreaM49),]
# manually correct countrycode mismatch: China code M49 is 1248
wdi$geographicAreaM49[wdi$geographicAreaM49 == "156"] = "1248"

startYear = "1990" 
endYear = max(wdi[, timePointYears])
wdi = wdi[timePointYears %in% as.character(startYear:endYear)]
## Imputation of missing data in wdi
## !! PROBLEM !! - countries that don't exist anymore. Former Sudan? Former Soviet Countries?
## -- We're going to have to 
incomeClasses <- data.table(read.csv("data-raw/IR_factor/class.csv"))
# keep only income groups
#incomeClasses <- incomeClasses[GroupName %in% c("High income", "Low income", "Middle income"),]
incomeClasses <- incomeClasses[GroupCode %in% c("HIC", "LIC", "UMC", "LMC"),]

incomeClasses[,geographicAreaM49 := as.character(countrycode(CountryCode, "wb", "iso3n"))]
setnames(incomeClasses, "GroupCode", "GroupName")
incomeClasses <- incomeClasses[,.(geographicAreaM49, CountryName, GroupName)]

# Sudan has the wrong name (it should be former Sudan)
incomeClasses[geographicAreaM49 == "736", CountryName := "Sudan (former)"]
# China should be 1248
incomeClasses[geographicAreaM49 == "156", geographicAreaM49 := "1248"]
#Exclude Channel Islands and Kosovo (not separately recognised by the UN)
incomeClasses <- incomeClasses[!is.na(geographicAreaM49)]

#! Warning: Modern Sudan gets counted as former Sudan in WB data. If this
#shouldn't happen, then we need a mapping for WB country concepts to FAO ones

defunctCountries <- data.table(geographicAreaM49 = c("729", "890", "402", "810", "891", "274", "230", "200", "58"),
                               CountryName = c("Sudan", "Yugoslav SFR(-1991)", "West Bank(-1995)", 
                                               "USSR(-1991)", "Serbia-Monte(1992-2005)", "Gaza Strip(-1995)", 
                                               "Ethiopia PDR(-1992)", "Czechoslovak(-1992)", "Bel-Lux(-1999)"
                               ),
#                              GroupName = c("Middle income", "Middle income", "Middle income", 
#                                             "High income", "Middle income", "Middle income",
#                                             "Low income", "High income", "High income"))
                              GroupName = c("Lower middle income", "Upper middle income", "Upper middle income", 
                                             "High income", "Upper middle income", "Upper middle income",
                                             "Low income", "High income", "High income"))

incomeClasses <- rbind(incomeClasses, defunctCountries)

# Create training set for imputation
wdi_withClasses <- merge(incomeClasses[,.(geographicAreaM49, GroupName)], wdi, by="geographicAreaM49", all.y=TRUE)

## Exclude Small island countries from training set

islandCountries <- data.table(read.csv("data-raw/IR_factor/FaostatAreaGroupLists.csv"))[Group.name == "Small Island Developing States"]

# Convert FAOSTAT codes to M49
setnames(islandCountries, "X", "geographicAreaM49")
islandCountries[,geographicAreaM49 := fs2m49(as.character(geographicAreaM49))]

#remove all countries in Small Island Developing States group # Note that the
#following countries aren't present in WB data: 
#Aruba, Cook Islands, Guadeloupe, Martinique, Montserrat, Nauru, Niue, 
#British Virgin Islands, Anguilla
training <- wdi_withClasses[!islandCountries, on="geographicAreaM49",]

trainingFullTS = training[, .SD[(all(!is.na(productivity)))], by="geographicAreaM49"]

# Make model  for each year
imp_model <- lm(productivity~GroupName*timePointYears, data=trainingFullTS)

wdi_withClasses[,imputed := predict(imp_model, data.frame(GroupName, timePointYears))]

imputeNA <- function(prod, imputed){
  if(all(is.na(prod))){
    out <- imputed
  } else {
    mu <- mean(prod, na.rm=TRUE)
    mu_p <- mean(imputed)
    
    mu_ratio <- mu/mu_p
    
    out <- prod
    out[is.na(out)] <- imputed[is.na(out)] * mu_ratio
  }
  out
  
}

# you can actually use productivity straight, but the intermediate is for (commented out) plots
wdi_withClasses[,new_productivity := imputeNA(productivity, imputed), by=geographicAreaM49]

# ### plot with red rectangles for NAs
# 
library(ggplot2)
plot_wdi <- copy(wdi_withClasses)
plot_wdi[,timePointYears := as.numeric(timePointYears)]


pdf("imputation_trial_plots.pdf")
for(i in sort(unique(plot_wdi[,Country.Name]))){
gp <- ggplot(plot_wdi[Country.Name == i], aes(x=timePointYears, y=new_productivity, group=Country.Name))+
  #facet_wrap(~Country.Name, scales="free_y")+
  labs(title=i, y="Agricultural Productivity (2005 dollars)")
if(nrow(plot_wdi[is.na(productivity) & Country.Name == i])> 0){
  gp <- gp + geom_rect(data=plot_wdi[is.na(productivity) & Country.Name == i], 
            aes(xmin = timePointYears-.51, xmax=timePointYears + .51, ymin=-Inf, ymax=Inf), 
            fill="indianred", colour = "#00000000")
}
print(gp + geom_line())
}
dev.off()
## FINAL PROCESSING

wdi_withClasses[is.na(productivity), productivity := new_productivity]
wdi <- wdi_withClasses
#keep only select columns
wdi = wdi[,.(geographicAreaM49, timePointYears, productivity)]
setkey(wdi, geographicAreaM49, timePointYears)

devtools::use_data(wdi, overwrite = TRUE)

