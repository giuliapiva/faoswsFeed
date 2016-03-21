library(data.table)
library(faosws)
library(faoswsUtil)
library(countrycode)

if (CheckDebug()) {
  SetClientFiles("~/certificates/production")
  #SetClientFiles("~/certificates/qa")
  GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws", "c63d8cf2-3e70-409d-a05b-7800aadaea44")
}

raw_land <- fread("data-raw/IR_factor/landData.csv")
raw_land[, AreaCode := fs2m49(as.character(AreaCode))]


# Remove China aggregate and 5000 codes: 351, 5000, 5100, 5101, 5102, 5103,
# 5104, 5105, 5200, 5203, 5204, 5206, 5207, 5300, 5301, 5302, 5303, 5304, 5305,
# 5400, 5401, 5402, 5403, 5404, 5500, 5501, 5502, 5503, 5504, 5706, 5801, 5802,
# 5803, 5815, 5817
raw_land <- raw_land[!is.na(AreaCode),]

# Fill grid out to full
fullgrid <- raw_land[,CJ(AreaCode = unique(AreaCode), Year = unique(Year))]
unique_keys <- unique(raw_land[, .(AreaCode, ElementCode, ItemCode)])
unique_keys <- unique_keys[fullgrid, , on="AreaCode"]
land <- merge(raw_land, unique_keys, all.y=T, by=names(unique_keys))

#### Read in income classes ####
incomeClasses <- fread("data-raw/IR_factor/class.csv")
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

defunctCountries <- data.table(geographicAreaM49 = c("729", "890", "402", "810", "891", "274", "230", "200", "58"),
                               CountryName = c("Sudan", "Yugoslav SFR(-1991)", "West Bank(-1995)", 
                                               "USSR(-1991)", "Serbia-Monte(1992-2005)", "Gaza Strip(-1995)", 
                                               "Ethiopia PDR(-1992)", "Czechoslovak(-1992)", "Bel-Lux(-1999)"
                               ),
                               #                              GroupName = c("Middle income", "Middle income", "Middle income", 
                               #                                             "High income", "Middle income", "Middle income",
                               #                                             "Low income", "High income", "High income"))
                               GroupName = c("LMC", "UMC", "UMC", 
                                             "HIC", "UMC", "UMC",
                                             "LIC", "HIC", "HIC"))

#The following countries are still missing: Falkland Islands (Malvinas), French
#Guiana, Guadeloupe, Martinique, Montserrat, Niue, Norfolk Is, Pacific Is,
#Réunion, Saint Helena, Ascension and Tristan da Cunha, Westn Sahara, Channel
#Is, British Virgin Islands

incomeClasses <- rbind(incomeClasses, defunctCountries)

land <- merge(land, incomeClasses, all.x=TRUE, by.y="geographicAreaM49", by.x = "AreaCode")
land[,Year := as.character(Year)]

islandCountries <- data.table(read.csv("data-raw/IR_factor/FaostatAreaGroupLists.csv"))[Group.name == "Small Island Developing States"]

# Convert FAOSTAT codes to M49
setnames(islandCountries, "X", "AreaCode")
islandCountries[,AreaCode := fs2m49(as.character(AreaCode))]

#remove all countries in Small Island Developing States group 
#Note that the following countries aren't present in WB data: Aruba, Cook
#Islands, Guadeloupe, Martinique, Montserrat, Nauru, Niue, British Virgin
#Islands, Anguilla
training <- land[!islandCountries, on="AreaCode",]

trainingFullTS = training[, .SD[(all(!is.na(Value)))], by="AreaCode"]

# Make model  for each year
imp_model <- lm(Value~GroupName*Year, data=trainingFullTS)

land[,imputed := predict(imp_model, data.frame(GroupName, Year))]

imputeNA <- function(prod, imputed){
  if(all(is.na(prod))){
    out <- imputed
  } else {
    mu <- mean(prod[!is.na(prod)], na.rm=TRUE)
    mu_p <- mean(imputed[!is.na(prod)])
    
    mu_ratio <- mu/mu_p
    
    out <- prod
    out[is.na(out)] <- imputed[is.na(out)] * mu_ratio
  }
  out
  
}

# you can actually use Value straight, but the intermediate is for (commented out) plots
land[,new_Value := imputeNA(Value, imputed), by=AreaCode]

# ### plot with red rectangles for NAs

# library(ggplot2)
# plot_wdi <- copy(land)
# plot_wdi[,Year := as.numeric(Year)]
# 
# 
# pdf("imputation_land_plots.pdf")
# for(i in sort(unique(plot_wdi[,CountryName]))){
# gp <- ggplot(plot_wdi[CountryName == i], aes(x=Year, y=new_Value, group=CountryName))+
#   #facet_wrap(~Country.Name, scales="free_y")+
#   labs(title=i, y="Land Area")
# if(nrow(plot_wdi[is.na(Value) & CountryName == i])> 0){
#   gp <- gp + geom_rect(data=plot_wdi[is.na(Value) & CountryName == i],
#             aes(xmin = Year-.51, xmax=Year + .51, ymin=-Inf, ymax=Inf),
#             fill="indianred", colour = "#00000000")
# }
# print(gp + geom_line())
# }
# dev.off()
### IMPUTATION validation

# nl <- function(val, imp){
#   if(all(is.na(val))){
#     return(imp)
#   }
#   
#   nval <- imp * mean(val, na.rm=TRUE)/mean(imp, na.rm=T)
#   nval
#   
# }
# 
# land[, nv:= nl(Value, imputed), by = CountryName]
# 
# pdf("adj_land_imputation_validation.pdf")
# for(country in sort(unique(na.omit(land$CountryName)))){
#   g<-ggplot(land[!is.na(GroupName) & CountryName == country,], aes(x=as.numeric(Year), y=nv, group=GroupName))+
#     geom_line(col="red")+
#     geom_line(aes(y=Value, group=GroupName))+
#     scale_y_continuous(limits = c(0,NA))+
#     labs(title=country, y="Land Area")
#   print(g)
# }
# dev.off()


## FINAL PROCESSING

land[is.na(Value), Value := new_Value]
landData <- land

setnames(landData, c("AreaCode", "ElementCode", "ItemCode", "Year", "Value"), 
         c("geographicAreaM49","landElement", "itemLand", "timePointYears", "permanentMeadows"))
landData[,(setdiff(names(landData), c("geographicAreaM49","landElement", "itemLand", "timePointYears", "permanentMeadows"))) := NULL]
landData[, `:=`(landElement = as.character(landElement),
                itemLand = as.character(itemLand))]

devtools::use_data(landData, overwrite = TRUE)