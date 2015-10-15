#### The Food and Agriculture Organization of the United Nations (FAO)
#### This script has been created on 05/02/2014 by BD bernhard.dalheiemr@fao.org
##
##       ANIMAL UNIT INDICES FOR ENERGY AND PROTEIN  
##
##                    V E R S I O N   6
##
##                Last revised on 08/10/2014
##
## MAIN CHANGES
##  (a) Calculations and data input are carried out through functions
##  (b) Energy and Protein Requirements for cattle are calculated by differentiating between dairy and 
##      beef cattle 
##  (c) Egg production is included in chicken energy requirement calculation
##  (d) Any updates in the database (sws) are now incorporated anytime the script is run
##  (E) CAMELS AND BUFFALOES ARE INCLUDED 
## 
##SUMMARY
## Within the framework of estimating feed use in FBS animal unit indices (AUI) based on energy and protein 
## requirements are required. This script provides AUI for the target period (1990:2011) and target species
## using functions designed for each species and requirement. Therefore the 16 functions, the sws_query_2
## function which retrieves data form the sws as well as the ojdbc14.jar file have to be located in the 
## woking directory.
## 
## Required packages (in functions): RJDBC, stringr, reshape2, rJava, plyr, DBI
##
##  INDEX
##  1. Source functions
##  2. Compile indices for species
##    2.1 Cattle
##    2.2 Buffalo
##    2.3 Sheep
##    2.4 Goats
##    2.5 Camel
##    2.6 Pigs
##    3.7 Chicken
##    4.8 Ducks
##    5.9 Geese
##    6.10 Turkeys
##  3. Combine all indices in one dataframe 
##  4. Prepare output csv 
##
## START

#setwd("T:/Onno/Feed-Model/Expert-Approach/Programming/Programs/functions")

## 1. SOURCE FUNCTIONS

source('functions/cattle_energy_factor.r')
source('functions/cattle_protein_factor.r')
source('functions/sheep_energy_factor.r')
source('functions/sheep_protein_factor.r')
source('functions/goat_energy_factor.r')
source('functions/goat_protein_factor.r')
source('functions/pig_energy_factor.r')
source('functions/pig_protein_factor.r')
source('functions/chicken_energy_factor.r')
source('functions/chicken_protein_factor.r')
source('functions/duck_energy_factor.r')
source('functions/duck_protein_factor.r')
source('functions/goose_energy_factor.r')
source('functions/goose_protein_factor.r')
source('functions/turkey_energy_factor.r')
source('functions/turkey_protein_factor.r')
source('functions/buffalo_energy_factor.r')
source('functions/buffalo_protein_factor.r')
source('functions/camel_energy_factor.r')
source('functions/camel_protein_factor.r')

## 2. COMPILE INDICEs

## 2.1 Cattle

# energy
ce <- cattle_energy_factor(1:299, 1990:2011) 
ce$item <- rep(866,nrow(ce)) 
ce <- ce[, c("area", "year", "item", "energy")]

# protein
cp <- cattle_protein_factor(1:299, 1990:2011) 
cattle <- merge(ce, cp, all=T)


## 2.2 Buffaloes

#energy
be <- buffalo_energy_factor(1:299, 1990:2012)
be$item <- rep(946, nrow(be))
be <- be[,c("area", "year", "item", "energy")]

#protein
bp <- buffalo_protein_factor(1:299, 1990:2012)
buffalo <- merge(be, bp, all=T)

## 2.3 Sheep

#energy
se <- sheep_energy_factor(1:299, 1990:2011)
se$item <- rep(976,nrow(se)) 
se <- se[, c("area", "year", "item", "energy")]

#protein
sp <- sheep_protein_factor(1:299, 1990:2011) 
sheep <- merge(se, sp, all=T)



## 2.4 Goats

#energy
ge <- goat_energy_factor(1:299, 1990:2011)
ge$item <- rep(1016,nrow(ge)) 
ge <- ge[, c("area", "year", "item", "energy")]

#protein
gp <- goat_protein_factor(1:299, 1990:2011) 
goat <- merge(ge, gp, all=T)

## 2.5 Camels

#energy
cae <- camel_energy_factor(1:299, 1990:2011)
cae$item <- rep(1126, nrow(cae))
cae <- cae[, c("area", "year", "item", "energy")]


#protein
cap <- camel_protein_factor(1:299, 1990:2011)
camel <- merge(cae, cap, all=T)

## 2.6 Pigs

#energy
pe <- pig_energy_factor(1:299, 1990:2011)
pe$item <- rep(1034,nrow(pe)) 
pe <- pe[, c("area", "year", "item", "energy")]

#protein
pp <- pig_protein_factor(1:299, 1990:2011) 
pig <- merge(pe, pp, all=T)

## 2.7 Chickens

#energy
che <- chicken_energy_factor(1:299, 1990:2011)
che$item <- rep(1057,nrow(che)) 
che <- che[, c("area", "year", "item", "energy")]

#protein
chp <- chicken_protein_factor(1:299, 1990:2011) 
chicken <- merge(che, chp, all=T)


## 2.8 Ducks

#energy
de <- duck_energy_factor(1:299, 1990:2011)
de$item <- rep(1068,nrow(de)) 
de <- de[, c("area", "year", "item", "energy")]

#protein
dp <- duck_protein_factor(1:299, 1990:2011) 
duck <- merge(de, dp, all=T)

## 2.9 Geese

#energy
gee <- goose_energy_factor(1:299, 1990:2011)
gee$item <- rep(1072,nrow(goo)) 
goo <- goo[, c("area", "year", "item", "energy")]

#protein
gep <- goose_protein_factor(1:299, 1990:2011) 
goose <- merge(goo, gep, all=T)

## 2.10 For Turkeys

#energy
te <- turkey_energy_factor(1:299, 1990:2011)
te$item <- rep(1079,nrow(te)) 
te <- te[, c("area", "year", "item", "energy")]

#protein
tp <- turkey_protein_factor(1:299, 1990:2011) 
turkey <- merge(te, tp, all=T)


## 3. COMBINE INDICES

indices <- rbind(cattle, buffalo, sheep, goat, camel, pig, chicken, duck, goose, turkey) 

## 4. PREPARE OUTPUT CSV

# format
indices <- indices[, c("area", "item", "year", "energy", "protein")] 
colnames(indices) <- c("Area.Code", "Item.Code", "Year", "Energy.Factor", "Protein.Factor")
indices <- indices[order(indices$Area.Code),]

#write
#write.csv(indices, 'T:/Onno/Feed-Model/Expert-Approach/Data/Animal Unit Indices/aui_6.csv', row.names=F)
write.csv(indices, '../Data/trans/aui_6.csv', row.names=F)


## End