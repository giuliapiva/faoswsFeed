#### The Food and Agriculture Organization of the United Nations (FAO)
#### This script has been created on 05/01/2014 by BD bernhard.dalheiemr@fao.org
##
##       V E R S I O N   6
##
## Last revised on 08/10/2014
##
## MAIN CHANGES
##  (a) Plausibility-Intervals for Energy and Protein Demand (see 6) 
##  
## 
## SUMMARY
## Within the framework of estimating feed use in FBS
## this script will calculate feed demand in terms of Energy (MJ) and Protein 
## (Crude Protein) expressed in tonnes. The work done in this script serves as a modul for the 
## previously establish model for feed demand in the livestock sector.
##
##  INDEX
##  1. Retrieve and prepare data from global aquaculture production
##  2. Load parameters "Percent on Feed" and "Economic Feed Efficiency Ratio" (EFCR). 
##     Extra-/Interpolate values for missing years
##  3. Apply country-specific FCRs and apply preditction mechanism on them
##  #4. Apply country-specific shares of demand covered by Fish meal and Fish oil
##  5. calculate Energy (MJ) and Protein(tonnes)
##  #6. Construct lower and upper bounds for energy and protein demand 
##     
## REQUIRED DATA INPUT:
## aquaculture_production.csv
## 2006.csv
## efcr.csv
## fo.csv
## fm.csv
## sws.csv
##
##START
##
## packages

library(stringr)
library(plyr)

########## 1.GLOBAL AQUACULTURE PRODUCTION

## retrieve data on production from FISHSTAT in folder as csv file and delete unnecessary info
setwd('T:/Onno/Feed-Model/Expert-Approach/Programming/Data/source')

aqua <- read.csv('aquaculture_production.csv')

aqua$S_1990 <- NULL
aqua$S_1991 <- NULL
aqua$S_1992 <- NULL
aqua$S_1993 <- NULL
aqua$S_1994 <- NULL
aqua$S_1995 <- NULL
aqua$S_1996 <- NULL
aqua$S_1997 <- NULL
aqua$S_1998 <- NULL
aqua$S_1999 <- NULL
aqua$S_2000 <- NULL
aqua$S_2001 <- NULL
aqua$S_2002 <- NULL
aqua$S_2003 <- NULL
aqua$S_2004 <- NULL
aqua$S_2005 <- NULL
aqua$S_2006 <- NULL
aqua$S_2007 <- NULL 
aqua$S_2008 <- NULL
aqua$S_2009 <- NULL
aqua$S_2010 <- NULL
aqua$S_2011 <- NULL
aqua$S_2012 <- NULL

colnames(aqua)[c(1,2,3,4,5)] <- c("area", "ocean.area", "Environment", "Species", "scientific.name") 

## Allocate area codes provided by csv
codes <- read.csv('sws.csv')
colnames(codes) <- c("area.code", "area")
aqua<- merge(codes, aqua, by="area", all.y=T)

## Compensate for discrepancies in area names 
aqua$area.code <- ifelse(aqua$area =="Bolivia (Plurinat.State)" , 19,
  ifelse( aqua$area =="Bosnia and Herzegovina", 80,
  ifelse( aqua$area =="Brunei Darussalam", 26,
  ifelse( aqua$area =="Côte d'Ivoire", 107,
  ifelse( aqua$area =="Central African Republic", 37,
  ifelse( aqua$area == "Channel Islands", 259,
  ifelse( aqua$area == "China", 41,
  ifelse( aqua$area == "China, Hong Kong SAR", 96,
  ifelse( aqua$area == "Congo, Dem. Rep. of the", 250,
  ifelse( aqua$area =="Congo, Republic of" , 46,
  ifelse( aqua$area =="Cook Islands" , 47,
  ifelse( aqua$area =="Czech Republic" , 167,
  ifelse( aqua$area =="Czechoslovakia" , 51,
  ifelse( aqua$area =="Dominican Republic" , 56,
  ifelse( aqua$area =="Equatorial Guinea" , 61,
  ifelse( aqua$area =="Faroe Islands" , 64,
  ifelse( aqua$area =="Fiji, Republic of" , 66,
  ifelse( aqua$area =="French Guiana" , 69,
  ifelse( aqua$area =="French Polynesia" , 70,
  ifelse( aqua$area == "Iran (Islamic Rep. of)", 102,
  ifelse( aqua$area == "Korea, Dem. People's Rep", 116,
  ifelse( aqua$area == "Korea, Republic of", 117,
  ifelse( aqua$area == "Lao People's Dem. Rep.", 120,
  ifelse( aqua$area == "Macedonia, Fmr Yug Rp of", 154,
  ifelse( aqua$area == "Moldova, Republic of", 146,
  ifelse( aqua$area == "New Caledonia", 153,
  ifelse( aqua$area == "Northern Mariana Is.", 163,
  ifelse( aqua$area == "Palestine, Occupied Tr.", 299,
  ifelse( aqua$area == "Papua New Guinea", 168, 
  ifelse( aqua$area == "Réunion", 182,
  ifelse( aqua$area == "Russian Federation", 185,
  ifelse( aqua$area == "Saint Kitts and Nevis", 188,
  ifelse( aqua$area == "Saint Lucia", 185,  
  ifelse( aqua$area == "Serbia and Montenegro", 186, 
  ifelse( aqua$area == "Solomon Islands", 25,
  ifelse( aqua$area == "Sudan (former)", 206,
  ifelse( aqua$area == "Syrian Arab Republic", 212,
  ifelse( aqua$area == "Taiwan Province of China", 214,
  ifelse( aqua$area == "Tanzania, United Rep. of", 215,
  ifelse( aqua$area == "Trinidad and Tobago", 220,
  ifelse( aqua$area == "United Arab Emirates", 225,
  ifelse( aqua$area == "United Kingdom", 229,
  ifelse( aqua$area == "United States of America", 231,
  ifelse( aqua$area == "US Virgin Islands", 240,
  ifelse( aqua$area == "Venezuela, Boliv Rep of", 236,
  ifelse( aqua$area == "Yugoslavia SFR", 248,
  ifelse( aqua$area == "Zanzibar", 3698,
                        aqua$area.code)))))))))))))))))))))))))))))))))))))))))))))))
          
## format into long data
aqua2 <- reshape(aqua, direction="long", varying=7:29, v.names="quantity", 
                 idvar=c("area", "area.code", "ocean.area", "Environment", "Species", "scientific.name"), timevar="Year", sep ="", times=1990:2012)
aqua <- data.frame(aqua2$area, aqua2$area.code, aqua2$ocean.area, aqua2$Environment, aqua2$Species, aqua2$scientific.name, aqua2$Year, aqua2$quantity)
colnames(aqua)<- c("area", "area.code", "ocean.area", "Environment", "Species", "scientific.name", "year", "quantity")

## Aggregate species (12 categories)
aqua$type <- ifelse(str_detect(aqua$Species, ignore.case("carp")) | str_detect( aqua$Species, ignore.case("cyprinids")), "carp", 
                    ifelse( str_detect(aqua$Species, ignore.case("catla")), "catla",
                    ifelse( str_detect(aqua$Species, ignore.case("roho")), "rohu",
                    ifelse( str_detect(aqua$Species, ignore.case("catfish")), "catfish",
                    ifelse( str_detect(aqua$Species, ignore.case("salmon")), "salmon",
                    ifelse( str_detect(aqua$Species, ignore.case("trout")), "trout",
                    ifelse( str_detect(aqua$Species, ignore.case("milkfish")), "milkfish",
                    ifelse( str_detect(aqua$Species, ignore.case("shrimp")), "marine crustaceans",
                    ifelse( str_detect(aqua$Species, ignore.case("crab")), "marine crustaceans",
                    ifelse( str_detect(aqua$Species, ignore.case("lobster")), "marine crustaceans",               
                    ifelse( str_detect(aqua$Species, ignore.case("Freshwater crustaceans")), "freshwater crustaceans",
"to_be_ident")))))))))))

aqua$type <- ifelse( aqua$Species == "Tilapias and other cichlids", "tilapias",                             
             ifelse( aqua$Species == "Miscellaneous marine crustaceans", "marine crustaceans",                    
             ifelse( aqua$type == "to_be_ident" & str_detect(aqua$Species, ignore.case("marine")),  "miscellaneous marine fish",
             ifelse( aqua$type == "to_be_ident" & aqua$Environment == "Freshwater", "miscellaneous freshwater fish", 
             ifelse( aqua$type == "to_be_ident" & aqua$Environment == "Marine", "miscellaneous marine fish",
             ifelse( aqua$Species == "Miscellaneous freshwater fishes", "miscellaneous freshwater fish",           
             ifelse( aqua$type == "to_be_ident" & str_detect(aqua$Species, ignore.case("river")), "miscellaneous freshwater fish",        
             ifelse( aqua$type == "to_be_ident" & aqua$Environment == "Brackishwater", "miscellaneous freshwater fish",         
                     aqua$type))))))))
aqua <- ddply(aqua, .( area, area.code, year, type), function(x) {
  sum(x$quantity)})

colnames(aqua) <- c("area", "area.code", "year", "type", "quantity")

########## 2. PARAMETERS EFCR, PERCENT ON FEED

## import parameters on EFCR, Percent on feed from csv
parameters <- read.csv('efcr.csv')

##prepare target for prediction
list <- ddply(parameters, c("type"), summarise, year = 1990:2012)
list <- merge(list, parameters, by=c("type", "year"), all=T)
test <- list[,1:2]

## predict missing EFCRs
fast <- ddply(parameters, .(type), function(x) {predict(lm(EFCR ~ poly(year, 2), data=x ), newdata=test)})

## predict missing Percent on feeds
pof <- ddply(parameters, .(type), function(x) {predict(lm(Percent.on.Feeds ~ poly(year, 2), data=x ), newdata=test)})

## predict missing FM
fm <- ddply(parameters, .(type), function(x) {predict(lm(fm ~ poly(year, 4), data=x ), newdata=test)})

## predict missing FO
fo <- ddply(parameters, .(type), function(x) {predict(lm(fo ~ poly(year, 2), data=x ), newdata=test)})


## clean up
fast <- fast[,1:26]
pof <- pof[,1:26]
fm <- fm[,1:26]
fo <- fo[,1:26]

fast <- reshape(fast, direction="long", varying=list(2:26), v.names="pred1", 
                 idvar="type")
fast <- data.frame(fast, row.names= NULL)
fast <- fast[order(fast$type),]

pof <- reshape(pof, direction="long", varying=list(2:26), v.names="pred2", 
                idvar="type")
pof <- data.frame(pof, row.names= NULL)
pof <- pof[order(pof$type),]


fm <- reshape(fm, direction="long", varying=list(2:26), v.names="pred3", 
                idvar="type")
fm <- data.frame(fm, row.names= NULL)
fm <- fm[order(fm$type),]

fo <- reshape(fo, direction="long", varying=list(2:26), v.names="pred4", 
                idvar="type")
fo <- data.frame(fo, row.names= NULL)
fo <- fo[order(fo$type),]

test$pred1 <- fast$pred1
test$pred2 <- pof$pred2
test$pred3 <- fm$pred3
test$pred4 <- fo$pred4

list <- merge(list, test, by=c("type", "year"), all=TRUE)
              


## merge predictions and given data
list$EFCR <- ifelse(is.na(list$EFCR), list$pred1, list$EFCR)
list$Percent.on.Feeds<- ifelse(is.na(list$Percent.on.Feeds), list$pred2, list$Percent.on.Feeds)
list$fm <- ifelse(is.na(list$fm), list$pred3, list$fm)
list$fo <- ifelse(is.na(list$fo), list$pred4, list$fo)

list$pred1 <- NULL
list$pred2 <- NULL
list$pred3 <- NULL
list$pred4 <- NULL

aqua <- merge(aqua, list, by=c("type", "year"), all=TRUE)

## creating variables for eventually calculating energy & protein
aqua <- aqua[order(aqua$area),]

test <- aqua$EFCR[1:length(aqua$EFCR)-1]
aqua$aqua.prev=c(0,test)
aqua$changeEFCR <- ((aqua$EFCR - aqua$aqua.prev)/aqua$aqua.prev)
aqua$changeEFCR <- ifelse( aqua$year==1990, 0, aqua$changeEFCR)

test <- aqua$fm[1:length(aqua$fm)-1]
aqua$fm.prev=c(0,test)
aqua$changefm <- ((aqua$fm - aqua$fm.prev)/aqua$fm.prev)
aqua$changefm <- ifelse( aqua$year==1990, 0, aqua$changefm)

test <- aqua$fo[1:length(aqua$fo)-1]
aqua$fo.prev=c(0,test)
aqua$changefo <- ((aqua$fo - aqua$fo.prev)/aqua$fo.prev)
aqua$changefo <- ifelse( aqua$year==1990, 0, aqua$changefo)


########## 3. COUNTRY SPECIFIC EFCR

## Import and prepare data
EFCR2006 <- read.csv('2006.csv') 

EFCR2006 <- reshape(EFCR2006, direction="long", varying=list(names(EFCR2006)[3:11]), v.names="EFCR", 
                    idvar=c("area.code", "year"), times= (names(EFCR2006)[3:11]))

EFCR2006 <- data.frame(EFCR2006, row.names = NULL) 
EFCR2006 <- EFCR2006[order(EFCR2006$area.code),]
EFCR2006 <- EFCR2006[complete.cases(EFCR2006),]

colnames(EFCR2006) <- c("area.code", "year", "type", "EFCR2006")

EFCR2006$type <- sub("[.]", " ", EFCR2006$type)

## merge with all data
aqua <- merge(aqua, EFCR2006, by=c("area.code",  "type", "year"), all=T)
aqua <- aqua[order(aqua$area),]

aqua$EFCR <- round(aqua$EFCR, digits=1)

## apply prediction method also to missing country specific EFCR

createbackw <- function(year) { test <- aqua$EFCR2006[2:length(aqua$EFCR2006)]
                                test2 <- aqua$changeEFCR[2:length(aqua$changeEFCR)]
             aqua$EFCR1=c(test,0)
             aqua$EFCR2=c(test2,0)
               v1 <- ifelse( aqua$year == year, aqua$EFCR1 * (1 - aqua$EFCR2), aqua$EFCR2006)

                        aqua$EFCR1 <- NULL
v1
}


createforw <- function(year) { test <- aqua$EFCR2006[1:length(aqua$EFCR2006)-1]
                           
                           aqua$EFCR1=c(0,test)
                           
                           v1 <- ifelse( aqua$year == year, aqua$EFCR1 * (1 + aqua$changeEFCR), aqua$EFCR2006)
                           
                           aqua$EFCR1 <- NULL
                           v1
}

aqua$EFCR2006 <- createforw(2007)
aqua$EFCR2006 <- createforw(2008)
aqua$EFCR2006 <- createforw(2009)
aqua$EFCR2006 <- createforw(2010)
aqua$EFCR2006 <- createforw(2011)
aqua$EFCR2006 <- createforw(2012)

aqua$EFCR2006 <- createbackw(2005)
aqua$EFCR2006 <- createbackw(2004)
aqua$EFCR2006 <- createbackw(2003)
aqua$EFCR2006 <- createbackw(2002)
aqua$EFCR2006 <- createbackw(2001)
aqua$EFCR2006 <- createbackw(2000)
aqua$EFCR2006 <- createbackw(1999)
aqua$EFCR2006 <- createbackw(1998)
aqua$EFCR2006 <- createbackw(1997)
aqua$EFCR2006 <- createbackw(1996)
aqua$EFCR2006 <- createbackw(1995)
aqua$EFCR2006 <- createbackw(1994)
aqua$EFCR2006 <- createbackw(1993)
aqua$EFCR2006 <- createbackw(1992)
aqua$EFCR2006 <- createbackw(1991)
aqua$EFCR2006 <- createbackw(1990)

## clean up
aqua$EFCR <- ifelse(!is.na(aqua$EFCR2006), aqua$EFCR2006, aqua$EFCR)
aqua$aqua.prev <- NULL
aqua$change <- NULL
aqua$EFCR2006 <- NULL

## round 
aqua$EFCR <- round(aqua$EFCR, digits=1)
aqua$Percent.on.Feeds <- round(aqua$Percent.on.Feeds, digits=2)

## disregard unrelevant values
aqua <- aqua[!is.na(aqua$area),]

########## 4. COUNTRY SPECIFIC SHARES OF FISH MEAL and FISH OIL IN FEED


## Load and prepare data on FM
fm2006 <- read.csv('fm.csv')

fm2006 <- reshape(fm2006, direction="long", varying=list(names(fm2006)[3:11]), v.names="fm", 
                   idvar=c("area.code", "year"), times= (names(fm2006)[3:11]))
fm2006 <- data.frame(fm2006, row.names = NULL) 
fm2006 <- fm2006[order(fm2006$area.code),]
fm2006 <- fm2006[complete.cases(fm2006),]
colnames(fm2006) <- c("area.code", "year", "type", "fm2006")
fm2006$type <- sub("[.]", " ", fm2006$type)
fm2006$fm2006 <- fm2006$fm2006 / 100

## merge with all data
aqua <- merge(aqua, fm2006, by=c("area.code",  "type", "year"), all=T)
aqua <- aqua[order(aqua$area),]



## apply prediction method also to missing country specific FM

createbackw <- function(year) { test <- aqua$fm2006[2:length(aqua$fm2006)]
                                test2 <- aqua$changefm[2:length(aqua$changefm)]
                                aqua$fm1=c(test,0)
                                aqua$fm2=c(test2,0)
                                v1 <- ifelse( aqua$year == year, aqua$fm1 * (1 - aqua$fm2), aqua$fm2006)
                                
                                aqua$fm1 <- NULL
                                v1
}


createforw <- function(year) { test <- aqua$fm2006[1:length(aqua$fm2006)-1]
                               
                               aqua$fm1=c(0,test)
                               
                               v1 <- ifelse( aqua$year == year, aqua$fm1 * (1 + aqua$changefm), aqua$fm2006)
                               
                               aqua$fm <- NULL
                               v1
}

aqua$fm2006 <- createforw(2007)
aqua$fm2006 <- createforw(2008)
aqua$fm2006 <- createforw(2009)
aqua$fm2006 <- createforw(2010)
aqua$fm2006 <- createforw(2011)
aqua$fm2006 <- createforw(2012)

aqua$fm2006 <- createbackw(2005)
aqua$fm2006 <- createbackw(2004)
aqua$fm2006 <- createbackw(2003)
aqua$fm2006 <- createbackw(2002)
aqua$fm2006 <- createbackw(2001)
aqua$fm2006 <- createbackw(2000)
aqua$fm2006 <- createbackw(1999)
aqua$fm2006 <- createbackw(1998)
aqua$fm2006 <- createbackw(1997)
aqua$fm2006 <- createbackw(1996)
aqua$fm2006 <- createbackw(1995)
aqua$fm2006 <- createbackw(1994)
aqua$fm2006 <- createbackw(1993)
aqua$fm2006 <- createbackw(1992)
aqua$fm2006 <- createbackw(1991)
aqua$fm2006 <- createbackw(1990)

## clean up
aqua$fm <- ifelse(!is.na(aqua$fm2006), aqua$fm2006, aqua$fm)
aqua$fm.prev <- NULL
aqua$changefm <- NULL
aqua$fm2006 <- NULL

## round 
aqua$fm <- round(aqua$fm, digits=2)


## disregard unrelevant values
aqua <- aqua[!is.na(aqua$area),]


## Load and prepare data on FO
fo2006 <- read.csv('fo.csv')


fo2006 <- reshape(fo2006, direction="long", varying=list(names(fo2006)[3:11]), v.names="fo", 
                  idvar=c("area.code", "year"), times= (names(fo2006)[3:11]))

fo2006 <- data.frame(fo2006, row.names = NULL) 
fo2006 <- fo2006[order(fo2006$area.code),]
fo2006 <- fo2006[complete.cases(fo2006),]


colnames(fo2006) <- c("area.code", "year", "type", "fo2006")

fo2006$type <- sub("[.]", " ", fo2006$type)
fo2006$fo2006 <- fo2006$fo2006 / 100

## merge with all data
aqua <- merge(aqua, fo2006, by=c("area.code",  "type", "year"), all=T)
aqua <- aqua[order(aqua$area),]



## apply prediction method also to missing country specific FO

createbackw <- function(year) { test <- aqua$fo2006[2:length(aqua$fo2006)]
                                test2 <- aqua$changefo[2:length(aqua$changefo)]
                                aqua$fo1=c(test,0)
                                aqua$fo2=c(test2,0)
                                v1 <- ifelse( aqua$year == year, aqua$fo1 * (1 - aqua$fo2), aqua$fo2006)
                                
                                aqua$fo1 <- NULL
                                v1
}


createforw <- function(year) { test <- aqua$fo2006[1:length(aqua$fo2006)-1]
                               
                               aqua$fo1=c(0,test)
                               
                               v1 <- ifelse( aqua$year == year, aqua$fo1 * (1 + aqua$changefo), aqua$fo2006)
                               
                               aqua$fo <- NULL
                               v1
}

aqua$fo2006 <- createforw(2007)
aqua$fo2006 <- createforw(2008)
aqua$fo2006 <- createforw(2009)
aqua$fo2006 <- createforw(2010)
aqua$fo2006 <- createforw(2011)
aqua$fo2006 <- createforw(2012)

aqua$fo2006 <- createbackw(2005)
aqua$fo2006 <- createbackw(2004)
aqua$fo2006 <- createbackw(2003)
aqua$fo2006 <- createbackw(2002)
aqua$fo2006 <- createbackw(2001)
aqua$fo2006 <- createbackw(2000)
aqua$fo2006 <- createbackw(1999)
aqua$fo2006 <- createbackw(1998)
aqua$fo2006 <- createbackw(1997)
aqua$fo2006 <- createbackw(1996)
aqua$fo2006 <- createbackw(1995)
aqua$fo2006 <- createbackw(1994)
aqua$fo2006 <- createbackw(1993)
aqua$fo2006 <- createbackw(1992)
aqua$fo2006 <- createbackw(1991)
aqua$fo2006 <- createbackw(1990)

## clean up
aqua$fo <- ifelse(!is.na(aqua$fo2006), aqua$fo2006, aqua$fo)
aqua$fo.prev <- NULL
aqua$changefo <- NULL
aqua$fo2006 <- NULL

## round 
aqua$fo <- round(aqua$fo, digits=3)

## Correct for unrealistic fm and fo
aqua$fo <- ifelse(aqua$fm + aqua$fo > 1, 0.5, aqua$fo)
aqua$fm <- ifelse(aqua$fm + aqua$fo > 1, 0.5, aqua$fm)

## disregard unrelevant values
aqua <- aqua[!is.na(aqua$area),]

########## 5. TOTAL DEMAND FOR ENERGY (MJ) AND PROTEIN (TONNES)

## Apply EFCR and POF to Production (=total feed (tonnes))
aqua$v1 <- aqua$quantity * aqua$Percent.on.Feeds * aqua$EFCR
feed <- aqua$v1

## apply species specific factors for Energy 
aqua$energy <- ifelse(aqua$type == "carp", feed * 15600,
               ifelse(aqua$type == "catla", feed * 14600,
               ifelse(aqua$type == "catfish", feed * 21000,
               ifelse(aqua$type == "rohu", feed * 16300,
               ifelse(aqua$type == "tilapias", feed * 11100,
               ifelse(aqua$type == "salmon", feed * 20000,
               ifelse(aqua$type == "trout", feed * 15500,
               ifelse(aqua$type == "milkfish", feed * 12600,
               ifelse(aqua$type == "marine crustaceans", feed * 16700,
               ifelse(aqua$type == "freshwater crustaceans", feed * 12800,
               ifelse(aqua$type == "miscellaneous marine fish", feed * 19000,
               ifelse(aqua$type == "miscellaneous freshwater fish", feed * 13600,
                     "NA"))))))))))))
aqua$energy <- as.numeric(aqua$energy)

## apply species specific factors for Protein
aqua$protein <- ifelse(aqua$type == "carp", feed * 0.32,
                ifelse(aqua$type == "catla", feed * 0.37,
                ifelse(aqua$type == "catfish", feed * 0.35,
                ifelse(aqua$type == "rohu", feed * 0.29,
                ifelse(aqua$type == "tilapias", feed * 0.35,
                ifelse(aqua$type == "salmon", feed * 0.42,
                ifelse(aqua$type == "trout", feed * 0.42,
                ifelse(aqua$type == "milkfish", feed * 0.4,
                ifelse(aqua$type == "marine crustaceans", feed * 0.5,
                ifelse(aqua$type == "freshwater crustaceans", feed * 0.4,
                ifelse(aqua$type == "miscellaneous marine fish", feed * 0.42,
                ifelse(aqua$type == "miscellaneous freshwater fish", feed * 0.32,
                "NA"))))))))))))
aqua$protein <- as.numeric(aqua$protein)

## Summ up all requirements in countries

# for Energy
out1 <- ddply(aqua, .( area, area.code, year), function(x) {
  sum(x$energy)})   
colnames(out1) <- c("area", "area.code", "year", "energy") 
# for Protein
out2 <- ddply(aqua, .( area, area.code, year), function(x) {
  sum(x$protein)}) 
colnames(out2) <- c("area", "area.code", "year", "protein")

## prepare output and csv ("aquademand.csv")

out <- merge(out1, out2, by=c("area.code", "area", "year"), all=T )


########## 6. UPPER AND LOWER BOUNDS OF DEMAND FOR ENERGY (MJ) AND PROTEIN (TONNES)




## Construct bounds for EFCR, Proportion on Feed, Share of FM/FO

##EFCR
#aqua$lgEFCR<- log(aqua$EFCR)
#min <- ddply(aqua, .(type), function(x) {
#  min(x$lgEFCR)})
#colnames(min) <- c("type", "min")

#max <- ddply(aqua, .(type), function(x) {
#  max(x$lgEFCR)})
#colnames(max) <- c("type", "max")

#med <- ddply(aqua, .(area, type), function(x) {
#  median(x$lgEFCR)})
#colnames(med) <- c("area","type", "med")

#var <- ddply(med, .(type), function(x) {
#  var(x$med)})
#colnames(med) <- c("type", "var")
#EFCR_bounds <- merge(min, max) 

#EFCR_bounds <- merge(EFCR_bounds, var)

#EFCR_bounds$min_orig <- exp(EFCR_bounds$min)
#EFCR_bounds$max_orig <- exp(EFCR_bounds$max)

#EFCR_bounds$EFCR_lower <- exp(EFCR_bounds$min - EFCR_bounds$V1^0.5)
#EFCR_bounds$EFCR_upper <- exp(EFCR_bounds$max + EFCR_bounds$V1^0.5)

#EFCR_bounds$min <- NULL
#EFCR_bounds$max <- NULL
#EFCR_bounds$V1 <- NULL
#EFCR_bounds$min_orig <- NULL
#EFCR_bounds$max_orig <- NULL

## Proportion on Feeds
#aqua$lgPoF<- log(aqua$Percent.on.Feeds/(1-aqua$Percent.on.Feed))
#min <- ddply(aqua, .(type), function(x) {
#  min(x$lgPoF)})
#colnames(min) <- c("type", "min")

#max <- ddply(aqua, .(type), function(x) {
#  max(x$lgPoF)})
#colnames(max) <- c("type", "max")

#med <- ddply(aqua, .(area, type), function(x) {
#  median(x$lgPoF)})
#colnames(med) <- c("area","type", "med")

#var <- ddply(med, .(type), function(x) {
#  var(x$med)})
#colnames(med) <- c("type", "var")
#PoF_bounds <- merge(min, max) 

#PoF_bounds <- merge(PoF_bounds, var)

#PoF_bounds$min_orig <- exp(PoF_bounds$min)/(1+exp(PoF_bounds$min))
#PoF_bounds$max_orig <- exp(PoF_bounds$max)/(1+exp(PoF_bounds$max))

#PoF_bounds$PoF_lower <- exp(PoF_bounds$min - PoF_bounds$V1^0.5)/(1+exp(PoF_bounds$min - PoF_bounds$V1^0.5))
#PoF_bounds$PoF_upper <- exp(PoF_bounds$max + PoF_bounds$V1^0.5)/(1+exp(PoF_bounds$max - PoF_bounds$V1^0.5))

#PoF_bounds$PoF_upper <- ifelse(PoF_bounds$type == "salmon" | PoF_bounds$type == "trout", 1, PoF_bounds$PoF_upper )
#PoF_bounds$PoF_lower <- ifelse(PoF_bounds$type == "salmon" | PoF_bounds$type == "trout", 1, PoF_bounds$PoF_lower )


#PoF_bounds$min <- NULL
#PoF_bounds$max <- NULL
#PoF_bounds$V1 <- NULL
#PoF_bounds$min_orig <- NULL
#PoF_bounds$max_orig <- NULL

## Fish meal
#aqua$lgFM<- log(aqua$fm/(1-aqua$fm))
#min <- ddply(aqua, .(type), function(x) {
#  min(x$lgFM)})
#colnames(min) <- c("type", "min")

#max <- ddply(aqua, .(type), function(x) {
#  max(x$lgFM)})
#colnames(max) <- c("type", "max")

#med <- ddply(aqua, .(area, type), function(x) {
#  median(x$lgFM)})
#colnames(med) <- c("area","type", "med")

#var <- ddply(med, .(type), function(x) {
#  var(x$med)})
#colnames(med) <- c("type", "var")
#fm_bounds <- merge(min, max) 

#fm_bounds <- merge(fm_bounds, var)

#fm_bounds$min_orig <- exp(fm_bounds$min)/(1+exp(fm_bounds$min))
#fm_bounds$max_orig <- exp(fm_bounds$max)/(1+exp(fm_bounds$max))

#fm_bounds$fm_lower <- exp(fm_bounds$min - fm_bounds$V1^0.5)/(1+exp(fm_bounds$min - fm_bounds$V1^0.5))
#fm_bounds$fm_upper <- exp(fm_bounds$max + fm_bounds$V1^0.5)/(1+exp(fm_bounds$max - fm_bounds$V1^0.5))

#fm_bounds$min <- NULL
#fm_bounds$max <- NULL
#fm_bounds$V1 <- NULL
#fm_bounds$min_orig <- NULL
#fm_bounds$max_orig <- NULL


## Fish Oil
#aqua$lgFO<- log(aqua$fo/(1-aqua$fo))
#min <- ddply(aqua, .(type), function(x) {
#  min(x$lgFO)})
#colnames(min) <- c("type", "min")

#max <- ddply(aqua, .(type), function(x) {
#  max(x$lgFO)})
#colnames(max) <- c("type", "max")

#med <- ddply(aqua, .(area, type), function(x) {
#  median(x$lgFO)})
#colnames(med) <- c("area","type", "med")

#var <- ddply(med, .(type), function(x) {
#  var(x$med)})
#colnames(med) <- c("type", "var")
#fo_bounds <- merge(min, max) 

#fo_bounds <- merge(fo_bounds, var)

#fo_bounds$min_orig <- exp(fo_bounds$min)/(1+exp(fo_bounds$min))
#fo_bounds$max_orig <- exp(fo_bounds$max)/(1+exp(fo_bounds$max))

#fo_bounds$fo_lower <- exp(fo_bounds$min - fo_bounds$V1^0.5)/(1+exp(fo_bounds$min - fo_bounds$V1^0.5))
#fo_bounds$fo_upper <- exp(fo_bounds$max + fo_bounds$V1^0.5)/(1+exp(fo_bounds$max - fo_bounds$V1^0.5))

#fo_bounds$fo_lower <- ifelse(is.na(fo_bounds$fo_lower), fo_bounds$min_orig, fo_bounds$fo_lower )
#fo_bounds$fo_upper <- ifelse(is.na(fo_bounds$fo_upper), fo_bounds$max_orig, fo_bounds$fo_upper )

#fo_bounds$min <- NULL
#fo_bounds$max <- NULL
#fo_bounds$V1 <- NULL
#fo_bounds$min_orig <- NULL
#fo_bounds$max_orig <- NULL

## Merge with main data
#aqua<- merge(aqua, EFCR_bounds, all=T)
#aqua<- merge(aqua, PoF_bounds, all=T)
#aqua<- merge(aqua, fm_bounds, all=T)
#aqua<- merge(aqua, fo_bounds, all=T)

#aqua$fo_upper <- ifelse(aqua$fm_upper + aqua$fo_upper > 1, 0.5, aqua$fo_upper)
#aqua$fm_upper <- ifelse(aqua$fm_upper + aqua$fo_upper > 1, 0.5, aqua$fm_upper)

#aqua$fo_lower <- ifelse(aqua$fm_lower + aqua$fo_lower > 1, 0.5, aqua$fo_lower)
#aqua$fm_lower <- ifelse(aqua$fm_lower + aqua$fo_lower > 1, 0.5, aqua$fm_lower)

## Construct bounds for total demands

## Apply EFCR and POF to Production (=total feed (tonnes))
#aqua$lower <- aqua$quantity * aqua$PoF_lower * aqua$EFCR_lower * (1- aqua$fm_upper - aqua$fo_upper) 
#aqua_lower <- aqua$lower

## apply species specific lower bound factors for Energy 
#aqua$energy_lower <- ifelse(aqua$type == "carp", aqua_lower * 13000,
#ifelse(aqua$type == "catla", aqua_lower * 12000,
#ifelse(aqua$type == "catfish", aqua_lower * 18700,
#ifelse(aqua$type == "rohu", aqua_lower * 15870,
#ifelse(aqua$type == "tilapias", aqua_lower * 10000,
#ifelse(aqua$type == "salmon", aqua_lower * 19000,
#ifelse(aqua$type == "trout", aqua_lower * 14500,
#ifelse(aqua$type == "milkfish", aqua_lower * 10400,
#ifelse(aqua$type == "marine crustaceans", aqua_lower * 15880,
#ifelse(aqua$type == "freshwater crustaceans", aqua_lower * 12122,
#ifelse(aqua$type == "miscellaneous marine fish", aqua_lower * 14300,
#ifelse(aqua$type == "miscellaneous freshwater fish", aqua_lower * 10000,
#"NA"))))))))))))
#aqua$energy_lower <- as.numeric(aqua$energy_lower)

## apply species specific lower bound factors for Protein
#aqua$protein_lower <- ifelse(aqua$type == "carp", aqua_lower * 0.28,
#ifelse(aqua$type == "catla", aqua_lower * 0.3,
#ifelse(aqua$type == "catfish", aqua_lower * 0.29,
#ifelse(aqua$type == "rohu", aqua_lower * 0.25,
#ifelse(aqua$type == "tilapias", aqua_lower * 0.28,
#ifelse(aqua$type == "salmon", aqua_lower * 0.4,
#ifelse(aqua$type == "trout", aqua_lower * 0.35,
#ifelse(aqua$type == "milkfish", aqua_lower * 0.3,
#ifelse(aqua$type == "marine crustaceans", aqua_lower * 0.43,
#ifelse(aqua$type == "freshwater crustaceans", aqua_lower * 0.28,
#ifelse(aqua$type == "miscellaneous marine fish", aqua_lower * 0.31,
#ifelse(aqua$type == "miscellaneous freshwater fish", aqua_lower * 0.25,
#"NA"))))))))))))
#aqua$protein_lower <- as.numeric(aqua$protein_lower)

#aqua$upper <- aqua$quantity * aqua$PoF_upper * aqua$EFCR_upper * (1- aqua$fm_lower - aqua$fo_lower) 
#aqua_upper <- aqua$upper

## apply species specific upper bound factors for Energy 
# aqua$energy_upper <- ifelse(aqua$type == "carp", aqua_upper * 18000,
# ifelse(aqua$type == "catla", aqua_upper * 17000,
# ifelse(aqua$type == "catfish", aqua_upper * 22600,
# ifelse(aqua$type == "rohu", aqua_upper * 16720,
# ifelse(aqua$type == "tilapias", aqua_upper * 13000,
# ifelse(aqua$type == "salmon", aqua_upper * 21000,
# ifelse(aqua$type == "trout", aqua_upper * 16500,
# ifelse(aqua$type == "milkfish", aqua_upper * 14700,
# ifelse(aqua$type == "marine crustaceans", aqua_upper * 16720,
# ifelse(aqua$type == "freshwater crustaceans", aqua_upper * 13376,
# ifelse(aqua$type == "miscellaneous marine fish", aqua_upper * 21000,
# ifelse(aqua$type == "miscellaneous freshwater fish", aqua_upper * 15000,
# "NA"))))))))))))
# aqua$energy_upper <- as.numeric(aqua$energy_upper)

## apply species specific upper boundfactors for Protein
# aqua$protein_upper <- ifelse(aqua$type == "carp", aqua_upper * 0.40,
# ifelse(aqua$type == "catla", aqua_upper * 0.40,
# ifelse(aqua$type == "catfish", aqua_upper * 0.36,
# ifelse(aqua$type == "rohu", aqua_upper * 0.38,
# ifelse(aqua$type == "tilapias", aqua_upper * 0.48,
# ifelse(aqua$type == "salmon", aqua_upper * 0.48,
# ifelse(aqua$type == "trout", aqua_upper * 0.48,
# ifelse(aqua$type == "milkfish", aqua_upper * 0.38,
# ifelse(aqua$type == "marine crustaceans", aqua_upper * 0.50,
# ifelse(aqua$type == "freshwater crustaceans", aqua_upper * 0.4,
# ifelse(aqua$type == "miscellaneous marine fish", aqua_upper * 0.48,
# ifelse(aqua$type == "miscellaneous freshwater fish", aqua_upper * 0.34,
# "NA"))))))))))))
# aqua$protein_upper <- as.numeric(aqua$protein_upper)


## Extract relevant data (=Energy/Protein Demand + Bounds)
#  out3 <- ddply(aqua, .( area, area.code, year), function(x) {
#    sum(x$energy_lower)})   
#  colnames(out3) <- c("area", "area.code", "year", "energy_lower") 
#  out4 <- ddply(aqua, .( area, area.code, year), function(x) {
#    sum(x$energy_upper)})
# 
# colnames(out4) <- c("area", "area.code", "year", "energy_upper") 
# 
# # for Protein
# out5 <- ddply(aqua, .( area, area.code, year), function(x) {
#   sum(x$protein_lower)}) 
# colnames(out5) <- c("area", "area.code", "year", "protein_lower")
# 
# out6 <- ddply(aqua, .( area, area.code, year), function(x) {
#   sum(x$protein_upper)})
# colnames(out6) <- c("area", "area.code", "year", "protein_upper")
# 
# 
# 
# out <- merge(out1, out3, by=c("area.code", "area", "year"), all=T )
# out <- merge(out, out4, by=c("area.code", "area", "year"), all=T )
# out <- merge(out, out2, by=c("area.code", "area", "year"), all=T )
# out <- merge(out, out5, by=c("area.code", "area", "year"), all=T )
# out <- merge(out, out6, by=c("area.code", "area", "year"), all=T )

## prepare output file

write.csv(out, 'output/aquademand.csv', row.names=FALSE)


##
##END



