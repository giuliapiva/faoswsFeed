#### This script has been created on 015/03/2015 by BD bernhard.dalheiemr@fao.org
##                                                     bernhard.dalheimer@hotmail.com    
##
##                                   V E R S I O N   6
##
##                              Last revised on 01/04/2015
##
## 
## SUMMARY
## Having measured Energy (MJ) and Protein (t) requirements of herds by country and year, this script 
## allocates FAOSTAT commodities from which the animals retrieve their requirements. The results 
## are disaggregated estimates of quantities (t) of feed use that can be implementet in Food Balance Sheets.
##
##
## This script runs if the following items are located in the working directory:
## feedAvail.r, optimize.r, demandadjust.r, sws_query.r, ojdbc14.jar, area-regions-registry.csv, feedlist.csv
## total-feed-demand_6-6.csv
##
##
## 1. Programmes & data  
## 2. Input Parameters
## 3. Subtract Nutrient supply provided by FeedOnly items (oilcakes, brans, etc.)
## 4. Establish distributions of feed based on Availability 
## 5. Incorporate Official Feed
## 6. Allocate Feed
##
##
## START
start_time <-Sys.time()


#setwd('C:/Users/Dalheimer/Documents/R_Programs/Feed_Allocation_2015')
options(scipen=999)


##packages
library(faosws)
library(faoswsUtil)
suppressPackageStartupMessages(library(data.table))
library(lpSolve)
library(RJDBC)
library(stringr)
library(reshape2)
library(rJava)
library(plyr)
library(DBI)
library(faoswsFeed)

## functions
source('archive/R/feedAvail.r')
source('archive/R/demandadjust.r')
source('archive/R/optimize.r')
source('R/sws_query.r')

#Set environment
if (CheckDebug()) {
  SetClientFiles("~/certificates/production")
  #GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws", "ebdda55c-21a4-4bdd-9d0c-5098cec843f7")
  GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws", "f45d0a2a-a798-435d-84e9-897a572c0d10")
}

## CSV inputs
feedlist <- read.csv('../Data/source/feedlist.csv') 
#demand <- read.csv('../Data/source/total-feed-demand_6-6.csv')
feedDemand <- calculateFeedDemand()

# 2. Preparation of Parameters

## Feeditems
feedlist <- within(feedlist, {
                   ENERGY <- ENERGY * 1000
                   PROTEIN <- PROTEIN /100
})

## Potential Feeds (All feeditems excluding Oil meals, meals and brans)
potentialfeedlist <- feedlist[feedlist$feedClassification == "Potential Feed",]
potentialfeeds <- potentialfeedlist$item

## Protein meals and Items that have only feed purpose

proteinfeedlist <- feedlist[feedlist$feedClassification == "FeedOnly",]
  proteinfeeds <- proteinfeedlist$item
 
## Feed Demand 

colnames(demand) <- c("area", "areaname", "year", "EDemand", "PDemand")
  

# 3. Subtract Nutrients provided by FeedOnly items (oilcakes, brans, etc.)
  
## Retrieve Availability (Supply) of FeedOnly items
  psupply <- feedAvail(area=1:299, year= 1990:2012, proteinfeeds, stv=T, food=F, processed=F)
 psupply$flag <- NULL
## Calculate Energy and Protein Availability
### allocate feed conversionf actors for energy and protein
psupply <- merge(psupply, proteinfeedlist, all.x=T)
        psupply$feedClassification <- NULL
  
### calculate Availability
  psupply$energysupply <- psupply$Avail *psupply$ENERGY
  psupply$proteinsupply <- psupply$Avail * psupply$PROTEIN

### sum energy and protein availabilities from different items 
  aggenergy <- ddply(psupply, .( area, year), function(x) {
    sum(x$energysupply)  })
  
  aggprotein <- ddply(psupply, .( area, year), function(x) {
    sum(x$proteinsupply)  })
  
  colnames(aggprotein) <- c("area", "year", "aggprotein")
  colnames(aggenergy) <- c("area", "year", "aggenergy")
  
  aggenergy[is.na(aggenergy)] <- 0
  aggprotein[is.na(aggprotein)] <- 0
  aggenergy$aggenergy[aggenergy$aggenergy <0] <-0
  aggprotein$aggprotein[aggprotein$aggprotein <0] <-0
  demand <- merge(demand, aggenergy, all.x=T)
  demand <- merge(demand, aggprotein, all.x=T)

  demand[is.na(demand)] <- 0

### Subtract Availability from Demand
  demand <- within(demand, {
            REDemand <- EDemand - aggenergy
            RPDemand <- PDemand - aggprotein
  })


demand$REDemand[demand$REDemand < 0 ] <- 0
demand$RPDemand[demand$RPDemand < 0 ] <- 0

# 4. Establish distributions of feed based on Availability 

## Retrieve Potential feed items data
Avail <- feedAvail(area=1:299, year=1990:2012, potentialfeeds)
Avail$Avail[Avail$Avail <0] <- 0  

## Apply nutritive factors and calculate nutrient availability
Avail <- merge(Avail, potentialfeedlist, all.x=T)
Avail$feedClassification <- NULL
Official <- subset(Avail, flag == " " )
Avail <- subset(Avail, flag != " ")

Avail$flag <- NULL

Avail <- within(Avail, {
  energyAvail <- Avail * ENERGY
  proteinAvail <- Avail * PROTEIN
})

## Sum nutrient Availabilities for each country and year (for construction of shares)
totenergyAvail <- ddply(Avail, .(area, year), 
                        function(x){sum(x$energyAvail)})
totproteinAvail <- ddply(Avail, .(area, year), 
                         function(x){sum(x$proteinAvail)})

colnames(totenergyAvail) <- c("area", "year", "totenergyAvail")
  Avail <- merge(Avail, totenergyAvail, all.x=T)

colnames(totproteinAvail) <- c("area", "year", "totproteinAvail")
  Avail <- merge(Avail, totproteinAvail, all.x=T)

## Construct shares of feed availablility
Avail <- within(Avail, {
                eshare <- energyAvail / totenergyAvail
                pshare <- proteinAvail / totproteinAvail
})

Avail[is.na(Avail)] <- 0

Avail <- data.table(Avail)
demand <- data.table(demand)

# 5. Official Feed


Official <- within(Official, { officialenergy <- Avail * ENERGY
                               officialprotein <- Avail * PROTEIN})

totofficialenergy <- ddply(Official, .(area, year), 
                        function(x){sum(x$officialenergy)})
totofficialprotein <- ddply(Official, .(area, year), 
                         function(x){sum(x$officialprotein)})

colnames(totofficialenergy) <- c("area", "year", "totofficialenergy")
demand<- merge(demand, totofficialenergy, all.x=T, by=c("area", 'year'))

colnames(totofficialprotein) <- c("area", "year", "totofficialprotein")
demand <- merge(demand, totofficialprotein, all.x=T, by=c("area", 'year'))

demand$totofficialenergy[is.na(demand$totofficialenergy)] <- 0
demand$totofficialprotein[is.na(demand$totofficialprotein)] <- 0

demand <- within(demand, {REDemand <- REDemand - totofficialenergy
                          RPDemand <- RPDemand -  totofficialprotein})


demand$REDemand[demand$REDemand < 0] <- 0
demand$RPDemand[demand$RPDemand < 0] <- 0

demand <- subset(demand, ,c(1:9))
demand[is.na(demand)] <- 0


# 5. Allocate Feed

## merge demand and availability data
dmsp <- merge(demand, Avail, by=c("area", "year"))

## apply availability shares to demand (Residual: after substraction carried out in step 3)
dmsp <- within(dmsp, {efeed <- (eshare * REDemand) / ENERGY 
                      pfeed <- (pshare * RPDemand) / PROTEIN})

## construct indicator measuring if both protein and energy demands are satisfied with current setup
dmsp <- within(dmsp, {pcheck <- efeed * PROTEIN} )

check <- ddply(dmsp, .(area, year), function(x) { sum(x$pcheck) })

colnames(check) <- c("area", "year", "prdm")

dmsp <- merge(dmsp, check, all.x=T)

dmsp <- within(dmsp, { prdmcheck <- ifelse(prdm < RPDemand, "GAP", "MET")})
unbalanced <- subset(dmsp, dmsp$prdmcheck =="GAP")

## Optimize feed allocation under the condition that both energy and protein demands are satisfied
dmsp$pfeed[is.na(dmsp$pfeed)] <- 0
dmsp$efeed[is.na(dmsp$efeed)] <- 0

# create auxiliary funcion which returns all years for a given country
yearlist <- subset(dmsp, ,c(1,2))

yearlist <- unique(yearlist)

years <- function(x) {yearlist$year[yearlist$area==x]}



## Apply optimization function: note Avialability is not a constraint

feed <- data.table()

for(i in unique(yearlist$area))
  for(j in 1:length(yearlist$year[yearlist$area==i]))
    feed <- rbind(feed, optimize(i, years(i)[j] ))


## Check if all conditions are met and  demand is met

finalenergy <- ddply(feed, .(area, year), function(x) {sum(x$finalfeed * x$ENERGY)})
colnames(finalenergy) <- c("area", "year", "finalenergy")
finalprotein <- ddply(feed, .(area, year), function(x) {sum(x$finalfeed * x$PROTEIN)})
colnames(finalprotein) <- c("area", "year", "finalprotein")
feed <- merge(feed, finalenergy, by=c("area", "year"), all.x=T)
feed <- merge(feed, finalprotein, by=c("area", "year"), all.x=T)

feed$finalcheck <- ifelse((feed$REDemand - feed$finalenergy) > 1, "GAP",
                               ifelse((feed$RPDemand - feed$finalprotein) > 1, "GAP", "MET")) 

## Reunite estimated with official and cakes & bran feed data
finalfeed <- subset(feed,, c(1,3,2,10,12,29))
finalfeed <- setnames(finalfeed, 6, "feed")
areaname <- subset(finalfeed, , c(1,2))
areaname <- unique(areaname)
Official <- merge(Official, areaname, all.x=T, by="area")
Official <- subset(Official,, c(1,11,3,2,6,5,4))
Official <- setnames(Official, c(6,7), c("feed", "ObservationFlag"))
Official$MethodFlag <- rep("-", length(Official$area))

finalfeed$ObservationFlag <- rep("E", length(finalfeed$area))
finalfeed$MethodFlag <- rep("e", length(finalfeed$area))

finalfeed <- rbind(finalfeed, Official)


finalfeed <-finalfeed[order(finalfeed$area, finalfeed$year, finalfeed$item),]

cakes <- merge(psupply, areaname, all.x=T, by="area")
cakes <- subset(cakes, , c(1,10,3,2,5,4))
cakes$ObservationFlag <- rep("E", length(cakes$area))
cakes$MethodFlag <- rep("b", length(cakes$area)) 
cakes <- setnames(cakes, 6, "feed")

finalfeed <- rbind(finalfeed, cakes)
finalfeed <-finalfeed[order(finalfeed$area, finalfeed$year, finalfeed$item),]

finalfeed <- subset(finalfeed, !is.na(areaname),)
finalfeed <- merge(finalfeed, feedlist[, c(1,5)], all.x=T, by="item")
finalfeed <- subset(finalfeed, , c(area, areaname, year, item, itemname, feed, ObservationFlag, MethodFlag, feedClassification))
finalfeed <-finalfeed[order(finalfeed$area, finalfeed$year, finalfeed$item),]

# csv output
write.csv(finalfeed,  "feed_6-10.csv", row.names=F)

end.time <- Sys.time()
end.time-start_time
## END