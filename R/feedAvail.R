
# This function provides feed availabilities. Using the equation:
# Production - Exports + Imports + Stockvariation = Food + Seed + Other Uses + Waste + Processed
# usage of single summands depends on user input. Data is downloaded from FAOSTAT sws


feedAvail <- function(area, year, feeditem, stv=F, food=T, seed=F, other=F, waste=F, processed=T  ) {

    feed <- sws_query(area=area, item=feeditem, ele=101, year=year, value.names=F)
    #feed$flag <- NULL
    feed$ele <- NULL
    colnames(feed) <- c("area", "item", "year", "feed", "flag")  
  
    production <- sws_query(area=area, item=feeditem, ele=51, year=year, value.names=F)
    production$flag <- NULL
    production$ele <- NULL
    colnames(production) <- c("area", "item", "year", "production")

    imports <- sws_query(area=area, item=feeditem, ele=61, year=year, value.names=F)
    imports$flag <- NULL
    imports$ele <- NULL
    colnames(imports) <- c("area", "item", "year", "imports")

    exports <- sws_query(area=area, item=feeditem, ele=91, year=year, value.names=F)
    exports$flag <- NULL
    exports$ele <- NULL
    colnames(exports) <- c("area", "item", "year", "exports")

    processed <- sws_query(area=area, item=feeditem, ele=131, year=year, value.names=F)
    processed$flag <- NULL
    processed$ele <- NULL
    colnames(processed) <- c("area", "item", "year", "processed")
    
if(stv==T){
    stockvar <- sws_query(area=area, item=feeditem, ele=71, year=year, value.names=F)
    stockvar$flag <- NULL
    stockvar$ele <- NULL
     
    
} else {stockvar <- data.frame(feed$area, feed$item, feed$year, 0)}

        colnames(stockvar) <- c("area", "item", "year", "stockvar")
    
if(food==T){
    food <- sws_query(area=area, item=feeditem, ele=141, year=year, value.names=F)
    food$flag <- NULL
    food$ele <- NULL
   
} else {food <- data.frame(feed$area, feed$item, feed$year, 0)}
      
colnames(food) <- c("area", "item", "year", "food")

if(seed==T){
    seed <- sws_query(area=area, item=feeditem, ele=111, year=year, value.names=F)
    seed$flag <- NULL
    seed$ele <- NULL
    
} else {seed <- data.frame(feed$area, feed$item, feed$year, 0)}

        colnames(seed) <- c("area", "item", "year", "seed")

if(other==T){
    other <- sws_query(area=area, item=feeditem, ele=151, year=year, value.names=F)
    other$flag <- NULL
    other$ele <- NULL
    
}  else {other <- data.frame(feed$area, feed$item, feed$year, 0)}  

      colnames(other) <- c("area", "item", "year", "other")

if(waste==T){
    waste <- sws_query(area=area, item=feeditem, ele=121, year=year, value.names=F)
    waste$flag <- NULL
    waste$ele <- NULL
    
}else {waste <- data.frame(feed$area, feed$item, feed$year, 0)}

      colnames(waste) <- c("area", "item", "year", "waste")

### 1.2 Combine data
    Avail <- merge(feed, production, all.x=T)
    Avail <- merge(Avail, imports, all =T)
    Avail <- merge(Avail, exports, all.x=T)
    Avail <- merge(Avail, processed, all.x=T)
    Avail <- merge(Avail, stockvar, all=T)
    Avail <- merge(Avail, food, all.x=T)
    Avail <- merge(Avail, seed, all.x=T)
    Avail <- merge(Avail, other, all.x=T)
    Avail <- merge(Avail, waste, all.x=T)
Avail <- Avail[!is.na(Avail$feed),]


#### Get rid of NAs
    Avail[is.na(Avail)] <- 0

### 1.3 Calculate Availability
    Avail <- within(Avail, 
                Avail <- {production + imports - exports - 
                            processed - food - seed - other - waste
                })
checkfeed <- ddply(Avail, .(area, item), function(z) {
                    sum(z$feed)

                    
                    })

colnames(checkfeed) <- c("area", "item", "cfeed")
checkfeed <- within(checkfeed, cfeed <- ifelse(cfeed == 0, 0, 1))

Avail <- merge(Avail, checkfeed, all.x=T)
Avail <- Avail[!Avail$cfeed == 0,]
Avail <- Avail[order(Avail$area, Avail$year),]

Avail <- within(Avail, { Avail <- ifelse(Avail <= 0, feed, Avail)
                         Avail <- ifelse(flag == " ", feed, Avail)})

Avail[,c(1,3,2,5, 15)]

}



