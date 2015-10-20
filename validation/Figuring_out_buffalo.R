#Where my buffalo at?
library(faosws)
library(data.table)
library(faoswsUtil)


GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws", "930fe2c6-b2ec-40f0-8763-6752c665a63f")
M49_codes <- GetCodeList("agriculture", "agriculture", "geographicAreaM49")
setkey(M49_codes, code)

raw_buffalo <- GetData(swsContext.datasets[[1]])
swsContext.datasets[[1]]

mean_buffalo <- raw_buffalo[ , .(Value=mean(Value, na.rm=T)),by=c("geographicAreaM49")]
#`addkey<-`(mean_buffalo, geographicAreaM49)

best_buffalo <- M49_codes[mean_buffalo][order(-Value),]
bestest_buffalo  <- unlist(best_buffalo[1:16,"code",with=FALSE])
cat(bestest_buffalo)

vars <- list(c(11, 946), c(41, 947), c(51, 951), c(31, 951), c(31, 947), 
             c(91, 946), c(61, 946))

vars <- list(c(5111, "02112"), # Buffalo stocks, Buffalo 
             c("5417 (54170?)", "21112"), # Buffalo carcass weight (Indigenous?), Buffalo meat
             c(5510,"02212"), # Production (t), Raw buffalo milk
             c(5318, "02212"), # Milk animals (head), Raw buffalo milk
             c(5320, "21112")) # slaughtered/prod animals (head), Buffalo meat

#FROM trade
extvars <- list(c(5900, "02112"), #Exports, buffalo
                c(5600, "02112")) #import, buffalo 
             

fcl2cpc(unique(sprintf("%04d",sapply(vars, `[[`, 2))))


####################################################

evalfun <- function(env, ...){
  dots <- list(...)
  list(energy=do.call("energy", dots, envir = env),
       protein=do.call("protein", dots, envir=env))}

flist <- list(camel = new.env(),
              buffalo = new.env(),
              rabbit = new.env())

camelprotein <- function(arg){
  paste0("camel: ", arg)
}

camelenergy <- function(arg){
  paste0("camel: ", arg)
}

buffaloprotein <- function(arg){
  paste0("buffalo: ", arg)
}

buffaloenergy <- function(arg){
  paste0("buffalo: ", arg)
}


rabbitprotein <- function(arg){
  paste0("rabbit: ", arg)
}

rabbitenergy <- function(arg){
  paste0("rabbit: ", arg)
}

assign("protein", camelprotein, flist$camel)
assign("energy", camelenergy, flist$camel)

assign("protein", buffaloprotein, flist$buffalo)
assign("energy", buffaloenergy, flist$buffalo)

assign("protein", rabbitprotein, flist$rabbit)
assign("energy", rabbitenergy, flist$rabbit)


lapply(flist, evalfun, "moo")


####################################################

GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws", "930fe2c6-b2ec-40f0-8763-6752c665a63f")

CPC_codes <- GetCodeList("agriculture", "agriculture", "measuredItemCPC")[,.(code, description)]
setnames(CPC_codes, c("code", "description"), c("measuredItemCPC", "CPCDescription"))
Element_codes <- rbind(GetCodeList("agriculture", "agriculture", "measuredElement")[,.(code, description)], 
                       GetCodeList("trade", "total_trade_CPC", "measuredElementTrade")[,.(code, description)])
setnames(Element_codes, c("code", "description"), c("measuredElement", "elementDescription"))

code_table <- data.frame(measuredElement = as.character(c(5111, 5417, 54170, 54171, 5510, 5318, 5320, 5900, 5600)),
                         measuredItemCPC = c("02112", "21112", "21112", "21112", "02212", "02212", "21112", "02112", "02112"),
                         description = c("Stocks", "Carcass.Wt","Carcass.Wt.Ind", "Live.Wt.Bio",  "Milk.Production", "Milk.Animals", "Slaughtered", "Exports", "Imports"),
                         stringsAsFactors = FALSE)

CPCmerge <- merge(CPC_codes, code_table, by="measuredItemCPC")
all_codes <- merge(CPCmerge, Element_codes, by="measuredElement")
setcolorder(all_codes, c("measuredItemCPC", "CPCDescription", "measuredElement", "elementDescription", "description"))



####################################################

library(faosws)
library(faoswsUtil)
library(data.table)

if (CheckDebug()) {
  SetClientFiles("~/certificates/production")
  GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws", "ebdda55c-21a4-4bdd-9d0c-5098cec843f7")
}


buffalo_energy_factor <- function(area=na.omit(fs2m49(as.character((1:299)[-22]))), year=1990:2012) {
  
  #  if(length(year) > 1) {
  #   library(plyr)
  #  return(ldply(year, cattle_energy_factor, area = area))
  #}
  
  year <- append(year, last(year) + 1)
  
  
#   vars <- list(c(5111, "02112"), # Buffalo stocks, Buffalo 
#                list(c(5417, 54170, 54171), "21112"), # Buffalo carcass weight (Indigenous?), Buffalo meat
#                c(5510,"02212"), # Production (t), Raw buffalo milk
#                c(5318, "02212"), # Milk animals (head), Raw buffalo milk
#                c(5320, "21112")) # slaughtered/prod animals (head), Buffalo meat
#   
  vars <- data.frame(measuredElement = as.character(c(5111, 5510, 55100, 55101, 5320, 53200, 53201, 5510, 5318, 5320)),
                     measuredItemCPC = c("02112", "21112", "21112", "21112", "21112", "21112", "21112", "02212", "02212", "21112"),
                     stringsAsFactors = FALSE)
  key = DatasetKey(domain = "agriculture", dataset = "aproduction",
                   dimensions = list(
                     Dimension(name = "geographicAreaM49", keys = area), #user input
                     Dimension(name = "measuredItemCPC", keys = vars$measuredItemCPC), # user input
                     Dimension(name = "measuredElement", keys = unique(vars$measuredElement)),
                     Dimension(name = "timePointYears", keys = as.character(year)) #user input
                   )
                     
                   )
  

prod_data <- GetData(key)[,.(geographicAreaM49, measuredItemCPC, measuredElement, timePointYears, Value)]

#   extvars <- list(c(5900, "02112"), #Exports, buffalo
#                   c(5600, "02112")) #import, buffalo 
    
  extvars <- data.frame(measuredElementTrade = as.character(c(5900, 5600)),
                        measuredItemCPC = c("02112", "02112"),
                        stringsAsFactors = FALSE)
  
  
  tradekey <- DatasetKey(domain = "trade", dataset = "total_trade_CPC",
                         dimensions = list(
                           Dimension(name = "geographicAreaM49", keys = c("356", "586", "1248", "524", "818", "608", "104", "704", "360", 
                                                                          "764", "50", "76", "418", "116", "144", "380")), #user input
                           Dimension(name = "measuredItemCPC", keys = extvars[,"measuredItemCPC"]), # user input
                           Dimension(name = "measuredElementTrade", keys = extvars[,"measuredElementTrade"]),
                           Dimension(name = "timePointYears", keys = as.character(2005:2010)))) #user input

  trade_data <- GetData(tradekey)[,.(geographicAreaM49, measuredItemCPC, measuredElementTrade, timePointYears, Value)]
  setnames(trade_data, "measuredElementTrade", "measuredElement")
  
raw_data <- rbind(prod_data, trade_data)


code_table <- data.frame(measuredElement = as.character(c(5111, 5510, 55100, 55101, 5320, 53200, 53201, 5510, 5318, 5900, 5600)),
                   measuredItemCPC = c("02112", "21112", "21112", "21112", "21112", "21112", "21112", "02212", "02212", "02112", "02112"),
                   description = c("Stocks", "Meat.Production", "Meat.Production.Ind", "Meat.Production.Bio", "Slaughtered", "Slaughtered.Ind", "Slaughtered.Bio", "Milk.Production", "Milk.Animals", "Exports", "Imports"),
                   stringsAsFactors = FALSE)

named_data <- merge(raw_data, code_table, by = c("measuredElement", "measuredItemCPC"), all.y = TRUE)

data <- dcast.data.table(named_data, geographicAreaM49 + timePointYears ~ description, value.var = "Value")
#remove any full NA rows
data <- data[!apply(data, 1, function(x) all(is.na(x))),]
# All missing values are to be treated as zero
data[is.na(data)] <- 0
  
  data <- within(data, {
    # Beef animals are all animals which are not dairy
    Beef.Animals <- Stocks - Milk.Animals

    # Change in stocks
    Stocksnext <- c(Stocks[2:length(Stocks)], NA)

    #Live cattle are heavier than carcasses
    liveweight <-   (Meat.Production.Bio + (Meat.Production + Meat.Production.Ind)  / .55) * 1000 / (Slaughtered + Slaughtered.Ind + Slaughtered.Bio)
    milkpercow <- Milk.Production * 1000 / Milk.Animals
    metabolicweight <- liveweight ^ 0.75
    
    weightgain <- (((Slaughtered + Exports - Imports + Stocksnext - Stocks - Stocks * 0.032) 
                    * liveweight) 
                   / Beef.Animals) / 365
    
    weightgain[weightgain < 0] <- 0 
    milkenergy <- (((365 * 0.077 * metabolicweight) + (0.74 * milkpercow)) * 4.184) / 0.6/ 35600
    beefenergy <- (365 * 4.184 * (0.077 * metabolicweight + (0.063 * (0.96*liveweight) ^ 0.75 *                                                          
                                                              weightgain ^ 1.097)))/0.6/35600
    #alternatively
    #beefenergy[weightgain == 0] <- (365*(8.3 + (0.091 * Carcass.Wt[weightgain == 0] * 2)))/35600
    
    energy <- (milkenergy * Milk.Animals + beefenergy * Beef.Animals) / Stocks
  })
  
  data[timePointYears != max(as.numeric(timePointYears)), .(
    geographicAreaM49, timePointYears, energy, Milk.Animals, milkenergy,  Beef.Animals, beefenergy, liveweight, weightgain, milkpercow
  )]
       
  
}

