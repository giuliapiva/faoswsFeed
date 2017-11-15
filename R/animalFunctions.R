#' Animal Functions
#' 
#' Functions mostly used in animal functions. These are for retrieving data from
#' the SWS.
#' 
#' @rdname animal-functions
#'   
#' @param animal character of animal as listed in \code{\link{codeTable}}.
#' @param func character. Whether for protein or energy function - All of the
#'   form {animal}_{energy|protein}_factor
#' @param area character. M49 area codes
#' @param year character. Years
#' 
#' @importFrom faoswsUtil fcl2cpc cpc2fcl fs2m49 m492fs

getProdData <- function(animal, func, area, year){
  
  prodCodes <- codeTable[module == animal & fun == func & table == "production", ]
  
  
  prodKey = DatasetKey(domain = "agriculture", dataset = "aproduction",
                       dimensions = list(
                         Dimension(name = "geographicAreaM49", keys = area), #user input
                         Dimension(name = "measuredItemCPC", keys = unique(prodCodes$measuredItemCPC)),
                         Dimension(name = "measuredElement", keys = unique(prodCodes$measuredElement)),
                         Dimension(name = "timePointYears", keys = year) #user input
                       ),
                       sessionId =  slot(swsContext.datasets[[1]], "sessionId")
  )
  
  prodData <- removeMissingFlags(GetData(prodKey, flags = TRUE))
  prodData[,`:=`(flagObservationStatus = NULL,
                 flagMethod = NULL)]
  prodData
}

getTradeData <- function(animal, func, area, year) {
  
  tradeCodes <-  codeTable[module == animal & fun == func & table == "trade",]
  
  getSplitTrade(area, tradeCodes$measuredItemCPC, tradeCodes$measuredElement, tradeCodes$measuredElementFS, year)
  
}

getSplitTrade <- function(area, item, element, fs_element, year){
  
  # Read data from FAOSTAT
  if(length(getYearCodes("faostat", year))){
    
    fsTradeItems <- na.omit(sub("^0+", "", cpc2fcl(unique(item),
                                                   version  = "latest",
                                                   returnFirst = TRUE)))
    if(!is.null(attr(fsTradeItems, "na.action"))){
      warning("Some items were omitted converting from cpc to fcl for trade data")
    }
    
    fsTradeKey = DatasetKey(
      domain = "faostat_one", dataset = "FS1_SUA",
      dimensions = list(
        #user input except curacao,  saint martin and former germany
        Dimension(name = "geographicAreaFS", keys = setdiff(m492fs(area), c("279", "534", "280"))), 
        Dimension(name = "measuredItemFS", keys = fsTradeItems),
        Dimension(name = "measuredElementFS", keys = na.omit(unique(fs_element))),
        Dimension(name = "timePointYears", keys = getYearCodes("faostat", year)) #user input
      ),
      sessionId =  slot(swsContext.datasets[[1]], "sessionId")
    )
    
    fsTradeData <- GetData(fsTradeKey, flags = FALSE)
    
  } else {
    fsTradeData <- makeEmptyDataset("faostat_one", "FS1_SUA")
  }
  
  #convert codes back to fs and cpc
  fsTradeData[, `:=`(geographicAreaFS = fs2m49(geographicAreaFS),
                     measuredItemFS = fcl2cpc(sprintf("%04d", as.numeric(measuredItemFS))))]
  fsTradeData <- fsTradeData[unique(data.table(measuredElementFS = fs_element, measuredElement = element)), 
                             on = "measuredElementFS"]
  fsTradeData[, measuredElementFS := NULL]
  
  setnames(fsTradeData, c("geographicAreaFS", "measuredItemFS"),
           c("geographicAreaM49", "measuredItemCPC"))
  
  
  # Prepare any trade module data
  if(length(getYearCodes("trade_module", year))){
    moduleTradeKey <- DatasetKey(domain = "trade",
                                 dataset = "total_trade_cpc_m49",
                                 dimensions = list(
                                   Dimension(name = "geographicAreaM49", keys = area), 
                                   Dimension(name = "measuredItemCPC", keys = unique(item)),
                                   Dimension(name = "measuredElementTrade", keys = na.omit(unique(element))),
                                   Dimension(name = "timePointYears", keys = getYearCodes("trade_module", year)) #user input
                                 )
    )
    
    moduleTradeData <- GetData(moduleTradeKey, flags = FALSE)
    
  } else {
    moduleTradeData <- makeEmptyDataset("trade", "total_trade_cpc_m49")
  }
  
  setnames(moduleTradeData, "measuredElementTrade", "measuredElement")
  
  tradeData <- rbind(moduleTradeData, fsTradeData)
  
  setcolorder(tradeData, c("geographicAreaM49", "measuredElement", "measuredItemCPC", "timePointYears", "Value"))
  
  return(tradeData[])
  
}

removeMissingFlags <- function(data, delete = FALSE, observationFlag = "flagObservationStatus", 
                               methodFlag = "flagMethod", missingCode = "M"){
  workingdata <- copy(data)
  workingdata <- workingdata[get(observationFlag) != missingCode,]
  
  if(delete){
    # There may not be a method flag
    to_delete <- setdiff(names(workingdata), c(observationFlag, methodFlag))
    workingdata <- workingdata[,to_delete, with = FALSE]
  }
  
  return(workingdata[])
  
}
