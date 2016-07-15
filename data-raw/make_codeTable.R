library(faosws)
library(faoswsUtil) # 0.2.12
library(data.table)

GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws", "930fe2c6-b2ec-40f0-8763-6752c665a63f")

codeTable <- data.table(read.csv("data-raw/codeTable.csv", colClasses = "character", stringsAsFactors = FALSE))
setkey(codeTable, measuredItemCPC, measuredElement)

#Get cpc codes to merge
cpcCodes <- GetCodeList("agriculture", "aproduction", "measuredItemCPC")[,.(code, description)]
setnames(cpcCodes, c("code", "description"), c("measuredItemCPC", "CPCDescription"))

#Get element codes to merge (from production and trade)
## WARNING: Making the false assumption that there is no overlap in codes (for simplicity, but not to hard to change) which seems to work
elementCodes <- rbindlist(list(GetCodeList("agriculture", "aproduction", "measuredElement", 
                                  unique(codeTable[table == "production", measuredElement]))[,.(code, description)], 
                       GetCodeList("trade", "total_trade_cpc_m49", "measuredElementTrade", 
                                   unique(codeTable[table == "trade", measuredElement]))[,.(code, description)]
                      ))
setnames(elementCodes, c("code", "description"), c("measuredElement", "elementDescription"))

cpcMerge <- merge(cpcCodes, codeTable, by = "measuredItemCPC")
allCodes <- merge(cpcMerge, elementCodes, by = "measuredElement")

#Add FS codes
allCodes[,measuredItemFS := cpc2fcl(measuredItemCPC, version = "latest")]

setcolorder(allCodes, c("module", "fun", "table", "measuredItemFS", "measuredItemCPC", "CPCDescription", "measuredElementFS", "measuredElement", "elementDescription", "variable"))
setkey(allCodes, measuredItemCPC, measuredElement)

codeTable <- allCodes

devtools::use_data(codeTable, overwrite = TRUE)