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
`addkey<-`(mean_buffalo, geographicAreaM49)

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
extvars <- 
             

fcl2cpc(unique(sprintf("%04d",sapply(vars, `[[`, 2))))
