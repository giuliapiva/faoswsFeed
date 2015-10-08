library(faosws)
library(data.table)
library(faoswsUtil)

GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws", "ebdda55c-21a4-4bdd-9d0c-5098cec843f7")

#Read in results from old and new system
oldfile <- data.table(read.csv("oldfeedDemand.csv"))
newfile <- data.table(read.csv("newfeedDemand.csv"))

newfile[,geographicAreaM49 := as.character(geographicAreaM49)]
setnames(newfile, c("energyDemand", "proteinDemand"), c("newEnergy", "newProtein"))
setkey(newfile, geographicAreaM49, timePointYears)

oldfile[,geographicAreaFCL := fs2m49(as.character(geographicAreaFCL))]
setnames(oldfile, c("energyDemand", "proteinDemand"), c("oldEnergy", "oldProtein"))
setkey(oldfile, geographicAreaFCL, timePointYears)

mergetest <- newfile[oldfile]
mergetest <- mergetest[,.(geographicAreaM49, timePointYears, oldEnergy, newEnergy, oldProtein, newProtein)]

keytranslations <- unique(mergetest$geographicAreaM49)
countrycodes <- GetCodeList("agriculture", "agriculture", "geographicAreaM49", keytranslations[!keytranslations %in% "532"])[,.(code, description)]
setkey(countrycodes, code)
mergetest <- countrycodes[mergetest]


oldmerge <- mergetest[, .(oldEnergy, oldProtein)]
newmerge <- mergetest[,.(newEnergy, newProtein)]

#which rows are different?
diffrows <- unique(which(newmerge != oldmerge, arr.ind=T)[,1])
#which rows are very different?
bigdiffs <-unique(which(abs(oldmerge - newmerge) > (0.01 * oldmerge), arr.ind=T)[,1])

mergetest[diffrows,]
bigdiffs <- mergetest[bigdiffs,]

write.csv(bigdiffs, "bigdiffs.csv", row.names = F)
