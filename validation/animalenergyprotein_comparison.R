library(data.table)
library(faosws)
library(faoswsUtil)

GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws", "ebdda55c-21a4-4bdd-9d0c-5098cec843f7")


old_animalunit <- data.table(read.csv("../Data/trans/animal_unit_6-12.csv")[,-1])
new_animalunit <- data.table(read.csv("../Data/trans/aui_6.csv"))
setnames(old_animalunit, c("Energy.Factor",  "Protein.Factor"), c("oldEnergy", "oldProtein"))
setnames(new_animalunit, c("Energy.Factor",  "Protein.Factor"), c("newEnergy", "newProtein"))

setkey(old_animalunit, Area.Code, Item.Code, Year)
setkey(new_animalunit, Area.Code, Item.Code, Year)

mergetable <- old_animalunit[new_animalunit]

oldmerge <- mergetable[, .(oldEnergy, oldProtein)]
newmerge <- mergetable[,.(newEnergy, newProtein)]

#which rows are different?
diffrows <- unique(which(newmerge != oldmerge, arr.ind=T)[,1])
#which rows are very different?
bigdiffs <-unique(which(abs(oldmerge - newmerge) > (0.01 * oldmerge), arr.ind=T)[,1])

mergetable[diffrows,]
bigdiffs <- mergetable[bigdiffs,]

##Which codes are frequent?
counts <- data.table(code=names(table(bigdiffs$Item.Code)), count=table(bigdiffs$Item.Code), key="code")
descriptions <- GetCodeList("faostat_one", "FS1_SUA_UPD", "measuredItemFS", unique(bigdiffs$Item.Code))
counts[descriptions[,.(code,description)]]
#1126




#"Camels"


## Make a linear model to find where the differences are coming from
lmdiffs <- copy(bigdiffs)
lmdiffs[, `:=`(energyDiff=oldEnergy / newEnergy, 
               proteinDiff=oldProtein / newProtein)]

energylm <- lm(energyDiff~as.factor(Area.Code)*as.factor(Item.Code)+Year, data=lmdiffs)
coefen<-coef(energylm)[!is.na(coef(energylm))]
names(sort(coefen, decreasing=TRUE))[1:10]
