calculateAnimalUnits <- function(){

## 2. COMPILE INDICEs

keys <- c("geographicAreaM49", "timePointYears", "measuredItemCPC")

## 2.1 Cattle

# energy
ce <- cattle_energy_factor() 
ce$measuredItemCPC <- "02111"
setkeyv(ce, keys) 
ce <- ce[, .(geographicAreaM49, timePointYears, measuredItemCPC, energy)]

# protein
cp <- cattle_protein_factor() 
cattle <- merge(ce, cp, all = T)


## 2.2 Buffaloes

#energy
be <- buffalo_energy_factor()
be$measuredItemCPC <- "02112"
setkeyv(be, keys)
be <- be[,.(geographicAreaM49, timePointYears, measuredItemCPC, energy)]

#protein
bp <- buffalo_protein_factor()
buffalo <- merge(be, bp, all = T)

## 2.3 Sheep

#energy
se <- sheep_energy_factor()
se$measuredItemCPC <- "02122"
setkeyv(se, keys)
se <- se[, .(geographicAreaM49, timePointYears, measuredItemCPC, energy)]

#protein
sp <- sheep_protein_factor() 
sheep <- merge(se, sp, all = T)

## 2.4 Goats

#energy
ge <- goat_energy_factor()
ge$measuredItemCPC <- "02123"
setkeyv(ge, keys)
ge <- ge[, .(geographicAreaM49, timePointYears, measuredItemCPC, energy)]

#protein
gp <- goat_protein_factor() 
goat <- merge(ge, gp, all = T)

## 2.5 Camels

#energy
cae <- camel_energy_factor()
cae$measuredItemCPC <- "02121.01"
cae <- cae[, .(geographicAreaM49, timePointYears, measuredItemCPC, energy)]


#protein
cap <- camel_protein_factor()
camel <- merge(cae, cap, all = T)

## 2.6 Pigs

#energy
pe <- pig_energy_factor()
pe$measuredItemCPC <- "02140"
setkeyv(pe, keys)
pe <- pe[,.(geographicAreaM49, timePointYears, measuredItemCPC, energy)]

#protein
pp <- pig_protein_factor() 
pig <- merge(pe, pp, all = T)

## 2.7 Chickens

#energy
che <- chicken_energy_factor()
che$measuredItemCPC <- "02151" 
setkeyv(che, keys)
che <- che[,.(geographicAreaM49, timePointYears, measuredItemCPC, energy)]

#protein
chp <- chicken_protein_factor() 
chicken <- merge(che, chp, all = T)


## 2.8 Ducks

#energy
de <- duck_energy_factor()
de$measuredItemCPC <- "02154"
setkeyv(de, keys)
de <- de[, .(geographicAreaM49, timePointYears, measuredItemCPC, energy)]

#protein
dp <- duck_protein_factor() 
duck <- merge(de, dp, all = T)

## 2.9 Geese

#energy
goo <- goose_energy_factor()
goo$measuredItemCPC <- "02153"
setkeyv(goo, keys)
goo <- goo[, .(geographicAreaM49, timePointYears, measuredItemCPC, energy)]

#protein
gep <- goose_protein_factor() 
goose <- merge(goo, gep, all = T)

## 2.10 For Turkeys

#energy
te <- turkey_energy_factor()
te$measuredItemCPC <- "02152"
setkeyv(te, keys)
te <- te[, .(geographicAreaM49, timePointYears, measuredItemCPC, energy)]

#protein
tp <- turkey_protein_factor() 
turkey <- merge(te, tp, all = T)


## 3. COMBINE INDICES

indices <- rbind(cattle, buffalo, sheep, goat, camel, pig, chicken, duck, goose, turkey) 

## 4. PREPARE OUTPUT CSV

# format
#indices <- indices[, .(geographicAreaM49, timePointYears, measuredItemCPC, energy, protein)] 

#write
#write.csv(indices, '../Data/trans/aui_6.csv', row.names=F)

indices

}

## End