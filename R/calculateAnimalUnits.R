#' Calculate protein and energy demand of individual animals
#' 
#' The main output of this function is a data.table of Animal Unit Indices
#' (AUIs)
#' 
#' @section Animals:
#' The module currently has equations for the following animals:
#' 
#' \itemize{
#' \item 1. Cattle
#' \item 2. Buffalo
#' \item 3. Sheep
#' \item 4. Goats
#' \item 5. Camels
#' \item 6. Pigs
#' \item 7. Chickens
#' \item 8. Ducks
#' \item 9. Geese
#' \item 10. Turkeys
#' \item 11. Horses
#' \item 12. Rabbits
#' }
#' 
#' @export

calculateAnimalUnits <- function(){

## 2. COMPILE INDICEs

keys <- c("geographicAreaM49", "timePointYears", "measuredItemCPC")

## 1. Cattle

# energy
ce <- cattle_energy_factor() 
ce$measuredItemCPC <- stockCodes["cattle", measuredItemCPC]
setkeyv(ce, keys) 
ce <- ce[, .(geographicAreaM49, timePointYears, measuredItemCPC, energy)]

# protein
cp <- cattle_protein_factor() 
cattle <- merge(ce, cp, all = T)


## 2. Buffalo

#energy
be <- buffalo_energy_factor()
be$measuredItemCPC <- stockCodes["buffalo", measuredItemCPC]
setkeyv(be, keys)
be <- be[,.(geographicAreaM49, timePointYears, measuredItemCPC, energy)]

#protein
bp <- buffalo_protein_factor()
buffalo <- merge(be, bp, all = T)

## 3. Sheep

#energy
se <- sheep_energy_factor()
se$measuredItemCPC <- stockCodes["sheep", measuredItemCPC]
setkeyv(se, keys)
se <- se[, .(geographicAreaM49, timePointYears, measuredItemCPC, energy)]

#protein
sp <- sheep_protein_factor() 
sheep <- merge(se, sp, all = T)

## 4. Goats

#energy
ge <- goat_energy_factor()
ge$measuredItemCPC <- stockCodes["goat", measuredItemCPC]
setkeyv(ge, keys)
ge <- ge[, .(geographicAreaM49, timePointYears, measuredItemCPC, energy)]

#protein
gp <- goat_protein_factor() 
goat <- merge(ge, gp, all = T)

## 5. Camels

#energy
cae <- camel_energy_factor()
cae$measuredItemCPC <- stockCodes["camel", measuredItemCPC]
cae <- cae[, .(geographicAreaM49, timePointYears, measuredItemCPC, energy)]


#protein
cap <- camel_protein_factor()
camel <- merge(cae, cap, all = T)

## 6. Pigs

#energy
pe <- pig_energy_factor()
pe$measuredItemCPC <- stockCodes["pig", measuredItemCPC]
setkeyv(pe, keys)
pe <- pe[,.(geographicAreaM49, timePointYears, measuredItemCPC, energy)]

#protein
pp <- pig_protein_factor() 
pig <- merge(pe, pp, all = T)

## 7. Chickens

#energy
che <- chicken_energy_factor()
che$measuredItemCPC <- stockCodes["chicken", measuredItemCPC]
setkeyv(che, keys)
che <- che[,.(geographicAreaM49, timePointYears, measuredItemCPC, energy)]

#protein
chp <- chicken_protein_factor() 
chicken <- merge(che, chp, all = T)


## 8. Ducks

#energy
de <- duck_energy_factor()
de$measuredItemCPC <- stockCodes["duck", measuredItemCPC]
setkeyv(de, keys)
de <- de[, .(geographicAreaM49, timePointYears, measuredItemCPC, energy)]

#protein
dp <- duck_protein_factor() 
duck <- merge(de, dp, all = T)

## 9. Geese

#energy
goo <- goose_energy_factor()
goo$measuredItemCPC <- stockCodes["goose", measuredItemCPC]
setkeyv(goo, keys)
goo <- goo[, .(geographicAreaM49, timePointYears, measuredItemCPC, energy)]

#protein
gep <- goose_protein_factor() 
goose <- merge(goo, gep, all = T)

## 10. Turkeys

#energy
te <- turkey_energy_factor()
te$measuredItemCPC <- stockCodes["turkey", measuredItemCPC]
setkeyv(te, keys)
te <- te[, .(geographicAreaM49, timePointYears, measuredItemCPC, energy)]

#protein
tp <- turkey_protein_factor() 
turkey <- merge(te, tp, all = T)

## 11. Horses

#energy
he <- horse_energy_factor()
he$measuredItemCPC <- stockCodes["horse", measuredItemCPC]
setkeyv(he, keys)
he <- he[, .(geographicAreaM49, timePointYears, measuredItemCPC, energy)]

#protein
hp <- horse_protein_factor()
horse <- merge(he, hp, all = T)

## 12. Rabbits

#energy
re <- rabbit_energy_factor()
re$measuredItemCPC <- stockCodes["rabbit", measuredItemCPC]
setkeyv(re, keys)
re <- re[, .(geographicAreaM49, timePointYears, measuredItemCPC, energy)]

#protein
rp <- rabbit_protein_factor()
rabbit <- merge(re, rp, all = T)



## 3. COMBINE INDICES

indices <- rbind(cattle, buffalo, sheep, goat, camel, pig, chicken, duck, goose, turkey, horse, rabbit) 

## 4. PREPARE OUTPUT CSV

# format
#indices <- indices[, .(geographicAreaM49, timePointYears, measuredItemCPC, energy, protein)] 

#write
#write.csv(indices, '../Data/trans/aui_6.csv', row.names=F)

indices

}

## End