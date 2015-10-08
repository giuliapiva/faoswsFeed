GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws", "ebdda55c-21a4-4bdd-9d0c-5098cec843f7")

fscodes <- GetCodeList("faostat_one", "FS1_SUA", "geographicAreaFS")
m49codes <- GetCodeList("feed", "feed_availability", "geographicAreaM49")

convtable <- faosws::GetTableData(schemaName = "ess", tableName = "fal_2_m49")

setkey(fscodes, code)
setkey(m49codes, code)
setkey(convtable, fal)

fs_conv <- fscodes[convtable]
setkey(fs_conv, m49)

mergetable <- fs_conv[m49codes]

check <- function(obj, start=1){
  for(i in start:nrow(obj)){
   response <- readline(paste0(i, ": ", obj[i, 1, with=FALSE], "|", obj[i, 2, with=FALSE]))
   if(response != "") {stop("They aren't the same, according to you. Stopping at ", i)}
  }
}

#check(mergetable[description != i.description,c("description", "i.description"), with=FALSE], 54)

# fs_merge <- fscodes[convtable, all=TRUE]
# fs_merge[is.na(description),]
# 
# setkey(convtable, m49)
# m49_merge <- m49codes[convtable, all=TRUE]
# m49_merge[is.na(description),]
# 

fs_conv_keys <- convtable$fal
m49_conv_keys <- convtable$m49

fs_all_keys <- fscodes$code
m49_all_keys <- m49codes$code

fs_diffs <- setdiff(fs_conv_keys, fs_all_keys)
m49_diffs <- setdiff(m49_conv_keys, m49_all_keys)

setkey(convtable, fal)
fscodes[convtable[m49 %in% m49_diffs]]

setkey(convtable, m49)
m49codes[convtable[fal %in% fs_diffs]]

## CURRENT PROBLEMS
# fs:22 - m49:532; Aruba ~ 532 is not a valid code, 533 is.
# fs:214 - m49:158; China, Taiwan Prov ~ Maps to nowhere. Maybe 156 China?
# 
# fs:281 - m49:663; USA, Summer Season/Saint-Martin ~ Names don't match
# #
# # These fs codes don't have matches in the fs codes
# #
#   code  description selectionOnly    type startDate endDate fal
# 1:  531     CuraÃ§ao         FALSE country        NA      NA 279
# 2:  534 Sint Maarten         FALSE country        NA      NA 280
# 3:  831     Guernsey         FALSE country        NA      NA 274
# 4:  832       Jersey         FALSE country        NA      NA 283




