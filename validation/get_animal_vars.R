library(data.table)
library(faosws)
library(faoswsUtil)


files <- list.files("functions", pattern="_factor", full.names = TRUE)
#file <-"functions/buffalo_energy_factor.r"

animal_list <- vector(length(files), mode="list")
names(animal_list) <- gsub("^functions/|\\.[rR]$", "", files)

for(file in files){

text <- getParseData(parse(file, keep.source=TRUE))
row1 <- which(text$text == "vars")[1]-1
if(is.na(row1)) next
id1 <- text[row1,"id"]
parent1 <- text[row1,"parent"]
match_parents <- text[text$parent %in% parent1 ,]
rowid <- which(match_parents$id==id1)
linestoextract <- match_parents[c(rowid, rowid+1),]$line1

vartext <-scan(file, what='character', quote="*", sep="*", skip=(linestoextract[1] - 1), nlines=diff(linestoextract), strip.white=TRUE)
eval(parse(text=paste(vartext, collapse="")))

ele <- sapply(vars, `[[`, 1)
item <- sapply(vars, `[[`, 2)

animal_list[[names(animal_list)[sapply(names(animal_list), grepl,file)]]] <- list(ele=ele, item=item)

}

animal_df <- do.call(rbind,lapply(animal_list, as.data.frame))
animal_df <- animal_df[!duplicated(animal_df),]

GetTestEnvironment("https://hqlprswsas1.hq.un.fao.org:8181/sws", "ebdda55c-21a4-4bdd-9d0c-5098cec843f7")
animal_df$cpc <- fcl2cpc(sprintf("%04d",animal_df$item))

cat(paste(shQuote(unique(animal_df$cpc), type="sh"), collapse=", "))
