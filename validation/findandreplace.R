## The following example demonstrates
## how a serach and replace string task
## can be peformed with R across several files

## Create two text files with content
# filenames <- c( tempfile(), tempfile() )
# for( f in  filenames ){
#   cat("functions/", file=f)
# }


filenames <- list.files("R", pattern = "factor.r$", full.names = T)
pattern1 <- "slot\\(swsContext\\.datasets\\[\\[1\\]\\]@dimensions\\$([a-zA-Z0-9]+), \"keys\"\\)"
pattern2 <- "getQueryKey\\(\"\\1\"\\)"


## Replace one pattern with another
for (f in filenames) {
  x <- readLines(f)
  y <- gsub(pattern1, pattern2, x , perl = T)
  cat(y, file = f, sep = "\n")
  
}

## Review output
for (f in filenames) {
  cat(readLines(f), sep = "\n")
}