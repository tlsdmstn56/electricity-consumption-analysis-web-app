require(readr)
setwd("/home/es-shiny/electricity-consumption-analysis-web-app/consumption-analysis/src")
data <- read_csv("../data/recs2015_clean.csv", locale=locale(tz="US/Pacific"))
codebook <- read_csv("../data/codebook_final.csv",na = c("",""))
CODEBOOK_JSON <- fromJSON("../data/codebook_final.json")

is.continuous <- function(coded) {
  lines <- unlist(strsplit(x = coded, split="\n"))
  elem <- unlist(strsplit(x = lines[1], split="\t"))
  if (grepl("^[0-9]+ - [0-9]+", elem[1])) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

test <- sapply(codebook$coded,is.continuous)
names(test) <- NULL
codebook['isContinuous'] <- test
codebook <- codebook[,-which('X1' %in% colnames(codebook))]
write.csv(codebook,"../data/codebook_final_with_type.csv",row.names = FALSE)
