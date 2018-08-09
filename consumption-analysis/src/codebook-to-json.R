library(readr)
library(jsonlite)

codebook <- read_csv("../data/codebook_final.csv")
codebook <- codebook[-(334:335),]
coded.to.list <- function(coded) {
  lines <- unlist(strsplit(x = coded, split="\n"))
  rs.list <- list()
  for (line in lines) {
    elem <- unlist(strsplit(x = line, split="\t"))
    if (grepl("[0-9]+",elem[1])){
      rs.list[elem[1]] <- elem[2]
    } else  {
      rs.list[elem[1]] <- elem[1]
    }
  }
  return(rs.list)
}

coded.list <- lapply(codebook$coded, coded.to.list)

out_list <- list()
for (i in 1:NROW(codebook)) {
  name <- codebook$name[i]
  out_list[[name]] <- list('coded'=coded.list[[i]], 'desc'=codebook$desc[i])
}

# testing
# if $coded is empty list(i.e. list()) it print out its name
for (name in names(out_list)){
    if(is.null(names(out_list[[name]]$coded))) print(name)
}

jsoned <- toJSON(out_list)
write(x=jsoned,file = "../data/codebook_final.json")

