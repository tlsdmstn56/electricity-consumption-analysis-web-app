library(readr)
library(dplyr)

# codebook cleasing
codebook <- readxl::read_xlsx("./data/codebook2015_public_v3.xlsx",skip = 3)

codebook <- data.frame(name=codebook$`SAS Variable Name`,
                      desc=codebook$`Variable Description`,
                      coded=codebook$X__1,
                      cat=codebook$`Final Response Set`
                      )
cat.table <- rep(0,NROW(codebook))
for(i in seq(1:NROW(codebook))) {
  code <- strsplit(as.character(codebook[i,3]),"\r\n")
  cat <- strsplit(as.character(codebook[i,4]),"\r\n")
  rs <- ""
  for(j in 1:NROW(cat[[1]])){
    rs <- paste(rs,code[[1]][j],"\t",cat[[1]][j],sep="",end="\n")
  }
  cat.table[i]=rs
}
codebook_final <- data.frame(name=unlist(codebook$name),
                            desc=unlist(codebook$desc),
                            coded=unlist(cat.table))

drop.codebook.row <- function(data, pattern, print_result=FALSE) {
  test <- data[-grep(pattern, unlist(data$name)), ]
  if (print_result) {
    cat(NROW(data),"-->",NROW(test),end="\n")
  }
  return(test)
}

codebook_final <- drop.codebook.row(codebook_final,"CUFEET")
codebook_final <- drop.codebook.row(codebook_final,"BRRWT")
codebook_final <- drop.codebook.row(codebook_final,"BTUNG")
codebook_final <- drop.codebook.row(codebook_final,"GALLON")
codebook_final <- drop.codebook.row(codebook_final,"DOEID")
codebook_final <- drop.codebook.row(codebook_final,"REGIONC")
codebook_final <- drop.codebook.row(codebook_final,"^Z+")
codebook_final <- drop.codebook.row(codebook_final,"TOTU")
codebook_final <- drop.codebook.row(codebook_final,"NWEIGHT")
codebook_final <- drop.codebook.row(codebook_final,"CDD")
codebook_final <- drop.codebook.row(codebook_final,"HDD")
codebook_final <- drop.codebook.row(codebook_final,"DOLNG")
codebook_final <- drop.codebook.row(codebook_final,"BTULP")
codebook_final <- drop.codebook.row(codebook_final,"DOLLP")
codebook_final <- drop.codebook.row(codebook_final,"BTUFO")
codebook_final <- drop.codebook.row(codebook_final,"DOLFO")
codebook_final <- drop.codebook.row(codebook_final,"TOTAL")
codebook_final <- drop.codebook.row(codebook_final,"FUEL")
codebook_final <- drop.codebook.row(codebook_final,"^TOT*")

write.csv(codebook_final,"./data/codebook_final.csv",row.names=FALSE)

# data cleasing
data <- read_csv("./data/recs2015_public_v3.csv", locale=locale(tz="US/Pacific"))
drop.column.re <- function(data, pattern, ask=FALSE){
  # iteractively drop column
  test <- data[, -grep(pattern, colnames(data))]
  if(!ask){
    return(test)
  } 
  # checking how many column will be dropped
  change <- paste(dim(data)[2],"->",dim(test)[2],sep=" ")
  res <- readline(prompt=paste(change,"Confirm[y/n]: ",sep="\n"))
  if(res=="y"){
    return(test)
  } else {
    return(data)
  }
}

data <- drop.column.re(data,"CUFEET")
data <- drop.column.re(data,"BRRWT")
data <- drop.column.re(data,"BTUNG")
data <- drop.column.re(data,"GALLON")
data <- drop.column.re(data,"DOEID")
data <- drop.column.re(data,"REGIONC")
data <- drop.column.re(data,"^Z+")
data <- drop.column.re(data,"TOTU")
data <- drop.column.re(data,"NWEIGHT")
data <- drop.column.re(data,"CDD",ask=F)
data <- drop.column.re(data,"HDD",ask=F)
data <- drop.column.re(data,"DOLNG")
data <- drop.column.re(data,"BTULP")
data <- drop.column.re(data,"DOLLP")
data <- drop.column.re(data,"BTUFO")
data <- drop.column.re(data,"DOLFO")
data <- drop.column.re(data,"TOTAL")
data <- drop.column.re(data,"FUEL")
data <- drop.column.re(data,"^TOT*")

write.csv(data,"./data/recs2015_clean.csv",row.names = FALSE)