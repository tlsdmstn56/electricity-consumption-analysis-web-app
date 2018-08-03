library(readr)
library(dplyr)
library(xlsx)
### codebook working
codebook = readxl::read_xlsx("./data/codebook2015_public_v3.xlsx",skip = 3)
codebook = data.frame(name=codebook$`SAS Variable Name`,
                      desc=codebook$`Variable Description`,
                      coded=codebook$X__1,
                      cat=codebook$`Final Response Set`
                      )
cat.table=rep(0,NROW(codebook))
for(i in seq(1:NROW(codebook))) {
  code = strsplit(as.character(codebook[i,3]),"\r\n")
  cat = strsplit(as.character(codebook[i,4]),"\r\n")
  rs=""
  for(j in 1:NROW(cat[[1]])){
    rs = paste(rs,code[[1]][j],"\t",cat[[1]][j],sep="",end="\n")
  }
  cat.table[i]=rs
}
codebook_final = data.frame(name=unlist(codebook$name),
                            desc=unlist(codebook$desc),
                            coded=cat.table)
write.csv(codebook_final,"./data/codebook_final.csv",row.names=FALSE)

## data cleasing - data ## --
data = read_csv("./data/recs2015_public_v3.csv",locale=locale(tz="US/Pacific"))
drop.column.re<-function(data,pattern,ask=FALSE){
  test = data[, -grep(pattern, colnames(data))]
  if(!ask) return(test)
  change = paste(dim(data)[2],"->",dim(test)[2],sep=" ")
  res <- readline(prompt=paste(change,"Confirm[y/n]: ",sep="\n"))
  if(res=="y") return(test)
  else return(data)
}
data = drop.column.re(data,"CUFEET")
data = drop.column.re(data,"BRRWT")
data = drop.column.re(data,"BTUNG")
data = drop.column.re(data,"GALLON")
data = drop.column.re(data,"DOEID")
data = drop.column.re(data,"REGIONC")
data = drop.column.re(data,"^Z+")
data = drop.column.re(data,"TOTU")
data = drop.column.re(data,"NWEIGHT")
data = drop.column.re(data,"CDD",ask=F)
data = drop.column.re(data,"HDD",ask=F)
data = drop.column.re(data,"DOLNG")
data = drop.column.re(data,"BTULP")
data = drop.column.re(data,"DOLLP")
data = drop.column.re(data,"BTUFO")
data = drop.column.re(data,"DOLFO")
data = drop.column.re(data,"TOTAL")
data = drop.column.re(data,"FUEL")
data = drop.column.re(data,"^TOT*")


write.csv(data,"./data/recs2015_clean.csv",row.names = FALSE)


### make summary table
make.summary.df<-function(data,name){
  tapp = tapply(data[['KWH']], as.factor(data[[name]]), summary)
  tapp=do.call(rbind,tapp)
}

plot(1:3,1:3,col=1:3)
