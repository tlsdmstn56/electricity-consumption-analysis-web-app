# utility functions
make.summary.df <- function(name){
  # make summary data frame by 'name'
  # 
  # Args:
  #   data: data.frame to be used for making summary data frame
  #   name: name of group to split the summary table
  #
  # Returns:
  #   data.frame of summary table by name\
  factored <- make.column.factor(name, add_label = TRUE)
  tapp <- tapply(KWH, factored, summary)
  tapp=do.call(rbind,tapp)
  return(tapp)
}

get.temp.path <- function(filename){
  return(paste0(TEMP_DIR,"/",filename))
}

isSecondgroupSet <- function(input) {
  # check p1_criterion2 is set
  #
  # Args:
  #   input: server function's input
  #
  # Returns:
  #   logical value: true when p1_criterion2 is set, otherwise false
  return("None"!=input$p1_criterion2 & input$p1_criterion1!=input$p1_criterion2)
}

make.column.factor <- function(group, add_label = FALSE) {
  # get factorized vector whose labels and levels are properly set
  #
  # Args:
  #   group: name of group(column) to be factorized
  #
  # Returns:
  #   vector(factor): factor vector with properly labeld
  if(is.continuous(group)){
    factored <- factor(DATA[[group]])
  } else {
    factor.coded <- names(CODEBOOK_JSON[[group]]$coded)
    factor.label <- unlist(CODEBOOK_JSON[[group]]$coded[factor.coded])
    factored <- factor(DATA[[group]],
                       levels = factor.coded,
                       labels = factor.label)
  }
  return(factored)
}

add.desc.factor <- function(x, desc) {
  if(is.factor(x)) return(factor(x, levels=c(desc, levels(x))))
  return(x)
}

get.desc <- function(group) {
  return(unlist(CODEBOOK[CODEBOOK$name == group, 2]))
}

is.continuous <- function(group) {
  # check given group is continuous
  #
  # Args:
  #   group: name of group(column) to have 
  #          checked if it is continuous
  #
  # Returns:
  #   logical: true if given group is contiuous
  rs <- (CODEBOOK %>% filter(name==group) %>% select(4) %>% as.logical)
  return(rs)
}