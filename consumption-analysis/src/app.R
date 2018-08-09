load.package <- function(package){
  if(!require(package,character.only = TRUE )){
    install.packages(package)
    library(package,character.only = TRUE)
  } 
}

# ibrary import
load.package("shiny")
load.package("DT")
load.package("readr")
load.package("dplyr")
load.package("jsonlite")
load.package("ggplot2")
#library(plotly)

# Data Import
DATA <- read_csv("../data/recs2015_clean.csv", locale=locale(tz="US/Pacific"))
CODEBOOK <- read_csv("../data/codebook_final_with_type.csv",na = c("",""))
CODEBOOK_JSON <- fromJSON("../data/codebook_final.json")
# Constants
KWH_COL_IDX <- grep("^KWH*",unlist(colnames(DATA)))
KWH_COL_IDX <- KWH_COL_IDX[2:length(KWH_COL_IDX)]
USAGE_DESC <- c('space hitting','AC(central, individual)','water heating',
                'all refrigerators','first refrigerators','second refrigerators',
                'freezers','cooking(stove, cooktop, oven)','microwave',
                'clothes washer','clothes dryer','dishwashers',
                'lighting(indor, outdoor)','all TV','first tv','second tv',
                'air handler for heating','air handler for cooling',
                'evaporative cooler','ceiling fan','dehumidifiers','humidifiers',
                'swimming pool pump','hot tub pumps','hot tub heaters','etc')
USAGE_DESC_TABLE <- data.frame(name=colnames(DATA)[KWH_COL_IDX])
COLNAMES_IN_DROPDOWN <- names(DATA)
DROPDOWN_MENU <- sort(COLNAMES_IN_DROPDOWN[1:242])
TEMP_DIR <- tempdir()

options(device = function() pdf(file = paste0(TEMP_DIR, "Rplots.pdf")))

# Define UI for application
#
# titlePanel
# textOutPut(description pages)
# navibar
#   tabPanel(Total Consumption)
#     [sidebarLayout]
#      sidebarPanel(two input dropdowns)
#      mainPanel
#       [tabsetPanel]
#        tabPanel(Plot)
#        tabPanel(Summary Table)
#        tabPanel(Anova Test)
#   tabPanel(Consumption Usage)
#     [sidebarLayout]
#      sidebarPanel(one input dropdowns)
#      mainPanel
#       [tabsetPanel]
#        tabPanel(Box Plot)
#        tabPanel(Bar Chart)
#        tabPanel(Summary Table)
#   tabPanel(Codebook)
ui <- fluidPage(
  # title
  titlePanel("Consumption Analysis", windowTitle="Consumption Analysis"),
  # guide(frequent used variables, data source, some infos)
  htmlOutput('guide'),
  # Navigation Bar on the top
  navbarPage("Menu",
             # Total Consumption Tab
             tabPanel("Total Consumption",
                      sidebarLayout(
                        sidebarPanel(
                          # first group
                          selectInput("p1_criterion1", 
                                      "First Criterion",
                                      DROPDOWN_MENU),
                          # second group
                          selectInput("p1_criterion2", 
                                      "Second Criterion",
                                      c("None",DROPDOWN_MENU)),
                          # group description
                          htmlOutput("p1_var_desc")),
                        # plots and summary, anova
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Plot", 
                                     br(),
                                     tags$p("Press Download Button for bigger image"),
                                     downloadButton("p1_download_boxplot", "Download"), 
                                     br(),
                                     plotOutput("p1_boxplot")),
                            tabPanel("Summary",
                                     verbatimTextOutput("p1_summary")),
                            tabPanel("Anova Test", 
                                     verbatimTextOutput("p1_aov"))
                          )) # end of main panel
                      )),
             # Consumption Usage tab
             tabPanel("Consumption Usage",
                      sidebarLayout(
                        # for group selection
                        sidebarPanel(
                          selectInput("p2_criterion", "Criterion",
                                      sort(COLNAMES_IN_DROPDOWN[1:242])),
                          verbatimTextOutput("p2_var_desc")),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Box Plot",
                                     br(),
                                     tags$p("Press Download Button for bigger image"),
                                     tags$div(
                                       downloadButton("p2_download_boxplot_split", 
                                                      "Download(file by plots, .zip)"), 
                                       downloadButton("p2_download_boxplot_combined", 
                                                      "Download(combined to one file, .jpeg)")
                                     ),
                                     br(),
                                     plotOutput("p2_boxplot", height=4000, width=1000)),
                            tabPanel("Bar Chart",
                                     br(),
                                     tags$p("Press Download Button for bigger image"),
                                     tags$div(
                                       downloadButton("p2_download_barplot_split", 
                                                      "Download(file by plots, .zip)"), 
                                       downloadButton("p2_download_barplot_combined", 
                                                      "Download(combined to one file, .jpeg)")
                                     ),
                                     br(),
                                     plotOutput("p2_barplot", height=3000, width=1000)),
                            tabPanel("Summary Table", 
                                     htmlOutput("p2_summary_header_avg"),
                                     br(),
                                     tags$div(
                                       downloadButton("p2_download_summary_avg", 
                                                      "Download(average, .csv)")
                                     ),
                                     br(),
                                     tableOutput("p2_summary_avg"),
                                     
                                     br(),
                                     htmlOutput("p2_summary_header_percentage"),
                                     br(),
                                     tags$div(
                                       downloadButton("p2_download_summary_percentage",
                                                      "Download(percentage, csv)")
                                     ),
                                     br(),
                                     tableOutput("p2_summary_percentage")
                            )),style="overflow-y:scroll") # end of main panel
                      )),
             # code book for variable description
             tabPanel("Codebook", dataTableOutput('codebook.df'))
  )
)

# utility functions
make.summary.df <- function(name){
  # make summary data frame by 'name'
  # 
  # Args:
  #   data: data.frame to be used for making summary data frame
  #   name: name of group to split the summary table
  #
  # Returns:
  #   data.frame of summary table by name
  tapp <- tapply(DATA[['KWH']], as.factor(DATA[[name]]), summary)
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

make.column.factor <- function(group) {
  # get factorized vector whose labels and levels are properly set
  #
  # Args:
  #   group: name of group(column) to be factorized
  #
  # Returns:
  #   vector(factor): factor vector with properly labeld
  factor.coded <- names(CODEBOOK_JSON[[group]]$coded)
  factor.label <- unlist(CODEBOOK_JSON[[group]]$coded[factor.coded])
  factored <- factor(DATA[[group]],
                     levels = factor.coded,
                     labels = factor.label)
  return(factored)
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
p1_boxplot <- NULL
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # simple caching
  
  
  render.p1.boxplot <- function(input) {
    vector_c1 <- make.column.factor(input$p1_criterion1)
    kwh <- DATA[['KWH']]
    if (!isSecondgroupSet(input)){
      # only when first group is selected
      p1_boxplot <<- ggplot(mapping=aes(y=kwh, x=vector_c1)) + 
        geom_boxplot() + xlab(input$p1_criterion1) +
        labs(title=paste("Box Plot of total consumption by",input$p1_criterion1))
      p1_boxplot
    } else{
      # when first and second group was selected
      vector_c2 <- make.column.factor(input$p1_criterion2)
      p1_boxplot <<- ggplot(mapping = aes(y = kwh, 
                                x = vector_c1, 
                                fill = vector_c2)) + 
        geom_boxplot() + xlab(input$p1_criterion1) + 
        labs(title = paste("Box Plot of total consumption by",
                           input$p1_criterion1,"and",input$p1_criterion2)) +
        labs(fill = input$p1_criterion2)
      p1_boxplot
    }
  }
  # for guide(frequent used variables, data source, some infos) on the top 
  output$guide<-renderUI({
    tagList(
      p("Data Source: ",
        a(href="https://www.eia.gov/consumption/residential/data/2015/", 
          "RECS2015(Residential Electricity Consumption Survey 2015)")),
      br(),
      tags$ul(
        tags$li("MONEYPY: income"),
        tags$li("CLIMATE_REGION_PUB: climate(cold, very cold, hot dry,...)"),
        tags$li("TYPEHUQ: Type of housing unit(mobile, single-family, apartment..)"),
        tags$li("UATYP10: Census 2010 Urban Type(Urban, Rural)"),
        tags$li("DIVISION: Census Division(New Englance, Pacific..)"),
        tags$li("HHSEX: Respondent gender"),
        tags$li("HHAGE: Respondent age"),
        tags$li("EMPLOYHH: Respondent employment status(fulltime, part time..)")
    )
  )
  })
  # variable desc side panel(total consumption panel)
  output$p1_var_desc <- renderUI({
    rs <- tagList(
      h3(input$p1_criterion1),
      p(' ',unlist(CODEBOOK[CODEBOOK$name==input$p1_criterion1,2])),
      br(),
      pre(unlist(CODEBOOK[CODEBOOK$name==input$p1_criterion1,3]))
    )
    
    # if user choose second group
    if (isSecondgroupSet(input)){
      rs <- tagList(rs, 
                    hr(),
                    h3(input$p1_criterion2),
                    p(' ',unlist(CODEBOOK[CODEBOOK$name==input$p1_criterion2,2])),
                    br(),
                    pre(unlist(CODEBOOK[CODEBOOK$name==input$p1_criterion2,3]))
                    )
    }
    return(rs)
  })
  # boxplot panel(total consumption panel)
  output$p1_download_boxplot <- downloadHandler(
    filename=function(){
      filename <- paste0(Sys.Date(),"_",input$p1_criterion1)
      if (isSecondgroupSet(input)) {
        filename <- paste0(filename,"_",input$p1_criterion2)
      }
      filename <- paste0(filename,"_boxplot.jpeg")
      return(filename)
    },
    content=function(filename){
      print(getOption('device'))
      nr.levels <- nlevels(DATA[[input$p1_criterion1]] %>% as.factor)
      jpeg_width = nr.levels * 10
      if (isSecondgroupSet(input)){
        jpeg_width = jpeg_width * 1.5
      }

      ggsave(filename = filename,
             plot = p1_boxplot,
             device = "jpeg",
             height = 15,
             width = jpeg_width,
             units = "cm")
    },
    contentType = "image/jpeg"
  )
  # rendering plot
  output$p1_boxplot <- renderPlot({
    render.p1.boxplot(input)
  })
  
  # summary tabnpanel(total consumption panel)
  output$p1_summary <- renderPrint({
    cat("By",input$p1_criterion1,end="\n")
    print(make.summary.df(input$p1_criterion1))
    if (isSecondgroupSet(input)){
      # only when second group is selected
      cat(" ",end="\n")
      cat("By",input$p1_criterion2,end="\n")
      print(make.summary.df(input$p1_criterion2))
    }
  })
  
  # anova test tab panel(total consumption panel)
  output$p1_aov <- renderPrint({
    total.consumption=DATA[['KWH']]
    if (!isSecondgroupSet(input)){
      # only when first group is selected
      cat("oneway ANOVA test of",input$p1_criterion1,end="\n")
      aov.result=aov(total.consumption~ as.factor(DATA[[input$p1_criterion1]]))
    } else {
      # when first and second group are selected
      cat("twoway ANOVA test of",input$p1_criterion1,"and",input$p1_criterion2,end="\n")
      aov.result=aov(total.consumption~ as.factor(DATA[[input$p1_criterion1]])+as.factor(DATA[[input$p1_criterion2]]))
    }
    cat(" ",end="\n")
    print(aov.result)
    cat("=======================================================",end="\n")
    summary.aov.result=summary(aov.result)
    print(summary.aov.result)
    # by the value of p-value, it print variable's influence
    p.value=summary.aov.result[[1]][["Pr(>F)"]]
    if (p.value[1]<0.05) 
      cat('\n\n','==>',input$p1_criterion1,' is influential to total consumption\n')
    if (isTRUE(p.value[2]<0.05)) 
      cat(' ==>',input$p1_criterion2,' is influential to total consumption\n')
  })
  
  # variable description panel(consumption usage page)
  output$p2_var_desc <- renderPrint({
    desc <- unlist(CODEBOOK[CODEBOOK$name==input$p2_criterion,2])
    coded <- unlist(CODEBOOK[CODEBOOK$name==input$p2_criterion,3])
    cat(input$p2_criterion, end="\n")
    cat('  :',desc,end="\n")
    br()
    cat(coded,end="\n")
  })
  # rendering bar plot tabpanel
  output$p2_boxplot <- renderPlot({
    f.group <- make.column.factor(input$p2_criterion)
    nr.levels <- nlevels(f.group)
    par(mfrow=c(13,2))
    for (col in 1:26){
      idx <- KWH_COL_IDX[col]
      desc <- USAGE_DESC[col]
      boxplot(DATA[[idx]]~f.group,
              xlab=input$p2_criterion,
              ylab=paste("KWH used by\n",desc),
              main=paste("Mean KWH used by",desc,"\ngrouped by",input$p2_criterion),
              range=3)
    }
  })
  output$p2_download_boxplot_split <- downloadHandler(
    filename=paste0("consumption_usage_by",input$p2_criterion,".zip"),
    content=function(filename){
      f.group <- make.column.factor(input$p2_criterion)
      nr.levels <- nlevels(f.group)
      file_list = c()
      for (col in 1:26){
        idx <- KWH_COL_IDX[col]
        desc <- USAGE_DESC[col]
        tmp_filename <- paste("average_KWH_used_by",desc,sep="",end=".jpeg")
        file_list = c(file_list, tmp_filename)
        tmp_filename <- get.temp.path(tmp_filename)
        jpeg(tmp_filename,height=700,width=700)
        boxplot(DATA[[idx]]~f.group,xlab=input$p2_criterion,ylab=paste("KWH used by\n",desc),
                main=paste("Mean KWH used by",desc,"grouped by",input$p2_criterion),
                range=3)
        dev.off()
      }
      wd.backup <- getwd()
      setwd(TEMP_DIR)
      zip(zipfile = filename, files = file_list)
      setwd(wd.backup)
    },
    contentType = "application/zip"
  )
  output$p2_download_boxplot_combined <- downloadHandler(
    filename = paste0("consumption_usage_by",input$p2_criterion,"(combined).jpeg"),
    content=function(filename){
      criterion <- input$p2_criterion
      f.group <- as.factor(DATA[[criterion]])
      nr.levels <- nlevels(f.group)
      jpeg(filename = filename, height=13*300, width = 2*300)
      par(mfrow=c(13,2))
      for (col in 1:26){
        idx <- KWH_COL_IDX[col]
        desc <- USAGE_DESC[col]
        tmp_filename <- paste("Mean KWH used by",desc,"\ngrouped by",criterion,sep="_",end=".jpeg")
        boxplot(DATA[[idx]]~f.group,xlab=criterion,ylab=paste("KWH used by\n",desc),
                main=paste("Mean KWH used by",desc,"\ngrouped by",criterion),
                range=3)
        
      }
      dev.off()
    }
  )
  output$p2_barplot <- renderPlot({
    criterion <- input$p2_criterion
    f.group <- as.factor(DATA[[criterion]])
    nr.levels <- nlevels(f.group)
    par(mfrow=c(floor(nr.levels/2)+1,2),
      oma=c(4,4,4,10),mar=c(7,10,1,1))
    tmp <- DATA %>% select(criterion,KWH_COL_IDX) %>% group_by_(criterion) %>% 
      summarise_all(funs(mean),rm.na=TRUE) 
    name=rownames(tmp)
    tmp <- tmp %>% slice(1:n()) %>% as.matrix
    xmax = max(tmp[,2:27])
    for (col in 1:nr.levels){
      barplot(tmp[col,2:27]+0.05,
              horiz=TRUE, 
              names.arg=USAGE_DESC,
              main=name[col],
              las=1,
              xlim=c(0,xmax+100),
              xlab="KWH(year)")
    }
  })
  get.codebook.str <- function(input) {
    desc <- unlist(CODEBOOK[CODEBOOK$name==input$p2_criterion,2])
    coded <- unlist(CODEBOOK[CODEBOOK$name==input$p2_criterion,3])
    text_out=""
    text_out <- paste(input$p2_criterion, end = "\n")
    text_out <- paste(text_out, '  :', desc, end = "\n\n")
    text_out <- paste(text_out, coded, end="\n")
    return(text_out)
  }
  output$p2_download_barplot_split <- downloadHandler(
    filename=paste0("consumption_usage_by",input$p2_criterion,".zip"),
    content=function(filename){
      # create plots
      criterion <- input$p2_criterion
      f.group <- as.factor(DATA[[criterion]])
      nr.levels <- nlevels(f.group)
      tmp <- DATA %>% select(criterion,KWH_COL_IDX) %>% group_by_(criterion) %>% 
        summarise_all(funs(mean),rm.na=TRUE) 
      name=rownames(tmp)
      tmp <- tmp %>% slice(1:n()) %>% as.matrix
      file_list=c()
      for (col in 1:nr.levels){
        tmp_filename <- paste0("[", name[col],"] consumption_usage_bar_plot.jpeg")
        file_list <- c(file_list , tmp_filename)
        tmp_filename <- get.temp.path(tmp_filename)
        jpeg(tmp_filename, height=800, width=500)
        par(mar = c(7,15,3,1) )
        xmax = max(tmp[col,2:27])
        barplot(tmp[col,2:27]+0.05,
                horiz=TRUE, 
                names.arg=USAGE_DESC,
                main=name[col],
                las=1,
                xlim=c(0,xmax+100),
                xlab="KWH(year)")
        dev.off()
      }
      # create codebook
      codebook_filename <- "codebook.txt"
      text_out <- get.codebook.str(input)
      write(x = text_out, file = get.temp.path(codebook_filename))
      wd.backup <- getwd()
      setwd(TEMP_DIR)
      zip(zipfile = filename, files = c(codebook_filename, file_list))
      setwd(wd.backup)
    },
    contentType = "application/zip"
  )
  output$p2_download_barplot_combined <- downloadHandler(
    filename = paste0("consumption_usage_by",input$p2_criterion,"(combined).jpeg"),
    content=function(filename){
      criterion <- input$p2_criterion
      f.group <- as.factor(DATA[[criterion]])
      nr.levels <- nlevels(f.group)
      # open device
      jpeg(filename = filename, height = 700, width = 250*(nr.levels))
      par(mfrow=c(1,nr.levels), mar=c(3,5,1,1), oma = c(10,10,3,3))
      
      tmp <- DATA %>% select(criterion,KWH_COL_IDX) %>% group_by_(criterion) %>% 
        summarise_all(funs(mean),rm.na=TRUE) 
      name=rownames(tmp)
      tmp <- tmp %>% slice(1:n()) %>% as.matrix
      xmax = max(tmp[,2:27])
      for (col in 1:nr.levels){
        if (col %% 5 == 1) {
          barplot(tmp[col,2:27]+0.05,
                  horiz=TRUE, 
                  names.arg=USAGE_DESC,
                  main=name[col],
                  las=1,
                  xlim=c(0,xmax+100),
                  xlab="KWH(year)")
        } else {
          barplot(tmp[col,2:27]+0.05,
                  horiz=TRUE, 
                  names.arg=USAGE_DESC,
                  main=name[col],
                  las=1,
                  xlab="KWH(year)",
                  xlim=c(0,xmax+100),
                  yaxt='n', 
                  ann=FALSE)
        }
      }
      dev.off()
    }
  )
  
  
  # rendering summary table for 2nd navpane
  get.average.table.consumption.usage <- function(input) {
    criterion <- input$p2_criterion
    f.group <- as.factor(DATA[[criterion]])
    nr.levels <- nlevels(f.group)
    tmp <- DATA %>% select(criterion, KWH_COL_IDX) %>% group_by_(criterion) %>% 
      summarise_all(funs(mean),rm.na=TRUE) 
    name=rownames(tmp)
    tmp <- tmp %>% slice(1:n()) %>% as.matrix %>% t
    tmp <- tmp[-1,]
    colnames(tmp) <- name
    rownames(tmp) <- USAGE_DESC
    return(tmp)
  }
  output$p2_summary_header_avg <- renderUI({
    div(
      h3( paste0("Summary Table By ",input$p2_criterion) ),
      p(paste0("Yearly Average Consumption Usage by ",
               input$p2_criterion,".")) 
    )
  })
  output$p2_summary_avg <- renderTable({
    return(get.average.table.consumption.usage(input))
  }, rownames=TRUE)
  
  output$p2_download_summary_avg <- downloadHandler(
    filename = paste0("mean_consumption_usage_by_",input$p2_criterion,".csv"),
    content = function(filename){
      csv_out <- get.average.table.consumption.usage(input)
      write.csv(x=csv_out, file=filename, row.names = TRUE)
    }
  )
  
  get.percentage.table.consumption.usage <- function(input) {
    tmp <- get.average.table.consumption.usage(input)
    column.sum <- colSums(tmp)
    tmp <- sweep(tmp,MARGIN=2,column.sum,`/`)
    tmp <- rbind(tmp, rep(x = 1, times = NCOL(tmp)))
    tmp <- round(tmp, 3)
    rownames(tmp)[NROW(tmp)] <- "TOTAL"
    return(tmp)
  }
  
  output$p2_summary_header_percentage <- renderUI({
    div(
      h3( paste0("Summary Table(Percentage) By ",input$p2_criterion) ),
      p(paste0("Yearly Consumption Usage Percentage by ",
               input$p2_criterion,".")) 
    )
  })
  output$p2_summary_percentage <- renderTable({
    return(get.percentage.table.consumption.usage(input))
  }, rownames=TRUE)
  
  output$p2_download_summary_percentage <- downloadHandler(
    filename = paste0("percentage_consumption_usage_by_",input$p2_criterion,".csv"),
    content = function(filename){
      csv_out <- get.percentage.table.consumption.usage(input)
      write.csv(x=csv_out, file=filename, row.names = TRUE)
    }
  )
  
  ## panel 3
  output$codebook.df <- renderDataTable(
    datatable(CODEBOOK, 
              options = list(
                paging = TRUE,
                pageLength = 20
      ))
    )
}

# Run the application 
shinyApp(ui=ui, server=server)

