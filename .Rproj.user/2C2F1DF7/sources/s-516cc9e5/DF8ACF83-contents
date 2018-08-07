# ibrary import
library(shiny)
library(DT)
library(readr)
library(dplyr)
#library(ggplot2)
#library(plotly)

# Data Import
data <- read_csv("../data/recs2015_clean.csv", locale=locale(tz="US/Pacific"))
codebook <- read_csv("../data/codebook_final.csv",na = c("",""))

# Constants
KWH_COL_IDX <- grep("^KWH*",unlist(colnames(data)))
KWH_COL_IDX <- KWH_COL_IDX[2:length(KWH_COL_IDX)]
USAGE_NAME <- c('space hitting','AC(central, individual)','water heating',
             'all refrigerators','first refrigerators','second refrigerators',
             'freezers','cooking(stove, cooktop, oven)','microwave',
             'clothes washer','clothes dryer','dishwashers',
             'lighting(indor, outdoor)','all TV','first tv','second tv',
             'air handler for heating','air handler for cooling',
             'evaporative cooler','ceiling fan','dehumidifiers','humidifiers',
             'swimming pool pump','hot tub pumps','hot tub heaters','etc')
COLNAMES_IN_DROPDOWN <- names(data)
DROPDOWN_MENU <- sort(COLNAMES_IN_DROPDOWN[1:242])


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
  verbatimTextOutput('guide'),
  # Navigation Bar on the top
  navbarPage("Menu",
             # Total Consumption Tab
             tabPanel("Total Consumption",
                      sidebarLayout(
                        sidebarPanel(
                          # first group
                          selectInput("firstgroup", 
                                      "First Criterion",
                                      DROPDOWN_MENU),
                          # second group
                          selectInput("secondgroup", 
                                      "Second Criterion",
                                      c("None",DROPDOWN_MENU)),
                          # group description
                          verbatimTextOutput("var.desc")),
                        # plots and summary, anova
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Plot", 
                                     br(),
                                     tags$p("Press Download Button for bigger image"),
                                     downloadButton("download_plot1", "Download"), 
                                     br(),
                                     plotOutput("plot1")),
                            tabPanel("Summary",
                                     verbatimTextOutput("summary1")),
                            tabPanel("Anova Test", 
                                     verbatimTextOutput("aov1"))
                            )) # end of main panel
             )),
             # Consumption Usage tab
             tabPanel("Consumption Usage",
                      sidebarLayout(
                        # for group selection
                        sidebarPanel(
                          selectInput("firstgroup2", "Criterion",
                             sort(COLNAMES_IN_DROPDOWN[1:242])),
                          verbatimTextOutput("var.desc2")),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Box Plot",
                                     br(),
                                     tags$p("Press Download Button for bigger image"),
                                     tags$div(
                                       downloadButton("download_box2_split", 
                                                      "Download(file by plots, .zip)"), 
                                       downloadButton("download_box2_combined", 
                                                      "Download(combined to one file, .jpeg)")
                                     ),
                                     br(),
                                     plotOutput("boxplot", height=4000, width=1000)),
                            tabPanel("Bar Chart",
                                     br(),
                                     tags$p("Press Download Button for bigger image"),
                                     tags$div(
                                       downloadButton("download_bar2_split", 
                                                      "Download(file by plots, .zip)"), 
                                       downloadButton("download_bar2_combined", 
                                                      "Download(combined to one file, .jpeg)")
                                     ),
                                     br(),
                                     plotOutput("barchart", height=3000, width=1000)),
                            tabPanel("Summary Table", tableOutput("summarytable"))
                            ),style="overflow-y:scroll") # end of main panel
             )),
             # code book for variable description
             tabPanel("Codebook", dataTableOutput('codebook.df'))
             )
)

# utility functions
make.summary.df <- function(data,name){
  # make summary data frame by 'name'
  # 
  # Args:
  #   data: data.frame to be used for making summary data frame
  #   name: name of group to split the summary table
  #
  # Returns:
  #   data.frame of summary table by name
  tapp <- tapply(data[['KWH']], as.factor(data[[name]]), summary)
  tapp=do.call(rbind,tapp)
  return(tapp)
}

# helper functions
q1 <- function(x){
  quantile(x,0.25)
}
q3 <- function(x){
  quantile(x,0.75)
}
br <- function(){
  cat(' ',"\n")
}
isSecondgroupSet <- function(input) {
  return("None"!=input$secondgroup & input$firstgroup!=input$secondgroup)
}
# Define server logic required to draw a histogram
server <- function(input, output) {
  renderPlot1 <- function(input) {
    firstgroup=as.factor(data[[input$firstgroup]])
    if (!isSecondgroupSet(input)){
      # only when first group is selected
      boxplot(data[['KWH']]~ firstgroup,
              xlab=input$firstgroup,
              ylab="Total Consumption of Electricity(KWH, 1 year)",
              main=paste("Box Plot of total consumption by",input$firstgroup,sep=" "),
              range=3
      )
    } else{
      # when first and second group was selected
      boxplot(data[['KWH']]~ firstgroup*as.factor(data[[input$secondgroup]]),
              xlab=paste("Legend: ",input$firstgroup,".", input$secondgroup,sep=""),
              ylab="Total Consumption of Electricity(KWH, 1 year)",
              main=paste("Box Plot of total consumption by\n",
                         input$firstgroup,"and",input$secondgroup,sep=" "),
              col=rainbow(nlevels(firstgroup)),
              range=3
      )
    }
  }
  # for guide(frequent used variables, data source, some infos) on the top 
  output$guide<-renderPrint({
    cat("Data Source: RECS2015(Residential Electricity Consumption Survey 2015)",end="\n")
    cat("   https://www.eia.gov/consumption/residential/data/2015/",end="\n")
    br()
    cat("* MONEYPY: income",end="\n")
    cat("* CLIMATE_REGION_PUB: climate(cold, very cold, hot dry,...)",end="\n")
    cat("* TYPEHUQ: Type of housing unit(mobile, single-family, apartment..",end="\n")
    cat("* UATYP10: Census 2010 Urban Type(Urban, Rural)",end="\n")
    cat("* DIVISION: Census Division(New Englance, Pacific..)",end="\n")
    cat("* HHSEX: Respondent gender",end="\n")
    cat("* HHAGE: Respondent age",end="\n")
    cat("* EMPLOYHH: Respondent employment status(fulltime, part time..)",end="\n")
  })
  # variable desc side panel(total consumption panel)
  output$var.desc <- renderPrint({
    cat(input$firstgroup,end="\n")
    cat(' ',unlist(codebook[codebook$name==input$firstgroup,2]),end="\n")
    cat(' ',end="\n")
    cat(unlist(codebook[codebook$name==input$firstgroup,3]),end="\n")
    # if user choose second group
    if (isSecondgroupSet(input)){
      cat("===========",end="\n")
      cat(input$secondgroup,end="\n")
      cat(' ',unlist(codebook[codebook$name==input$secondgroup,2
                              ]),end="\n")
      cat(' ',end="\n")
      cat(unlist(codebook[codebook$name==input$secondgroup,3]),end="\n")
    }
    
  })
  # boxplot panel(total consumption panel)
  output$download_plot1 <- downloadHandler(
    filename=function(){
      filename <- paste0(Sys.Date(),"_",input$firstgroup)
      if ("None"!=input$secondgroup & input$firstgroup!=input$secondgroup) {
        filename <- paste0(filename,"_",input$secondgroup)
      }
      filename <- paste0(filename,"_boxplot.jpeg")
      return(filename)
    },
    content=function(filename){
      nr.levels <- nlevels(data[[input$firstgroup]] %>% as.factor)
      if ("None"!=input$secondgroup & input$firstgroup!=input$secondgroup){
        nr.levels <- nr.levels * nlevels(data[[input$secondgroup]] %>% as.factor)
      }
      jpeg_width = nr.levels*100
      jpeg(filename = filename, 
           height = 500, 
           width = jpeg_width
           )
      renderPlot1(input)
      dev.off()
    }
  )
  # rendering plot
  output$plot1 <- renderPlot({
    renderPlot1(input)
  })
  
  # summary tabnpanel(total consumption panel)
  output$summary1 <- renderPrint({
    cat("By",input$firstgroup,end="\n")
    print(make.summary.df(data,input$firstgroup))
    if (isSecondgroupSet(input)){
      # only when second group is selected
      cat(" ",end="\n")
      cat("By",input$secondgroup,end="\n")
      print(make.summary.df(data,input$secondgroup))
    }
  })
  
  # anova test tab panel(total consumption panel)
  output$aov1 <- renderPrint({
    total.consumption=data[['KWH']]
    if (!isSecondgroupSet(input)){
      # only when first group is selected
      cat("oneway ANOVA test of",input$firstgroup,end="\n")
      aov.result=aov(total.consumption~ as.factor(data[[input$firstgroup]]))
    } else {
      # when first and second group are selected
      cat("twoway ANOVA test of",input$firstgroup,"and",input$secondgroup,end="\n")
      aov.result=aov(total.consumption~ as.factor(data[[input$firstgroup]])+as.factor(data[[input$secondgroup]]))
    }
    cat(" ",end="\n")
    print(aov.result)
    cat("=======================================================",end="\n")
    summary.aov.result=summary(aov.result)
    print(summary.aov.result)
    # by the value of p-value, it print variable's influence
    p.value=summary.aov.result[[1]][["Pr(>F)"]]
    if (p.value[1]<0.05) 
      cat('\n\n','==>',input$firstgroup,' is influential to total consumption\n')
    if (isTRUE(p.value[2]<0.05)) 
      cat(' ==>',input$secondgroup,' is influential to total consumption\n')
  })
  
  # variable description panel(consumption usage page)
  output$var.desc2 <- renderPrint({
    desc <- unlist(codebook[codebook$name==input$firstgroup2,2])
    coded <- unlist(codebook[codebook$name==input$firstgroup2,3])
    cat(input$firstgroup2, end="\n")
    cat('  :',desc,end="\n")
    br()
    cat(coded,end="\n")
  })
  # rendering bar plot tabpanel
  output$boxplot <- renderPlot({
    criterion <- input$firstgroup2
    f.group <- as.factor(data[[criterion]])
    nr.levels <- nlevels(f.group)
    par(mfrow=c(13,2))
    for (col in 1:26){
      idx <- KWH_COL_IDX[col]
      desc <- USAGE_NAME[col]
      boxplot(data[[idx]]~f.group,xlab=criterion,ylab=paste("KWH used by\n",desc),
              main=paste("Mean KWH used by",desc,"\ngrouped by",criterion),
              range=3)
      
    }
  })
  output$download_box2_split <- downloadHandler(
    filename=paste0("consumption_usage_by",input$firstgroup2,".zip"),
    content=function(filename){
      criterion <- input$firstgroup2
      f.group <- as.factor(data[[criterion]])
      nr.levels <- nlevels(f.group)
      file_list = c()
      for (col in 1:26){
        idx <- KWH_COL_IDX[col]
        desc <- USAGE_NAME[col]
        tmp_filename <- paste("Mean KWH used by",desc,"\ngrouped by",criterion,sep="_",end=".jpeg")
        file_list = c(file_list, tmp_filename)
        jpeg(tmp_filename,height=400,width=400)
        boxplot(data[[idx]]~f.group,xlab=criterion,ylab=paste("KWH used by\n",desc),
                main=paste("Mean KWH used by",desc,"\ngrouped by",criterion),
                range=3)
        dev.off()
      }
      zip(zipfile = filename, files = file_list)
    },
    contentType = "application/zip"
  )
  output$download_box2_combined <- downloadHandler(
    filename = paste0("consumption_usage_by",input$firstgroup2,"(combined).jpeg"),
    content=function(filename){
      criterion <- input$firstgroup2
      f.group <- as.factor(data[[criterion]])
      nr.levels <- nlevels(f.group)
      jpeg(filename = filename, height=13*300, width = 2*300)
      par(mfrow=c(13,2))
      for (col in 1:26){
        idx <- KWH_COL_IDX[col]
        desc <- USAGE_NAME[col]
        tmp_filename <- paste("Mean KWH used by",desc,"\ngrouped by",criterion,sep="_",end=".jpeg")
        boxplot(data[[idx]]~f.group,xlab=criterion,ylab=paste("KWH used by\n",desc),
                main=paste("Mean KWH used by",desc,"\ngrouped by",criterion),
                range=3)
        
      }
      dev.off()
    }
  )
  output$barchart <- renderPlot({
    criterion <- input$firstgroup2
    f.group <- as.factor(data[[criterion]])
    nr.levels <- nlevels(f.group)
    par(mfrow=c(floor(nr.levels/2)+1,2),
      oma=c(4,4,4,10),mar=c(7,10,1,1))
    tmp <- data %>% select(criterion,KWH_COL_IDX) %>% group_by_(criterion) %>% 
      summarise_all(funs(mean),rm.na=TRUE) 
    name=rownames(tmp)
    tmp <- tmp %>% slice(1:n()) %>% as.matrix
    xmax = max(tmp[,2:27])
    for (col in 1:nr.levels){
      barplot(tmp[col,2:27]+0.05,
              horiz=TRUE, 
              names.arg=USAGE_NAME,
              main=name[col],
              las=1,
              xlim=c(0,xmax+100),
              xlab="KWH(year)")
    }
  })
  get.codebook.str <- function(input) {
    desc <- unlist(codebook[codebook$name==input$firstgroup2,2])
    coded <- unlist(codebook[codebook$name==input$firstgroup2,3])
    text_out=""
    text_out <- paste(input$firstgroup2, end = "\n")
    text_out <- paste(text_out, '  :', desc, end = "\n\n")
    text_out <- paste(text_out, coded, end="\n")
    return(text_out)
  }
  output$download_bar2_split <- downloadHandler(
    filename=paste0("consumption_usage_by",input$firstgroup2,".zip"),
    content=function(filename){
      # create plots
      criterion <- input$firstgroup2
      f.group <- as.factor(data[[criterion]])
      nr.levels <- nlevels(f.group)
      tmp <- data %>% select(criterion,KWH_COL_IDX) %>% group_by_(criterion) %>% 
        summarise_all(funs(mean),rm.na=TRUE) 
      name=rownames(tmp)
      tmp <- tmp %>% slice(1:n()) %>% as.matrix
      file_list=c()
      for (col in 1:nr.levels){
        tmp_filename <- paste0("[", name[col],"] consumption_usage_bar_plot.jpeg")
        file_list <- c(file_list , tmp_filename)
        jpeg(tmp_filename, height=800, width=500)
        par(mar = c(7,15,3,1) )
        xmax = max(tmp[col,2:27])
        barplot(tmp[col,2:27]+0.05,
                horiz=TRUE, 
                names.arg=USAGE_NAME,
                main=name[col],
                las=1,
                xlim=c(0,xmax+100),
                xlab="KWH(year)")
        dev.off()
      }
      # create codebook
      codebook_filename <- "codebook.txt"
      text_out <- get.codebook.str(input)
      write(x = text_out, file = filename)
      zip(zipfile = filename, files = c(codebook_filename, file_list))
    },
    contentType = "application/zip"
  )
  output$download_bar2_combined <- downloadHandler(
    filename = paste0("consumption_usage_by",input$firstgroup2,"(combined).jpeg"),
    content=function(filename){
      criterion <- input$firstgroup2
      f.group <- as.factor(data[[criterion]])
      nr.levels <- nlevels(f.group)
      # open device
      jpeg(filename = filename, height = 700, width = 250*(nr.levels))
      par(mfrow=c(1,nr.levels), mar=c(3,5,1,1), oma = c(10,3,3,3))
      
      tmp <- data %>% select(criterion,KWH_COL_IDX) %>% group_by_(criterion) %>% 
        summarise_all(funs(mean),rm.na=TRUE) 
      name=rownames(tmp)
      tmp <- tmp %>% slice(1:n()) %>% as.matrix
      xmax = max(tmp[,2:27])
      for (col in 1:nr.levels){
        if (col %% 5 == 1) {
          barplot(tmp[col,2:27]+0.05,
                  horiz=TRUE, 
                  names.arg=USAGE_NAME,
                  main=name[col],
                  las=1,
                  xlim=c(0,xmax+100),
                  xlab="KWH(year)")
        } else {
          barplot(tmp[col,2:27]+0.05,
                  horiz=TRUE, 
                  names.arg=USAGE_NAME,
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
  # rendering summary table for 2nd navpanel
  output$summarytable <- renderTable({
    criterion <- input$firstgroup2
    tmp <- data %>% select(criterion,KWH_COL_IDX) 
    tmp.mean <- tmp %>% summarise_all(funs(mean))
    tmp.med <- tmp %>% summarise_all(funs(median))
    tmp.q1 <- tmp %>% summarise_all(funs(quantile),probs=0.25)
    tmp.q3 <- tmp %>% summarise_all(funs(quantile),probs=0.75)
    rs <- t(rbind(Q1=tmp.q1,MEAN=tmp.mean,MEDIAN=tmp.med,Q3=tmp.q3))
    rs <- rs[2:NROW(rs),]
    rs <- cbind(rs,description=USAGE_NAME)
    return(rs)
  }, rownames=TRUE)
  
  
  ## panel 3
  output$codebook.df <- renderDataTable(
    datatable(codebook[1:(NROW(codebook)-2),], 
              options = list(
                paging = TRUE,
                pageLength = 20
                ))
    )
}

# Run the application 
shinyApp(ui=ui, server=server)

