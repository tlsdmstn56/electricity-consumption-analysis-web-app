# ibrary import
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)

# Data Import
data <- read_csv("../data/recs2015_clean.csv", locale=locale(tz="US/Pacific"))
codebook <- read_csv("../data/codebook_final.csv")

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
                            tabPanel("Plot", plotOutput("plot1")),
                            tabPanel("Summary", verbatimTextOutput("summary1")),
                            tabPanel("Anova Test", verbatimTextOutput("aov1"))
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
                            tabPanel("Box Plot", plotOutput("boxplot",
                                                            height=4000,
                                                            width=700)),
                            tabPanel("Bar Chart", plotOutput("barchart",
                                                             height=3000,
                                                             width=700)),
                            tabPanel("Summary Table", tableOutput("summarytable"))
                            ),style="overflow-y:scroll") # end of main panel
             )),
             # code book for variable description
             tabPanel("Codebook",tableOutput('codebook.df'))
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
  cat('',"\n")
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  # for guide(frequent used variables, data source, some infos) on the top 
  output$guide<-renderPrint({
    cat("Data Source: RECS2015(Residential Electricity Consumption Survey 2015)",end="\n")
    cat("   https://www.eia.gov/consumption/residential/data/2015/",end="\n")
    br()
    cat("* MONEYPY: 소득",end="\n")
    cat("* CLIMATE_REGION_PUB: 날씨(cold, very cold, hot dry,...)",end="\n")
    cat("* TYPEHUQ: 주거 형태(mobile, single-family, apartment..",end="\n")
    cat("* UATYP10: 도시, 시골",end="\n")
    cat("* DIVISION: 지역(Census Division, New Englance, Pacific..)",end="\n")
    cat("* HHSEX: 응답자 성별",end="\n")
    cat("* HHAGE: 응답자 나이",end="\n")
    cat("* EMPLOYHH: 응답자 고용 상태(fulltime, part time..)",end="\n")
    
  })
  # variable desc side panel(total consumption panel)
  output$var.desc <- renderPrint({
    cat(input$firstgroup,end="\n")
    cat(' ',unlist(codebook[codebook$name==input$firstgroup,2]),end="\n")
    cat(' ',end="\n")
    cat(unlist(codebook[codebook$name==input$firstgroup,3]),end="\n")
    # if user choose second group
    if ("None"!=input$secondgroup & input$firstgroup!=input$secondgroup){
      cat("===========",end="\n")
      cat(input$secondgroup,end="\n")
      cat(' ',unlist(codebook[codebook$name==input$secondgroup,2
                              ]),end="\n")
      cat(' ',end="\n")
      cat(unlist(codebook[codebook$name==input$secondgroup,3]),end="\n")
    }
    
  })
  # boxplot panel(total consumption panel)
  output$plot1 <- renderPlot({
    firstgroup=as.factor(data[[input$firstgroup]])
    if (input$secondgroup == "None" | input$firstgroup == input$secondgroup){
      # only when first group is selected
      boxplot(data[['KWH']]~ firstgroup,
              xlab=input$firstgroup,
              ylab="Total Consumption of Electricity",
              main=paste("Box Plot of total consumption by",input$firstgroup,sep=" "),
              range=3
      )
    } else{
      # when first and second group was selected
      boxplot(data[['KWH']]~ firstgroup*as.factor(data[[input$secondgroup]]),
              xlab=paste("Legend: ",input$firstgroup,".", input$secondgroup,sep=""),
              ylab="Total Consumption of Electricity",
              main=paste("Box Plot of total consumption by\n",
                         input$firstgroup,"and",input$secondgroup,sep=" "),
              col=rainbow(nlevels(firstgroup)),
              range=3
      )
    }
  })
  
  # summary tabnpanel(total consumption panel)
  output$summary1 <- renderPrint({
    cat("By",input$firstgroup,end="\n")
    print(make.summary.df(data,input$firstgroup))
    if ("None"!=input$secondgroup & input$firstgroup!=input$secondgroup){
      # only when second group is selected
      cat(" ",end="\n")
      cat("By",input$secondgroup,end="\n")
      print(make.summary.df(data,input$secondgroup))
    }
  })
  
  # anova test tab panel(total consumption panel)
  output$aov1 <- renderPrint({
    total.consumption=data[['KWH']]
    if (input$secondgroup == "None" | input$firstgroup == input$secondgroup){
      # only when first group is selected
      cat("oneway ANOVA test of",input$firstgroup,end="\n")
      aov.result=aov(total.consumption~ as.factor(data[[input$firstgroup]]))
    }else{
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
    cat(input$firstgroup2, end="\n")
    cat(' ',unlist(codebook[codebook$name==input$firstgroup2,2]),end="\n")
    cat(' ',end="\n")
    cat(unlist(codebook[codebook$name==input$firstgroup2,3]),end="\n")
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
  # rendering plot
  output$barchart <- renderPlot({
    criterion <- input$firstgroup2
    f.group <- as.factor(data[[criterion]])
    nr.levels <- nlevels(f.group)
    par(mfrow=c(floor(nr.levels/2)+1,2),
      oma=c(4,4,4,10),mar=c(7,10,1,1))
    tmp <- data %>% select(criterion,KWH_COL_IDX) %>% group_by_(criterion) %>% 
      summarise_all(funs(mean)) 
    name=rownames(tmp)
    tmp <- tmp %>% slice(1:n()) %>% as.matrix
    for (col in 1:nr.levels){
      barplot(tmp[col,2:27]+0.05,
              horiz=TRUE, 
              names.arg <- USAGE_NAME,
              main=name[col],
              las <- 1,
              xlab="KWH")
    }
  })
  # rendering summary table for 2nd navpanel
  output$summarytable <- renderTable({
    criterion <- input$firstgroup2
    tmp <- data %>% select(criterion,KWH_COL_IDX) 
    tmp.mean <- tmp %>% summarise_all(funs(mean))
    tmp.med <- tmp %>% summarise_all(funs(median))
    tmp.q1 <- tmp %>% summarise_all(funs(q1))
    tmp.q3 <- tmp %>% summarise_all(funs(q3))
    rs <- t(rbind(Q1=tmp.q1,MEAN=tmp.mean,MEDIAN=tmp.med,Q3=tmp.q3))
    rs <- rs[2:NROW(rs),]
    rs <- cbind(rs,description=USAGE_NAME)
    return(rs)
  }, rownames=TRUE)
  
  
  ## panel 3
  output$codebook.df <- renderTable(
    codebook[1:(NROW(codebook)-2),]
    )
}

# Run the application 
shinyApp(ui=ui, server=server)

