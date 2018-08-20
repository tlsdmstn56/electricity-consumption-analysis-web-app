# library import
library(shiny)
library(DT)
library(readr)
library(dplyr)
library(jsonlite)
library(ggplot2)
library(plotly)
library(shinycssloaders)
library(gridExtra)
library(tidyr)
library(tibble)
library(shinyBS)
library(lemon)
library(shinyWidgets)

theme_set(theme_grey())
# Data Import
DATA <-
  read_csv("../data/recs2015_clean.csv", locale = locale(tz = "US/Pacific"))
CODEBOOK <-
  read_csv("../data/codebook_final_with_type.csv", na = c("", ""))
CODEBOOK <- CODEBOOK[-c(334, 335), ]
CODEBOOK_JSON <- fromJSON("../data/codebook_final.json")
# Constants
KWH <- DATA[['KWH']]
KWH_COL_IDX <- grep("^KWH*", unlist(colnames(DATA)))
KWH_COL_IDX <- KWH_COL_IDX[2:length(KWH_COL_IDX)]
KWH_DF <- as.data.frame(DATA[, KWH_COL_IDX])
USAGE_DESC <-
  c(
    'space hitting',
    'AC(central, individual)',
    'water heating',
    'all refrigerators',
    'first refrigerators',
    'second refrigerators',
    'freezers',
    'cooking(stove, cooktop, oven)',
    'microwave',
    'clothes washer',
    'clothes dryer',
    'dishwashers',
    'lighting(indor, outdoor)',
    'all TV',
    'first tv',
    'second tv',
    'air handler for heating',
    'air handler for cooling',
    'evaporative cooler',
    'ceiling fan',
    'dehumidifiers',
    'humidifiers',
    'swimming pool pump',
    'hot tub pumps',
    'hot tub heaters',
    'etc'
  )
USAGE_DESC_TABLE <- data.frame(name = colnames(DATA)[KWH_COL_IDX])
N_USAGE_DESC <- length(USAGE_DESC)
colnames(KWH_DF) <- USAGE_DESC
KWH_DF_LONG <- gather(KWH_DF)
COLNAMES_IN_DROPDOWN <- names(DATA)
DROPDOWN_MENU <- sort(COLNAMES_IN_DROPDOWN[1:242])
TEMP_DIR <- tempdir()

options(
  device = function()
    pdf(file = paste0(TEMP_DIR, "Rplots.pdf"))
)

source("utils.R", local = TRUE)
source("ui.R", local = TRUE)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # simple caching
  p1_boxplot <- NULL
  p2_boxplot_one <- NULL
  p2_boxplot_each <- NULL
  p2_barplot_one <- NULL
  p2_barplot_each <- NULL
  
  p1_c1_factor <- reactive(make.column.factor(input$p1_criterion1))
  p1_c2_factor <- reactive(make.column.factor(input$p1_criterion2))
  p2_c_factor <- reactive(make.column.factor(input$p2_criterion))
  p1_c1_desc <- reactive(get.desc(input$p1_criterion1))
  p1_c2_desc <- reactive(get.desc(input$p1_criterion2))
  p2_c_desc <- reactive(get.desc(input$p2_criterion))
  p1_c1_levels <- reactive(levels(p1_c1_factor))
  p1_c0_levels <- reactive(levels(p1_c2_factor))
  p2_c_levels <- reactive(levels(p2_c_factor))
  p1_c1_nlevels <- reactive(nlevels(p1_c1_factor))
  p1_c2_nlevels <- reactive(nlevels(p1_c2_factor))
  p2_c_nlevels <- reactive(nlevels(p2_c_factor))
  p1_boxplot_height <- reactive({
    if(input$p1_box_autosize){
      return(NULL)
    } else {
      return(input$p1_box_height)
    }
  })
  p1_boxplot_width <- reactive({
    if(input$p1_box_autosize){
      return(NULL)
    } else {
      return(input$p1_box_width-100)
    }
  })
  p2_boxplot_height <- reactive({
    if(input$p2_box_autosize){
      return(NULL)
    } else {
      return(input$p2_box_height)
    }
  })
  p2_boxplot_width <- reactive({
    if(input$p2_box_autosize){
      return(NULL)
    } else {
      return(input$p2_box_width-100)
    }
  })
  
  p2_barplot_height <- reactive({
    if(input$p2_bar_autosize){
      return(700)
    } else {
      return(input$p2_bar_height)
    }
  })
  p2_barplot_width <- reactive({
    if(input$p2_bar_autosize){
      return(NULL)
    } else {
      return(input$p2_bar_width)
    }
  })
  
  # ----------------------------------------
  #
  # [Panel 1] Boxplot
  #
  # ----------------------------------------
  render.p1.boxplot <- function(input) {
    box.xlab <- list(title = paste0(p1_c1_desc(),"(",
                                    input$p1_criterion1,")"))
    box.ylab <- list(title = "KWH (1 year)")
    box.title <- paste("Box Plot of total consumption by\n", input$p1_criterion1)
    box.margin <- list(t=100, b=100)
    if (!isSecondgroupSet(input)) {
      # only when first group is selected
      p1_boxplot <<- plot_ly(x = ~p1_c1_factor(), y = ~KWH, type="box",
                             height = p1_boxplot_height(),
                             width = p1_boxplot_width()) %>%
        layout(title = box.title, xaxis = box.xlab,
               yaxis = box.ylab,
               margin = box.margin)
    } else{
      # when first and second group was selected      
      box.title <- paste(box.title, "and", input$p1_criterion2)
      p1_boxplot <<- plot_ly(x = ~p1_c1_factor(), y = ~KWH, 
                             color= ~p1_c2_factor(), type="box", 
                             hoverinfo="q1+median+mean+q3",
                             height = p1_boxplot_height(),
                             width = p1_boxplot_width()) %>%
        layout(title = box.title, xaxis = box.xlab, 
               yaxis = box.ylab,
               margin = box.margin,
               # annotations = legendtitle,
               boxmode = "group")
    }
    return(p1_boxplot)
  }
  
  # variable desc side panel(total consumption panel)
  output$p1_var_desc <- renderUI({
    criterion1.title <- paste0(input$p1_criterion1,"(x-axis)")
    rs <- tagList(h4(criterion1.title),
                  p(' ', p1_c1_desc()))
    
    # if user choose second group
    if (isSecondgroupSet(input)) {
      criterion2.title <- paste0(input$p1_criterion2,"(color)")
      rs <- tagList(rs,
                    hr(),
                    h4(criterion2.title),
                    p(' ', p1_c2_desc()))
    }
    return(rs)
  })
  
  # rendering plot
  output$p1_boxplot <- renderPlotly({
    render.p1.boxplot(input) %>% layout(dragmode = "pan")
  })
  # ----------------------------------------
  #
  # [Panel 1] Summary Table
  #
  # ----------------------------------------  
  # summary tabnpanel(total consumption panel)
  output$p1_summary <- renderPrint({
    cat("By", input$p1_criterion1, end = "\n")
    print(make.summary.df(input$p1_criterion1))
    if (isSecondgroupSet(input)) {
      # only when second group is selected
      cat(" ", end = "\n")
      cat("By", input$p1_criterion2, end = "\n")
      print(make.summary.df(input$p1_criterion2))
    }
  })
  # ----------------------------------------
  #
  # [Panel 1] Anova Test
  #
  # ----------------------------------------
  # anova test tab panel(total consumption panel)
  output$p1_aov <- renderPrint({
    if (!isSecondgroupSet(input)) {
      # only when first group is selected
      cat("oneway ANOVA test of", input$p1_criterion1, end = "\n")
      aov.result = aov(KWH ~ p1_c1_factor())
    } else {
      # when first and second group are selected
      cat("twoway ANOVA test of",
          input$p1_criterion1,
          "and",
          input$p1_criterion2,
          end = "\n")
      aov.result = aov(KWH ~ p1_c1_factor() + p1_c2_factor())
    }
    cat(" ", end = "\n")
    print(aov.result)
    cat("=======================================================",
        end = "\n")
    summary.aov.result = summary(aov.result)
    print(summary.aov.result)
    # by the value of p-value, it print variable's influence
    p.value = summary.aov.result[[1]][["Pr(>F)"]]
    if (p.value[1] < 0.05)
      cat('\n\n',
          '==>',
          input$p1_criterion1,
          ' is influential to total consumption\n')
    if (isTRUE(p.value[2] < 0.05))
      cat(' ==>',
          input$p1_criterion2,
          ' is influential to total consumption\n')
  })
  
  # variable description panel(consumption usage page)
  output$p2_var_desc <- renderUI({
    tagList(h3(input$p2_criterion),
            p(p2_c_desc()))
  })
  
  # ----------------------------------------
  #
  # [Panel 2] Box Plot
  #
  # ----------------------------------------
  output$p2_boxplots_ui <- renderUI({
    if(is.null(input$p2_box_y)){
      plotlyOutput(
        "p2_boxplot_1",
        height = p2_boxplot_height(),
        width = p2_boxplot_width()
      ) %>% withSpinner
    }
    rs <- lapply(input$p2_box_y, function(x) {
      id <- paste0("p2_boxplot_",x)
      po <- plotlyOutput(outputId = id,
                         height = "auto",
                         width = "auto") %>% withSpinner
      return(div.flex.element(po,
                              width = p2_boxplot_width(),
                              height = p2_boxplot_height()))
    })
    lapply(input$p2_box_y, function(x) {
      x <- as.numeric(x)
      id <- paste0("p2_boxplot_",x)
      output[[id]] <- render.p2.boxplot(x)
    })
    do.call("div.flex.container", rs)
  })
  render.p2.boxplot <- function(usage_idx) {
    renderPlotly({
      box.ylab <- list(title = paste0("Annual KWH consumed by\n",p2_c_desc()))
      box.xlab  <- list(title = p2_c_desc())
      box.title <- paste("Consumption Usage by\n", p2_c_desc())
      box.margin <- list(t=100, b=100, r=80,l=80)
        plot_ly(x = ~p2_c_factor(), y = ~KWH_DF[,usage_idx], type="box",
                height = p2_boxplot_height(), width = p2_boxplot_width()) %>%
        layout(xaxis = box.xlab, yaxis = box.ylab, title=box.title,
               margin = box.margin)
    })
  }
  lapply(1:N_USAGE_DESC, function(x) {
    id <- paste0("p2_boxplot_",x)
    output[[id]] <- render.p2.boxplot(x)
  })
  # ----------------------------------------
  #
  # [Panel 2] Bar Plot
  #
  # ----------------------------------------
  output$p2_barplots <- renderPlotly({
    factored <- make.column.factor(input$p2_criterion)
    nr.levels <- nlevels(factored)
    barplot_list <- list()
    tmp <-
      DATA %>% select(KWH_COL_IDX) %>% mutate(criterion = p2_c_factor()) %>%
      group_by(criterion) %>%
      summarise_all(funs(mean), rm.na = TRUE)
    colnames(tmp)[-1] <- USAGE_DESC
    tmp.long <- gather(tmp, usage, avg_kwh,-criterion)
    bar.xlab <- list(title = "Consumption Usage")
    bar.ylab <- list(title = "Average KWH (1 year)")
    bar.title <- paste0("\nConsumption Usage by\n",p2_c_desc())
    bar.margin <- list(t=100, b=150, r=80,l=80)
    plot_ly(tmp.long, x = ~usage, y = ~avg_kwh, color = ~criterion, 
            type="bar", height = p2_barplot_height(), width = p2_barplot_width()) %>%
      layout(margin = list(r=100), barmode='group', legend = list(y=-0.3,orientation = 'h'),
             xaxis = bar.xlab, yaxis = bar.ylab, title = bar.title, margin = bar.margin)
  })
  
  # ----------------------------------------
  #
  # [Panel 2] Summary
  #
  # ----------------------------------------
  get.average.table.consumption.usage <- function(input) {
    criterion <- input$p2_criterion
    f.group <- as.factor(DATA[[criterion]])
    nr.levels <- nlevels(f.group)
    tmp <-
      DATA %>% select(criterion, KWH_COL_IDX) %>% group_by_(criterion) %>%
      summarise_all(funs(mean), rm.na = TRUE)
    name = rownames(tmp)
    tmp <- tmp %>% slice(1:n()) %>% as.matrix %>% t
    tmp <- tmp[-1, ]
    colnames(tmp) <- name
    rownames(tmp) <- USAGE_DESC
    return(tmp)
  }
  output$p2_summary_header_avg <- renderUI({
    curr_user_view <<- "p2_summary"
    div(h3(paste0(
      "Summary Table By ", input$p2_criterion
    )),
    p(
      paste0(
        "Yearly Average Consumption Usage by ",
        input$p2_criterion,
        "."
      )
    ))
  })
  output$p2_summary_avg <- renderTable({
    return(get.average.table.consumption.usage(input))
  }, rownames = TRUE)
  
  output$p2_download_summary_avg <- downloadHandler(
    filename = paste0("mean_consumption_usage_by_", input$p2_criterion, ".csv"),
    content = function(filename) {
      csv_out <- get.average.table.consumption.usage(input)
      write.csv(x = csv_out,
                file = filename,
                row.names = TRUE)
    }
  )
  
  get.percentage.table.consumption.usage <- function(input) {
    tmp <- get.average.table.consumption.usage(input)
    column.sum <- colSums(tmp)
    tmp <- sweep(tmp, MARGIN = 2, column.sum, `/`)
    tmp <- rbind(tmp, rep(x = 1, times = NCOL(tmp)))
    tmp <- round(tmp, 3)
    rownames(tmp)[NROW(tmp)] <- "TOTAL"
    return(tmp)
  }
  
  output$p2_summary_header_percentage <- renderUI({
    div(h3(
      paste0("Summary Table(Percentage) By ", input$p2_criterion)
    ),
    p(
      paste0(
        "Yearly Consumption Usage Percentage by ",
        input$p2_criterion,
        "."
      )
    ))
  })
  output$p2_summary_percentage <- renderTable({
    return(get.percentage.table.consumption.usage(input))
  }, rownames = TRUE)
  
  output$p2_download_summary_percentage <- downloadHandler(
    filename = paste0(
      "percentage_consumption_usage_by_",
      input$p2_criterion,
      ".csv"
    ),
    content = function(filename) {
      csv_out <- get.percentage.table.consumption.usage(input)
      write.csv(x = csv_out,
                file = filename,
                row.names = TRUE)
    }
  )
  
  # ----------------------------------------
  #
  # [Panel 3] Codebook
  #
  # ----------------------------------------
  output$p3_codebook <- renderDataTable({
    curr_user_view <<- "p3_codebook"
    datatable(CODEBOOK[, -4],
              options = list(paging = TRUE,
                             pageLength = 20))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
