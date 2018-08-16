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
  curr_user_view <- ""
  
  render.p1.boxplot <- function(input) {
    vector_c1 <- make.column.factor(input$p1_criterion1)
    kwh <- DATA[['KWH']]
    if (!isSecondgroupSet(input)) {
      # only when first group is selected
      # p1_boxplot <<- ggplot(mapping = aes(y = kwh, x = vector_c1)) +
      #   geom_boxplot() + xlab(input$p1_criterion1) + ylab("kwh(1 year)") +
      #   labs(title = paste("Box Plot of total consumption by", input$p1_criterion1))
      # return(ggplotly(p1_boxplot) %>% layout(dragmode = "pan"))
      box.title <- paste("Violin and Box Plot of total consumption by", input$p1_criterion1)
      box.xlab <- list(title = get.desc(input$p1_criterion1))
      box.ylab <- list(title = "KWH (1 year)")
      box.margin <- list(t=100, b=100)
      p1_boxplot <<- plot_ly(x = ~vector_c1, y = ~kwh, type="violin",
                             box = list(visible=TRUE), meanline = list(visible=TRUE),
                             hoverinfo = 'none') %>%
        layout(title = box.title, xaxis = box.xlab, 
               yaxis = box.ylab,
               margin = box.margin)
      return(p1_boxplot)
    } else{
      # when first and second group was selected
      vector_c2 <- make.column.factor(input$p1_criterion2)
      p1_boxplot <<- ggplot(mapping = aes(y = kwh,
                                          x = vector_c1,
                                          fill = vector_c2)) +
        geom_boxplot() + xlab(input$p1_criterion1) + ylab("kwh(1 year)") +
        labs(
          title = paste(
            "Box Plot of total consumption by",
            input$p1_criterion1,
            "and",
            input$p1_criterion2
          )
        ) +
        labs(fill = input$p1_criterion2)
      return(ggplotly(p1_boxplot) %>% layout(boxmode = "group", dragmode = "pan"))
    }
  }
  
  # variable desc side panel(total consumption panel)
  output$p1_var_desc <- renderUI({
    rs <- tagList(h3(input$p1_criterion1),
                  p(' ', get.desc(input$p1_criterion1)))
    
    # if user choose second group
    if (isSecondgroupSet(input)) {
      rs <- tagList(rs,
                    hr(),
                    h3(input$p1_criterion2),
                    p(' ', get.desc(input$p1_criterion2)))
    }
    return(rs)
  })
      
  # boxplot panel(total consumption panel)
  output$p1_download_boxplot <- downloadHandler(
    filename = function() {
      filename <- paste0(input$p1_criterion1)
      if (isSecondgroupSet(input)) {
        filename <- paste0(filename, "_", input$p1_criterion2)
      }
      filename <- paste0(filename, "_boxplot.jpeg")
      return(filename)
    },
    content = function(filename) {
      print(getOption('device'))
      nr.levels <-
        nlevels(DATA[[input$p1_criterion1]] %>% as.factor)
      jpeg_width = nr.levels * 4
      if (isSecondgroupSet(input)) {
        jpeg_width = jpeg_width * 1.5
      }
      
      ggsave(
        filename = filename,
        plot = p1_boxplot,
        device = "jpeg",
        height = 8,
        width = jpeg_width,
        units = "cm"
      )
    },
    contentType = "image/jpeg"
  )
  # rendering plot
  output$p1_boxplot <- renderPlotly({
    curr_user_view <<- "p1_boxplot"
    ggplotly(render.p1.boxplot(input)) %>% layout(dragmode = "pan")
  })
  
  # summary tabnpanel(total consumption panel)
  output$p1_summary <- renderPrint({
    curr_user_view <<- "p1_summary"
    cat("By", input$p1_criterion1, end = "\n")
    print(make.summary.df(input$p1_criterion1))
    if (isSecondgroupSet(input)) {
      # only when second group is selected
      cat(" ", end = "\n")
      cat("By", input$p1_criterion2, end = "\n")
      print(make.summary.df(input$p1_criterion2))
    }
  })
  
  # anova test tab panel(total consumption panel)
  output$p1_aov <- renderPrint({
    curr_user_view <<- "p1_anova"
    total.consumption = DATA[['KWH']]
    if (!isSecondgroupSet(input)) {
      # only when first group is selected
      cat("oneway ANOVA test of", input$p1_criterion1, end = "\n")
      aov.result = aov(total.consumption ~ as.factor(DATA[[input$p1_criterion1]]))
    } else {
      # when first and second group are selected
      cat("twoway ANOVA test of",
          input$p1_criterion1,
          "and",
          input$p1_criterion2,
          end = "\n")
      aov.result = aov(total.consumption ~ as.factor(DATA[[input$p1_criterion1]])
                       + as.factor(DATA[[input$p1_criterion2]]))
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
    desc <- unlist(CODEBOOK[CODEBOOK$name == input$p2_criterion, 2])
    coded <- unlist(CODEBOOK[CODEBOOK$name == input$p2_criterion, 3])
    
    tagList(h3(input$p2_criterion),
            p(desc))
  })
  output$p2_legend <- renderUI({
    if (input$p2_tabs != "box") {
      return(p(""))
    }
    factored <- make.column.factor(input$p2_criterion)
    tagList(hr(),
            h4("Legend"),
            p("Y axis is measured by KWH"),
            p("From left"),
            tags$ol(
            lapply(levels(factored), tags$li)
            )
    )
  })
  # rendering bar plot tabpanel
  output$p2_boxplot <- renderPlotly({
    curr_user_view <<- "p2_boxplot"
    factored <- make.column.factor(input$p2_criterion)
    tmp <- cbind(criterion = rep(factored, 26), KWH_DF_LONG)
    p2_boxplot_one <<-
      ggplot(tmp, mapping = aes(x = criterion, y = value, text = criterion)) +
      geom_boxplot() +
      ylab("KWH (1 year)") +
      facet_rep_wrap(
        ~ key,
        ncol = 2,
        repeat.tick.labels = 'bottom',
        scale = "free_y"
      ) +
      xlab(input$p2_criterion) +
      labs(
        title = paste("Consumption usage by", input$p2_criterion),
        subtitle = "All y axis is KWH"
      ) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
    return(
      ggplotly(p2_boxplot_one, height = 400 * 13, width = 400 * 2) %>%
        layout(dragmode = "pan")
    )
  })
  output$p2_download_boxplot_split <- downloadHandler(
    filename = paste0("consumption_usage_by_", input$p2_criterion, ".zip"),
    content = function(filename) {
      factored <- make.column.factor(input$p2_criterion)
      nr.levels <- nlevels(factored)
      file_list = c()
      withProgress(message = "Saving Plots", value = 0, {
        for (col in 1:26) {
          desc <- USAGE_DESC[col]
          tmp <- data.frame(x = factored, y = KWH_DF[, col])
          g <- ggplot(tmp,
                      mapping = aes(y = y, x = x),
                      environment = environment()) +
            geom_boxplot() +
            xlab(input$p2_criterion) +
            ylab(paste("KWH used by", desc)) +
            labs(title = paste("KWH used by", desc)) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
          tmp_filename <-
            paste("KWH_used_by", desc, sep = "", end = ".jpeg")
          incProgress(1 / 26, paste("making", tmp_filename))
          file_list = c(file_list, tmp_filename)
          tmp_filename <- get.temp.path(tmp_filename)
          ggsave(
            filename = tmp_filename,
            plot = g,
            device = "jpeg",
            scale = 3,
            height = 6,
            width = nr.levels * 2,
            units = "cm"
          )
        }
      })
      
      wd.backup <- getwd()
      setwd(TEMP_DIR)
      zip(zipfile = filename, files = file_list)
      setwd(wd.backup)
    },
    contentType = "application/zip"
  )
  output$p2_download_boxplot_combined <- downloadHandler(
    filename = paste(
      "consumption_usage_by",
      input$p2_criterion,
      "(combined).jpeg"
    ),
    content = function(filename) {
      factored <- make.column.factor(input$p2_criterion)
      nr.levels <- nlevels(factored)
      ggsave(
        filename = filename,
        plot = p2_boxplot_one,
        device = "jpeg",
        height = 13 * 6,
        width = nr.levels * 2.5,
        scale = 2.5,
        units = 'cm',
        limitsize = FALSE
      )
      
    }
  )
  output$p2_barplot <- renderPlotly({
    curr_user_view <<- "p2_barplot"
    factored <- make.column.factor(input$p2_criterion)
    nr.levels <- nlevels(factored)
    barplot_list <- list()
    tmp <-
      DATA %>% select(KWH_COL_IDX) %>% mutate(criterion = factored) %>%
      group_by(criterion) %>%
      summarise_all(funs(mean), rm.na = TRUE)
    colnames(tmp)[-1] <- USAGE_DESC
    tmp.long <- gather(tmp, usage, avg_kwh,-criterion)
    p2_barplot_one <<-
      ggplot(tmp.long, mapping = aes(x = usage, y = avg_kwh, fill = criterion)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      labs(title = paste("Bar Plot of consumption usage by",
                         input$p2_criterion)) +
      labs(fill = input$p2_criterion) +
      xlab("Consumption Usage") +
      ylab("Average KWH (1 year)")
    return(
      ggplotly(p2_barplot_one,
               height = 500,
               width = nr.levels * 250) %>%
        layout(dragmode = "pan")
    )
  })
  get.codebook.str <- function(input) {
    desc <- unlist(CODEBOOK[CODEBOOK$name == input$p2_criterion, 2])
    coded <- unlist(CODEBOOK[CODEBOOK$name == input$p2_criterion, 3])
    text_out = ""
    text_out <- paste(input$p2_criterion, end = "\n")
    text_out <- paste(text_out, '  :', desc, end = "\n\n")
    text_out <- paste(text_out, coded, end = "\n")
    return(text_out)
  }
  output$p2_download_barplot_split <- downloadHandler(
    filename = paste0("consumption_usage_by", input$p2_criterion, ".zip"),
    content = function(filename) {
      # create plots
      criterion <- input$p2_criterion
      factored <- make.column.factor(criterion)
      nr.levels <- nlevels(factored)
      level <- levels(factored)
      tmp <-
        DATA %>% select(criterion, KWH_COL_IDX) %>% group_by_(criterion) %>%
        summarise_all(funs(mean), rm.na = TRUE) %>% as.matrix
      # tmp <- tmp %>% slice(1:n()) %>% as.matrix
      tmp <- apply(tmp[, -1], 2, as.numeric) %>% t
      file_list = c()
      for (col in 1:nr.levels) {
        tmp_filename <-
          paste0("[", level[col], "] consumption_usage_bar_plot.jpeg")
        file_list <- c(file_list , tmp_filename)
        tmp_filename <- get.temp.path(tmp_filename)
        curr.vec <- as.vector(tmp[, col])
        print(curr.vec)
        g <- ggplot(mapping = aes(x = USAGE_DESC, y = curr.vec)) +
          geom_bar(stat = "identity") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          xlab("Consumption Usage") +
          ylab("Average KWH (1 year)") +
          ggtitle(paste("Consumption Usage of ", level[col]))
        ggsave(filename = tmp_filename,
               plot = g,
               device = 'jpeg')
      }
      wd.backup <- getwd()
      setwd(TEMP_DIR)
      zip(zipfile = filename, files = file_list)
      setwd(wd.backup)
    },
    contentType = "application/zip"
  )
  output$p2_download_barplot_combined <- downloadHandler(
    filename = paste0(
      "consumption_usage_by",
      input$p2_criterion,
      "(combined).jpeg"
    ),
    content = function(filename) {
      factored <- make.column.factor(input$p2_criterion)
      nr.levels <- nlevels(factored)
      ggsave(
        filename = filename,
        plot = p2_barplot_one,
        device = "jpeg",
        height = 5,
        width = 12 * log(nr.levels, base = 20) + nr.levels / nr.levels,
        scale = 2.5,
        units = 'cm',
        limitsize = FALSE
      )
    }
  )
  
  
  # rendering summary table for 2nd navpane
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
  
  ## panel 3
  output$p3_codebook <- renderDataTable({
    curr_user_view <<- "p3_codebook"
    datatable(CODEBOOK[, -4],
              options = list(paging = TRUE,
                             pageLength = 20))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
