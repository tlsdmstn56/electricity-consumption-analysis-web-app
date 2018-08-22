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
FLEX_CONTAINER_STYLE <- paste0(
  "display: flex; flex-wrap: wrap; ",
  "justify-content: space-evenly; ",
  "align-items: baseline;"
)

CHOICE_LIST <- list()
for (i in 1:N_USAGE_DESC) {
  CHOICE_LIST[[USAGE_DESC[i]]] <- i
}

div.flex.container <- function(...) {
  return(div(..., style = FLEX_CONTAINER_STYLE))
}

div.flex.element <-
  function(...,
           style = "",
           height = 400,
           width = 400,
           margin = 30) {
    style <- paste0(style, "height:", height, "px; ")
    style <- paste0(style, "width:", width, "px; ")
    style <- paste0(style, "margin:", margin, "px; ")
    return(div(..., style = style))
  }

ui.add.help.icon <- function(name, id) {
  return(
      list(
      h3(name,style="display: inline;"),
      span(icon("info-circle"), id = id, style="display: inline;"),
      bsTooltip(id=id, title=TOOLTIP_CONTENT, 
                placement = "right", trigger = "hover",
                option=list(container='body'))
      )
  )
}

HEAD <- tags$head(
  tags$script(
    '
      var dimension = [0, 0];
      $(document).on("shiny:connected", function(e) {
      dimension[0] = window.innerWidth;
      dimension[1] = window.innerHeight;
      Shiny.onInputChange("dimension", dimension);
      });
      $(window).resize(function(e) {
      dimension[0] = window.innerWidth;
      dimension[1] = window.innerHeight;
      Shiny.onInputChange("dimension", dimension);
      });
      '
  )
)

TOOLTIP_CONTENT <- paste("<div style=\"text-align: left;\">[Frequnetly Used Variable]",
                         "* MONEYPY: income",
                         "* CLIMATE_REGION_PUB: climate(cold, very cold, hot dry,...)",
                         "* TYPEHUQ: Type of housing unit(mobile, single-family, apartment..)",
                         "* UATYP10: Census 2010 Urban Type(Urban, Rural)",
                         "* DIVISION: Census Division(New Englance, Pacific..)",
                         "* HHSEX: Respondent gender",
                         "* HHAGE: Respondent age",
                         "* EMPLOYHH: Respondent employment status(fulltime, part time..)",
                         "</div>",
                         sep = "<br>")

HEADER <- list(titlePanel("Consumption Analysis", windowTitle = "Consumption Analysis"),
# guide(frequent used variables, data source, some infos)
tagList(
  p(
    "Data Source: ",
    a(
      href = "https://www.eia.gov/consumption/residential/data/2015/",
      "RECS2015(Residential Electricity Consumption Survey 2015)"
    )
  ),
  p("eunsoo.sheen@gmail.com for any feedback")
))

PANEL1_SIDE <- list(
  sidebarPanel(
    h3("Auto-sizing Plot"),
    switchInput(inputId = "p1_box_autosize", value =
                  TRUE),
    conditionalPanel(
      condition = "!input.p1_box_autosize",
      sliderInput(
        "p1_box_height",
        "Height:",
        min = 0,
        max = 1000,
        value = 600
      ),
      sliderInput(
        "p1_box_width",
        "Width:",
        min = 0,
        max = 2000,
        value = 800
      )
    ),
    hr(),
    # first group
    ui.add.help.icon("X variable","p1_c1_help"),
    pickerInput(inputId = "p1_criterion1", 
                choices = DROPDOWN_MENU, 
                options = list(`live-search` = TRUE,
                               `actions-box` = TRUE,
                               size = 10)),
    # second group
    ui.add.help.icon("Color Variable","p1_c2_help"),
    pickerInput(inputId = "p1_criterion2",
                choices = c('None', DROPDOWN_MENU), 
                options = list(`live-search` = TRUE,
                               `actions-box` = TRUE,
                               size = 10)),
    # group description
    htmlOutput("p1_var_desc"),
    width = 3
  )
)

PANEL1_MAIN <- list(mainPanel(tabsetPanel(
  tabPanel("Box Plot",
           plotlyOutput("p1_boxplot") %>% withSpinner),
  tabPanel("Summary",
           uiOutput("p1_summary")),
  tabPanel("Anova Test",
           uiOutput("p1_aov")),
  id = "p1_tabs"
),width = 9))


PANEL2_SIDE <- list(
  sidebarPanel(
    conditionalPanel("input.p2_tabs!='summary'",
                     h3("Auto-sizing Plot")),
    conditionalPanel(
      condition = "input.p2_tabs=='box'",
      switchInput(inputId = "p2_box_autosize", 
                  value = TRUE),
      conditionalPanel(
        "!input.p2_box_autosize",
        sliderInput(
          "p2_box_height",
          "Height:",
          min = 400,
          max = 1000,
          value = 500
        ),
        sliderInput(
          "p2_box_width",
          "Width:",
          min = 400,
          max = 1200,
          value = 500
        ),
        sliderInput(
          "p2_box_bottom",
          "Bottom Margin (if x label is not visible):",
          min = 50,
          max = 200,
          value = 80
        ),
        sliderInput(
          "p2_box_right",
          "Right Margin (if x label is not visible):",
          min = 50,
          max = 200,
          value = 100
        )
      ),
      hr()
    ), # end of conditional Panel
    conditionalPanel(
      "input.p2_tabs=='bar'",
      switchInput(inputId = "p2_bar_autosize", value = TRUE),
      conditionalPanel(
        "!input.p2_bar_autosize",
        sliderInput(
          "p2_bar_height",
          "Height:",
          min = 400,
          max = 1000,
          value = 500
        ),
        sliderInput(
          "p2_bar_width",
          "Width:",
          min = 400,
          max = 1200,
          value = 500
        ),
        sliderInput(
          "p2_bar_right",
          "Right Margin (if x label is not visible):",
          min = 50,
          max = 200,
          value = 100
        ),
        sliderInput(
          "p2_bar_bottom",
          "Bottom Margin (if x label is not visible):",
          min = 0,
          max = 200,
          value = 80
        ),
        sliderInput(
          "p2_bar_legend_loc",
          "Legend Vertical Location:",
          min = -2,
          max = 3,
          value = -0.3,
          step=0.1
        )
      ),
      hr()
    ),
    ui.add.help.icon("X axis","p2_c_help"),
    pickerInput(inputId = "p2_criterion", 
                choices = sort(COLNAMES_IN_DROPDOWN[1:242]), 
                options = list(`live-search` = TRUE,
                               `actions-box` = TRUE,
                               size = 10)),
    conditionalPanel(
      condition = "input.p2_tabs=='box'",
      pickerInput(inputId = "p2_box_y", 
                  label = h3("Y axis"), 
                  choices = CHOICE_LIST, 
                  options = list(`live-search` = TRUE,
                                 `actions-box` = TRUE,
                                 `deselect-all-text` = "deselect all",
                                 `select-all-text` = "select all",
                                 `selected-text-format` = "count > 1",
                                 size = 10),
                  multiple = TRUE,
                  selected = CHOICE_LIST[1]
                  )
      # checkboxGroupInput(
      #   "p2_box_y",
      #   label = h3("Y axis"),
      #   choices = CHOICE_LIST,
      #   selected = 1
      # )
    ),
    htmlOutput("p2_var_desc"),
    width = 3
  )
)

# TODO button

ui <- fluidPage(
  HEAD,
  HEADER,
  # Navigation Bar on the top
  navbarPage(
    title = "",
    # Total Consumption Tab
    tabPanel(
      "Total Consumption",
      sidebarLayout(
        PANEL1_SIDE,
        # plots and summary, anova
        PANEL1_MAIN
      ),
      value = "p1"
    ),
    # Consumption Usage tab
    tabPanel(
      "Consumption Usage",
      sidebarLayout(
        # for group selection
        PANEL2_SIDE,
        mainPanel(
          tabsetPanel(
          tabPanel("Box Plot",
                   uiOutput('p2_boxplots_ui'), value = "box"),
          tabPanel(
            "Bar Chart",
            plotlyOutput("p2_barplots"),
            value = "bar"
            ),
          tabPanel(
              "Summary Table",
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
                downloadButton(
                  "p2_download_summary_percentage",
                  "Download(percentage, csv)"
                )
              ), 
              br(),
              tableOutput("p2_summary_percentage"),
              value = "summary"
            ),
            id = "p2_tabs"
          ),width = 9) # end of p2 tabsetpanel) # end of main panel
      ),
        value = "p2"
        ),
        # code book for variable description
        tabPanel("Codebook", dataTableOutput('p3_codebook'),
                 value = "p3"),
        id = 'main_nav'
      ) 
    )
