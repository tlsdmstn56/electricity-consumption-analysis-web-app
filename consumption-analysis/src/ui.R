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
  tagList(
    p("Data Source: ",
      a(href="https://www.eia.gov/consumption/residential/data/2015/", 
        "RECS2015(Residential Electricity Consumption Survey 2015)")),
    tags$ul(
      tags$li("MONEYPY: income"),
      tags$li("CLIMATE_REGION_PUB: climate(cold, very cold, hot dry,...)"),
      tags$li("TYPEHUQ: Type of housing unit(mobile, single-family, apartment..)"),
      tags$li("UATYP10: Census 2010 Urban Type(Urban, Rural)"),
      tags$li("DIVISION: Census Division(New Englance, Pacific..)"),
      tags$li("HHSEX: Respondent gender"),
      tags$li("HHAGE: Respondent age"),
      tags$li("EMPLOYHH: Respondent employment status(fulltime, part time..)")
    ),
    p("eunsoo.sheen@encoredtech.com for any feedback")
  ),
  # Navigation Bar on the top
  navbarPage(title = "Menu",
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
                                     plotlyOutput("p1_boxplot") %>% withSpinner
                            ),
                            tabPanel("Summary",
                                     verbatimTextOutput("p1_summary")),
                            tabPanel("Anova Test", 
                                     verbatimTextOutput("p1_aov")),
                            id = "p1_tabs"
                          )) # end of main panel
                      ),
                      value="p1"),
             # Consumption Usage tab
             tabPanel("Consumption Usage",
                      sidebarLayout(
                        # for group selection
                        sidebarPanel(
                          selectInput("p2_criterion", "Criterion",
                                      sort(COLNAMES_IN_DROPDOWN[1:242])),
                          htmlOutput("p2_var_desc"),
                          htmlOutput("p2_legend")),
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
                                     plotlyOutput("p2_boxplot") %>% withSpinner,
                                     value = "box"
                                     
                            ) ,
                            
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
                                     plotlyOutput("p2_barplot"
                                                  #, width=1500, height=700
                                                  ) %>% 
                                       withSpinner,
                                     value = "bar"),
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
                                     tableOutput("p2_summary_percentage"),
                                     value = "summary"
                                    ),
                            id="p2_tabs"
                            ) # end of p2 tabsetpanel
                          ,style="overflow-y:scroll") # end of main panel
                      ),
                      value="p2"),
             # code book for variable description
             tabPanel("Codebook", dataTableOutput('p3_codebook'),
                      value="p3"),
             id = 'main_nav')
)