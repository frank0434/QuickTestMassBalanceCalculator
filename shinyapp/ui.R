# draw a UI plan first on the paper first.
# Note:
# 1. all id for inputs or tabs should be unique and have cooresponding objects in the server
# 2. full page is width 12. only integer is acceptable
# 3. conditionalPanel won't work properly if there are duplicated `id` names in the UI and server
# 4. `inline` argument usually to control the text direaction, either horizontal or vertical
# 5. The styling text seems only to take affect after restart R session.

# source the global file for necessary pkgs and input data
source("global.R")

# the UI
shinyUI(fluidPage(

  #use shinyjs package for refresh feature
  useShinyjs(),
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),

  #beginning of the page layout. column to control the width of the layout

  column(width = 10,
         offset = 1,
         headerPanel('Quick Test Mass Balance Calculator'),
         br(),
         # navbarPage( title = "Page", # tabs on top
         # navlistPanel( # tabs on the right hand side
         tabsetPanel( # tabs on top
           id = "app.tabs",
           #beginning of the tabs
           # crop tab ----
           tabPanel(h3("1. Crop info"),
                    value = "crop.info",
                    br(),

                    fluidRow(
                      column(width = 4,
                             radioButtons(inputId = "FallowOrCropping",
                                          label = "Paddock Status",
                                          choices = c("Fallow", "Cropping"), selected = " ")),
                      # the refresh page button ----
                      column(width = 4,
                             br(),
                             actionButton("refresh", "Start a New Session?"))
                    ),
                    conditionalPanel(
                      condition = "input.FallowOrCropping == 'Fallow'",

                      #the internal layout for fallow
                        fluidRow(
                          br(),
                          #define the style of the action button - this affects all the buttons
                          tags$head(
                            tags$style(HTML(btn_style))),
                          #the selectInput layout
                          column(12,
                                 selectInput(inputId = "input_system_fallow", label = "Farm System", choices = input_systems, width = width_box)),
                          column(12,
                                 dateInput("Sampling.Date_fallow", label = "Sampling Date", width = width_box, format = "d MM yyyy")),
                          column(12,
                                 dateInput(inputId = "input_nextsamplingDate_fallow", label = "Next Sampling Date", width = width_box, format = "d MM yyyy")),
                          column(12,
                                 textInput(inputId = "input_paddock.id_fallow", label = "Paddock Name/Number (Optional)", width = width_box)),
                          column(12,
                                 actionButton("JumpToSoil_fallow", "Next >")
                          )
                        )
                      ),
                    conditionalPanel(
                      condition = "input.FallowOrCropping == 'Cropping'",

                      #the internal layout of the first tab
                      fluidRow(
                        br(),
                        #define the style of the action button - this affects all the buttons
                        tags$head(
                          tags$style(HTML(btn_style))),
                        #the selectInput layout
                        column(12,
                               selectInput(inputId = "input_system", label = "Farm System", choices = input_systems, width = width_box)),
                        column(12,
                               selectInput(inputId = "input_crop", label = "Crop Type", choices = crops, width = width_box)),
                        column(12,
                               dateInput("input_PlantingDate", label = "Planting Date", width = width_box, format = "d MM yyyy")),
                        column(12,
                               dateInput("Sampling.Date", label = "Sampling Date", width = width_box, format = "d MM yyyy")),
                        column(12,
                               dateInput(inputId = "input_nextsamplingDate", label = "Next Sampling Date/Side Dressing", width = width_box, format = "d MM yyyy")),
                        column(12,
                               selectInput("input_componentYield", label = "Harvested component (t FW/ha)", choices =  c(""), width = width_box)
                        ),
                        column(12,
                               textInput(inputId = "input_paddock.id", label = "Paddock Name/Number (Optional)", width = width_box)),
                        column(12,
                               actionButton("JumpToSoil", "Next >")
                        )
                      )


                    )

             ),
           # soil tab ----
           tabPanel(h3("2. Soil info"),
                    value = "soil.info",
                    br(),

                    navbarPage(title = "Sampling Depth",
                               id = "soil.tabset.layer.1.1",
                               collapsible = TRUE,
                               # theme = "www/bootstrap.min.css",
                               tabPanel("TOP", # 0-15 cm layer ----
                                        value = "Panel.1.1", # the value is link back to the server
                                        column(width = 12,
                                               numericInput(inputId = "samplingDepth1.1",
                                                            label = p("Sampling depth start (cm)"),
                                                            value = 0, min = 0, max = 0
                                                            )),
                                          column(width = 12,
                                                 numericInput(inputId = "samplingDepth1.2",
                                                                label = p("Sampling depth end (cm)"),
                                                                value = "", min = 10, max = 100, step = 5
                                                              )),
                                          column(width = 12,
                                                 selectInput.soilProperty(id = "Texture.1",
                                                                          label = "Soil Texture",
                                                                          choices = soil.texture)),
                                          column(width = 12,
                                                 selectInput.soilProperty(id = "Moisture.1",
                                                                          label = "Soil Moisture",
                                                                          choices = soil.moisture)),
                                          column(width = 12,
                                                 div(style='display: inline-block;',
                                                     numericInput(inputId = "Qtest1",
                                                                  label = "Quick test result in Nitrate-N (mg/L)",
                                                                  min = 0, value = 0),
                                                     img(src=b64,style='display: inline-block;')
                                                     )),
                                        column(width = 12,
                                               br()),
                                        column(width = 12,
                                               numericInput(inputId = "AMN1.1",
                                                            label = p("AMN kg/ha @ 0 - 15 cm (if applicable)\r",
                                                                      em('Anaerobic Mineralisable N',
                                                                         a('(More details)', href = 'https://www.far.org.nz/assets/files/blog/files//e7b9c43f-c4f6-52cb-b0f9-1e9e6539bb91.pdf', target = "_blank"
                                                                           ))),
                                                            min = 0, value = 0)),
                                        column(12,
                                               actionButton("nextLayer.1.1", "Next Layer >"))
                                          ),
                                 tabPanel("MIDDLE", # 15 -30 cm -------
                                          value = "Panel.1.2",
                                          column(width = 12,
                                                 numericInput(inputId = "samplingDepth2.1",
                                                              label = p("Sampling depth start (cm)"),
                                                              value = "", min = 10, max = 60, step = 10
                                                 )),
                                          column(width = 12,
                                                 numericInput(inputId = "samplingDepth2.2",
                                                              label = p("Sampling depth end (cm)"),
                                                              value = "", min = 20, max = 100, step = 10
                                                 )),
                                          column(width = 12,
                                                 selectInput.soilProperty(id = "Texture.2",
                                                                          label = "Soil Texture")),
                                          column(width = 12,
                                                 selectInput.soilProperty(id = "Moisture.2",
                                                                          label = "Soil Moisture",
                                                                          choices = soil.moisture)),
                                          column(width = 12,
                                                 div(qstyle='display: inline-block;',
                                                     numericInput(inputId = "Qtest2",
                                                                  label = "Quick test result in Nitrate-N (mg/L)",
                                                                  min = 0, value = 0),
                                                     img(src=b64,style='display: inline-block;')
                                                 )),
                                          column(width = 12,
                                                 br()),
                                          column(12,
                                                 actionButton("nextLayer.1.3", "Next Layer >")),

                                          column(12,
                                                 actionButton("nextLayer.1.2", "< Previous Layer"))

                                          ),
                                 tabPanel("DEEP (optional)", # 30-60 cm layer -------
                                          value = "Panel.2",
                                          column(width = 12,
                                                 numericInput(inputId = "samplingDepth3.1",
                                                              label = p("Sampling depth start (cm)"),
                                                              value = "", min = 20, max = 100, step = 10
                                                 )),
                                          column(width = 12,
                                                 numericInput(inputId = "samplingDepth3.2",
                                                              label = p("Sampling depth end (cm)"),
                                                              value = "", min = 31, max = 100, step = 10
                                                 )),
                                          column(width = 12,
                                                 selectInput.soilProperty(id = "Texture.3",
                                                                          label = "Soil Texture",
                                                                          choices = soil.texture)),
                                          column(width = 12,
                                                 selectInput.soilProperty(id = "Moisture.3",
                                                                          label = "Soil Moisture",
                                                                          choices = soil.moisture)),
                                          column(width = 12,
                                                 div(style='display: inline-block;',
                                                     numericInput(inputId = "Qtest3",
                                                                  label = "Quick test result in Nitrate-N (mg/L)",
                                                                  min = 0, value = 0),
                                                     img(src=b64,style='display: inline-block;')
                                                 )),
                                          column(width = 12,
                                                 br()),
                                          column(12,
                                                 actionButton("nextLayer.1.4", "< Previous Layer"))
                                 ),
                               tabPanel(p(strong("Restart")),
                                        value = "Restartpanel",
                                        column(width = 12,
                                               actionButton("refresh.soil", "Restart?"))
                               )
                      ),
                    br(),
                    br(),
                    br(),
                    column(12,
                           actionButton("JumpToReport", "Go To Report >")),
                    column(12,
                           actionButton("JumpToCrop", "< Back to Crop Information"))

                    ),
           # report tab ----
           tabPanel(h3("3. Report"),
                    value = "report.tab",
                    # define the style of feedback message
                    tags$head(
                    tags$style(HTML("
                    .shiny-output-error-validation {
                    color: red;
                    font-size: 20px;
                    font-weight:bold;
                    } "))),
                    column(12,
                      fluidRow(
                        conditionalPanel(
                          condition = "input.FallowOrCropping == 'Cropping'",
                          column(5,
                                 br(),
                                 # p(strong(h4("Estimated nitrogen requirement"))),
                                 DT::dataTableOutput("N_inCrop"),
                                 br(),
                                 DT::dataTableOutput("report.table2")
                                 )),
                        column(5,offset = 2,
                               br(),

                               verticalLayout(column(5,
                                                     br(),
                                                     # radioButtons('format_data', 'File format', c('csv', 'Excel'), inline = TRUE),
                                                     downloadButton("qTestResults.csv", "Download Test Results")),
                                              column(5,
                                                     br(),
                                                     # radioButtons('format', 'Document format', c('PDF', 'Word'), inline = TRUE),
                                                     downloadButton("report", "Download Report"))
                                              )))),
                    column(12,
                           hr(),
                           # p(strong(h4("Supported information for the nitrogen requirement"))),
                           splitLayout(
                             DT::dataTableOutput("soil_filtered")
                             #          DT::dataTableOutput("N_inCrop")# will mask the yield selection input
                             ), # splitlayout
                           br(),
                           hr(),
                           fluidRow(
                             column(width = 6,
                                    plotOutput('distPlot2')),
                             conditionalPanel(
                               condition = "input.FallowOrCropping == 'Cropping'",
                               column(width = 6,
                                      plotOutput('P_N.uptake')))
                             ),
                           br(),
                           br(),
                           column(width = 12,
                             actionButton("JumpToSoil2", "< Back To Soil Information"),
                             p("Source Code on ",
                               a('Github Page.', href = 'https://github.com/frank0434/QuickTestMassBalanceCalculator', target = "_blank"))))
                    ) # the end of report tab
           # tabPanel(h3("debugging tab"),
           #          textOutput("df_AMN"),
           #          textOutput("df_days"),
           #          textOutput("crop_period"),
           #          DT::dataTableOutput("df_graph"),
           #          DT::dataTableOutput("df_graph2")
           #          ) # the end of report tab
           )# the end of the main tabset
         ) # column control width end
  ) # fluidpage end
  ) # THE END
