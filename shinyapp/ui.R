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
         headerPanel('Quick Test Mass Calculator'),
         br(),
         # navbarPage( title = "Page", # tabs on top
         # navlistPanel( # tabs on the right hand side
         tabsetPanel( # tabs on top
           id = "app.tabs",
           #beginning of the tabs
           # crop tab ----
           tabPanel(h3("Crop info"),
                    value = "crop.info",

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
             ),
           # soil tab ----
           tabPanel(h3("Soil info"),
                    value = "soil.info",
                    br(),
                    #two top layer options for user to choose the sampling method
                    fluidRow(
                      column(width = 4,
                             radioButtons(inputId = "samplingDepth",
                                          label = "The Top Layer Sampling Depth",
                                          choices = c(layer.1.1, layer.1), selected = "")),
                      # the refresh page button ----
                      column(width = 4,
                             br(),
                             actionButton("refresh", "Start a New Session?"))
                      ),
                    #the conditionalPanel will show the desired sampling depth after user's selection
                    conditionalPanel(
                      condition = "input.samplingDepth == '0-15 cm'",
                      navbarPage(title = "Sampling Depth",
                                 id = "soil.tabset.layer.1.1",
                                 tabPanel("0 - 15 cm",
                                          value = "Panel.1.1", # the value is link back to the server
                                          column(width = 12,
                                                 selectizeInput(inputId = "Texture.1.1", label = "Soil Texture", choices = soil.texture,
                                                                options = list(
                                                                  placeholder = 'Please select an option below',
                                                                  onInitialize = I('function() { this.setValue(""); }')))),
                                          column(width = 12,
                                                 selectizeInput(inputId = "Moisture.1.1", label = "Soil Moisture", choices = soil.moisture,
                                                                options = list(
                                                                  placeholder = 'Please select an option below',
                                                                  onInitialize = I('function() { this.setValue(""); }')))),
                                          column(width = 12,
                                                 numericInput(inputId = "Qtest1.1", label = "Quick test Nitrate result (mg/L)", min = 0, value = 0)),
                                          column(width = 12,
                                                 numericInput(inputId = "AMN1.1", label = "AMN kg/ha @ 0 - 15 cm (if applicable)", min = 0, value = 0)),
                                          column(12,
                                                 actionButton("nextLayer.1.1", "Next Layer >"))
                                          ),
                                 tabPanel("15 - 30 cm",
                                          value = "Panel.1.2",
                                          column(width = 12,
                                                 selectizeInput(inputId = "Texture.1.2", label = "Soil Texture", choices = soil.texture,
                                                                options = list(
                                                                  placeholder = 'Please select an option below',
                                                                  onInitialize = I('function() { this.setValue(""); }')))),
                                          column(width = 12,
                                                 selectizeInput(inputId = "Moisture.1.2", label = "Soil Moisture", choices = soil.moisture,
                                                                options = list(
                                                                  placeholder = 'Please select an option below',
                                                                  onInitialize = I('function() { this.setValue(""); }')))),
                                          column(width = 12,
                                                 numericInput(inputId = "Qtest1.2", label = "Quick test Nitrate result (mg/L)", min = 0, value = 0)),

                                          column(12,
                                                 actionButton("nextLayer.1.3", "Next Layer >")),

                                          column(12,
                                                 actionButton("nextLayer.1.2", "< Previous Layer"))

                                          ),
                                 tabPanel("30 - 60 cm",value = "Panel.2",
                                          column(width = 12,
                                                 selectizeInput(inputId = "Texture.2", label = "Soil Texture", choices = soil.texture,
                                                                options = list(
                                                                  placeholder = 'Please select an option below',
                                                                  onInitialize = I('function() { this.setValue(""); }')))),
                                          column(width = 12,
                                                 selectizeInput(inputId = "Moisture.2", label = "Soil Moisture", choices = soil.moisture,
                                                                options = list(
                                                                  placeholder = 'Please select an option below',
                                                                  onInitialize = I('function() { this.setValue(""); }')))),
                                          column(width = 12,
                                                 numericInput(inputId = "Qtest2", label = "Quick test Nitrate result (mg/L)", min = 0, value = 0)),
                                          column(12,
                                                 actionButton("nextLayer.1.4", "< Previous Layer"))
                                 )
                      )
                    ),
                    conditionalPanel(
                      condition = "input.samplingDepth == '0-30 cm'",
                      navbarPage(title = "Sampling Depth",
                                 id = "soil.tabset.layer.1",
                                 tabPanel("0 - 30 cm",value = "Panel.1",
                                          column(width = 12,
                                                 selectizeInput(inputId = "Texture.1", label = "Soil Texture", choices = soil.texture,
                                                                options = list(
                                                                  placeholder = 'Please select an option below',
                                                                  onInitialize = I('function() { this.setValue(""); }')))),
                                          column(width = 12,
                                                 selectizeInput(inputId = "Moisture.1", label = "Soil Moisture", choices = soil.moisture,
                                                                options = list(
                                                                  placeholder = 'Please select an option below',
                                                                  onInitialize = I('function() { this.setValue(""); }')))),
                                          column(width = 12,
                                                 numericInput(inputId = "Qtest1", label = "Quick test Nitrate result (mg/L)",min = 0, value = 0)),
                                          column(width = 12,
                                                 numericInput(inputId = "AMN1", label = "AMN kg/ha @ 0 - 15 cm (if applicable)", min = 0, value = 0)),
                                          column(12,
                                                 actionButton("nextLayer.1", "Next Layer >"))
                                          ),
                                 tabPanel("30 - 60 cm",value = "Panel.2.1",
                                          column(width = 12,
                                                 selectizeInput(inputId = "Texture.2.1", label = "Soil Texture", choices = soil.texture,
                                                                options = list(
                                                                  placeholder = 'Please select an option below',
                                                                  onInitialize = I('function() { this.setValue(""); }')))),
                                          column(width = 12,
                                                 selectizeInput(inputId = "Moisture.2.1", label = "Soil Moisture", choices = soil.moisture,
                                                                options = list(
                                                                  placeholder = 'Please select an option below',
                                                                  onInitialize = I('function() { this.setValue(""); }')))),
                                          column(width = 12,
                                                 numericInput(inputId = "Qtest2.1", label = "Quick test Nitrate result (mg/L)", min = 0, value = 0)),
                                          column(12,
                                                 actionButton("nextLayer.2", "< Previous Layer"))
                                          )
                                 )
                      ), # the end of 0-30 conditional page

                    br(),
                    br(),
                    br(),
                    column(12,
                           actionButton("JumpToReport", "Go To Report >")),
                    column(12,
                           actionButton("JumpToCrop", "< Back to Crop Information"))

                    ),
           # report tab ----
           tabPanel(h3("Report"),
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
                      column(5,
                             br(),
                             # p(strong(h4("Estimated nitrogen requirement"))),
                             DT::dataTableOutput("N_inCrop"),
                             br(),
                             DT::dataTableOutput("report.table2")
                             ),
                    column(5,offset = 2,
                      verticalLayout(column(5,
                           br(),
                           radioButtons('format_data', 'File format', c('csv', 'Excel'), inline = TRUE),
                           downloadButton("qTestResults.csv", "Download Test Results")
                           ),
                    column(5,
                           br(),
                           radioButtons('format', 'Document format', c('PDF', 'Word'), inline = TRUE),
                           downloadButton("report", "Download Report")
                           ))))),
                    column(12,
                           hr(),
                           # p(strong(h4("Supported information for the nitrogen requirement"))),
                           splitLayout(
                             DT::dataTableOutput("soil_filtered")
                             #          DT::dataTableOutput("N_inCrop")# will mask the yield selection input
                             ), # splitlayout
                           br(),
                           hr(),
                           splitLayout(cellWidths = c("47%","52%"),
                             plotOutput('distPlot2'),
                             plotOutput('P_N.uptake')

                             ),
                           br(),
                           br(),
                           column(width = 12,
                             actionButton("JumpToSoil2", "< Back To Soil Information")))
                    ) # the end of report tab
           # tabPanel(h3("debugging tab"),
           #          textOutput("df_AMN"),
           #          textOutput("df_days"),
           #          DT::dataTableOutput("df_graph"),
           #          DT::dataTableOutput("df_graph2")
           #          ) # the end of report tab
           )# the end of the main tabset
         ) # column control width end
  ) # fluidpage end
  ) # THE END
