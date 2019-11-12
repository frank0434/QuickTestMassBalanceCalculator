#
# This is the user-interface definition of a Shiny web application. You can
# Define UI for application that draws a histogram
source("global.R")

shinyUI(fluidPage(
  useShinyjs(),
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),


  column(width = 10,
         offset = 1,
         headerPanel('Quick Test Mass Calculator'),
         br(),
         # navbarPage( title = "Page", # tabs on top
         # navlistPanel( # tabs on the right hand side
         tabsetPanel( # tabs on top
           id = "app.tabs",
           tabPanel(h3("Crop info"),
                    value = "crop.info",
             fluidRow(

               br(),
               textOutput("comparsion"),
               column(12,
                      selectInput(inputId = "input_system", label = "Farm System", choices = input_systems, width = width_box)),
               column(12,
                      selectInput(inputId = "input_crop", label = "Crop Type", choices = crops, width = width_box)),
               column(12,
                      dateInput("input_PlantingDate", label = "Planting Date", width = width_box)),
               column(12,
                      dateInput("Sampling.Date", label = "Sampling Date", width = width_box)),
               column(12,
                      dateInput(inputId = "input_nextsamplingDate", label = "Next Sampling Date/Side Dressing", width = width_box)),
               column(12,
                      selectInput("input_componentYield", label = "Harvested component (t FW/ha)", choices =  c(""), width = width_box)
                      ),
               column(12,
                      actionButton("JumpToSoil", "Next >")
                      )
               # column(6,
               #        selectInput("input_targetYield", label = "Target yield (t FW/ha)", choices =  c(""))
               #        ) # disable the total fresh weight value
               )
             ),
           tabPanel(h3("Soil info"),
                    value = "soil.info",
                    br(),
                    fluidRow(
                      column(width = 4,
                             radioButtons(inputId = "samplingDepth",
                                          label = "The Top Layer Sampling Depth",
                                          choices = c(layer.1.1, layer.1), selected = "")),
                      column(width = 2,
                             br(),

                             actionButton("refresh", "Start a New Session?"))
                      ),
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
                                                 actionButton("nextLayer.1.2", "< Previous Layer"),
                                                 actionButton("nextLayer.1.3", "Next Layer >"))
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
                      # conditional panel: inputId must not be the same
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

                    # # Sidebar layout with input and output definitions ----
                    # navlistPanel(id = "depth.tabs",
                    #              widths = c(3,9),
                    #   tabPanel(
                    #            ),
                    #   tabPanel(
                    #   ),
                    # column(12,
                    #        actionButton("resetSoil", "Reset all")),
                    column(12,
                           actionButton("JumpToCrop", "< Back to Crop Information"),
                           actionButton("JumpToReport", "Go To Report >")
                    )
                    ),

           tabPanel(h3("Report"),
                    value = "report.tab",
                    tags$head(
                    tags$style(HTML("
                    .shiny-output-error-validation {
                    color: black;
                    font-size: 20px;
                    font-weight:bold;
                    } "))),

                      column(7,
                             br(),
                             # p(strong(h4("Estimated nitrogen requirement"))),
                             DT::dataTableOutput("N_inCrop")
                             ),
                      column(3,
                               offset = 2,

                               br(),
                               radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'), inline = TRUE),
                               downloadButton("report", "Download Report",
                                              style = "border: 3px solid black; padding:14px; font-size:100%")
                               ),
                        column(12,
                               hr(),
                               # p(strong(h4("Supported information for the nitrogen requirement"))),
                               splitLayout(
                                 DT::dataTableOutput("soil_filtered")
                                 #          DT::dataTableOutput("N_inCrop")# will mask the yield selection input
                                 ),
                      hr(),
                      splitLayout(
                        plotOutput('P_N.uptake'),
                        plotOutput('distPlot2')
                        # DT::dataTableOutput("N_graphing")
                        ),
                      br(),
                      br(),
                      column(width = 12,
                             actionButton("JumpToSoil2", "< Back To Soil Information")))
                    ) # the end of report tab
           )# the end of the main tabset
         ) # column control width end
  ) # fluidpage end
  ) # THE END
