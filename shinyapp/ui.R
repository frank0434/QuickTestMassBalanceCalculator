#
# This is the user-interface definition of a Shiny web application. You can
# Define UI for application that draws a histogram
source("global.R")

shinyUI(fluidPage(
  column(width = 10,
         offset = 1,
         headerPanel('Quick Test Mass Calculator'),
         br(),
         # navbarPage( title = "Page", # tabs on top
         # navlistPanel( # tabs on the right hand side
         tabsetPanel( # tabs on top
           tabPanel(h3("Crop info"),
             fluidRow(

               br(),
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
                      )
               # column(6,
               #        selectInput("input_targetYield", label = "Target yield (t FW/ha)", choices =  c(""))
               #        ) # disable the total fresh weight value
               )
             ),
           tabPanel(h3("Soil info"),

                    br(),



                    # Sidebar layout with input and output definitions ----
                    navlistPanel(id = "depth.tabs",
                                 widths = c(3,9),
                      tabPanel("0 - 15 cm",
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
                                      numericInput(inputId = "AMN1.1", label = "AMN kg/ha @ 0 - 15 cm (if applicable)", min = 0, value = 0))
                               ),
                      tabPanel("15 - 30 cm",
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
                                      numericInput(inputId = "Qtest1.2", label = "Quick test Nitrate result (mg/L)", min = 0, value = 0))
                      ),
                      tabPanel("0 - 30 cm",
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
                                      numericInput(inputId = "AMN1", label = "AMN kg/ha @ 0 - 15 cm (if applicable)", min = 0, value = 0))
                      ),
                      tabPanel("30 - 60 cm",
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
                                      numericInput(inputId = "Qtest2", label = "Quick test Nitrate result (mg/L)", min = 0, value = 0))
                      )
                      )
                    ),

           tabPanel(h3("Report"),
             fixedRow(
               column(7,
                      br(),
                      p(strong(h4("Estimated nitrogen requirement"))),
                      DT::dataTableOutput("N_inCrop")
                      ),
               column(3,
                      offset = 2,
                      br(),
                      br(),
                      radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                                   inline = TRUE),
                      downloadButton("report", "Download Report",
                                   style = "border: 3px solid black; padding:14px; font-size:100%")

               ),


               column(12,
                      hr(),
                      # br(),
                      p(strong(h4("Supported information for the nitrogen requirement"))),
                      splitLayout(
                        DT::dataTableOutput("soil_filtered")

               #          DT::dataTableOutput("N_inCrop")# will mask the yield selection input
               )
               )
               ),
             hr(),
             splitLayout(
               plotOutput('P_N.uptake'),
               plotOutput('distPlot2')
               # DT::dataTableOutput("N_graphing")
               ))
           )# the end of the main tabset
  ) # column control width end
  ) # fluidpage end
  ) # THE END
