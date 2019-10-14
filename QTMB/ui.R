#
# This is the user-interface definition of a Shiny web application. You can



# Define UI for application that draws a histogram
source("global.R")

shinyUI(fluidPage(
  theme = "style.css",
  headerPanel('The Nitrate Quick Test Mass Balance Tool'),
  hr(),

  fluidRow(
    column(5,
           fixedRow(
             p(strong(h4("Crop"))),
             hr(),
             column(6,
                    selectInput(inputId = "input_system",
                                label = "System",
                                choices = input_systems)),
             column(4,
                    selectInput(inputId = "input_crop", label = "Crop", choices = crops)),
             column(6,
                    dateInput("input_PlantingDate", label = "Planting Date")),
             column(4,
                    selectInput("input_targetYield", label = "Target yield (t FW/ha)", choices =  c(""))),
             column(6,
                    htmlOutput("Harvested_value")),
             column(6,
                    htmlOutput("N_uptake_estimated"))
           ),

           hr(),
           fixedRow(
             p(strong(h4("Seasonal N Balance"))),
             hr(),
             DT::dataTableOutput("N_inCrop")
             # column(6,
             #        numericInput(inputId = "1", label = "Soil N supply",value = 0)),
             # column(4,
             #        numericInput(inputId = "2", label = "Remaining Crop N requirement",value = 0)),
             # column(6,
             #        textInput("3", label = "Net", value = paste0(000,"surplus") ))
           ),
           hr(),
           fixedRow(
             p(strong(h4("Next sampling date (SD)"))),
             hr(),
             column(10,
                    dateInput(inputId = "input_nextsamplingDate",
                              label = "Next SD")),
             DT::dataTableOutput("N_require")
             # column(6,
             #        numericInput(inputId = "2",
             #                     label = "Crop N Requirement Until next SD (kg/ha)", value = 0)),
             # column(6,
             #        numericInput("date1", label = "Net (kg/ha)",value = 0 ))


           )

    ),
    column(7,
           p(strong(h4("Soil Nitrogen"))),
           hr(),
           p(em("Nitrate Quick Test")),

           dateInput("Sampling.Date", label = "Sampling Date"),

           br(),
           fixedRow(
             column(4,
                    p(strong(h5("Sampling Depth"))),
                    verticalLayout(
                      sliderInput(inputId = "depth.1", label = "Depth 1", min = 0, max = 30, value = 15,step = 5),
                      sliderInput(inputId = "depth.2", label = "Depth 2", min = 30, max = 60, value = 45,step = 5),
                      sliderInput(inputId = "depth.3", label = "Depth 3", min = 60, max = 90, value = 75,step = 5)
                    )
             ),
             column(2,
                    p(strong(h5("Soil Texture"))),
                    verticalLayout(
                      selectInput(inputId = "Texture.1", label = "Depth 1", choices = ""),
                      selectInput(inputId = "Texture.2", label = "Depth 2", choices = ""),
                      selectInput(inputId = "Texture.3", label = "Depth 3", choices = "")
                    )
             ),
             column(2,
                    p(strong(h5("Soil Moisture"))),
                    verticalLayout(
                      selectInput(inputId = "Moisture.1", label = "Depth 1", choices = ""),
                      selectInput(inputId = "Moisture.2", label = "Depth 2", choices = ""),
                      selectInput(inputId = "Moisture.3", label = "Depth 3", choices = "")
                    )
             ),
             column(2,
                    p(strong(h5("QtestNitrate"))),
                    verticalLayout(
                      numericInput(inputId = "Qtest1", label = "Depth 1",value = 0),
                      numericInput(inputId = "Qtest2", label = "Depth 2", value = 0),
                      numericInput(inputId = "Qtest3", label = "Depth 3",value = 0)
                    )
             ),
             column(2,
                    p(strong(h5("AMN"))),
                    verticalLayout(
                      numericInput(inputId = "AMN1", label = "Depth 1",value = 0),
                      textInput(inputId = "AMN2", label = "Depth 2", placeholder = "Place holder"),
                      textInput(inputId = "AMN3", label = "Depth 3", placeholder = "Place holder")
                    )
             ),
             column(12,
                    htmlOutput("period")),
             br(),
             column(12,

                    DT::dataTableOutput("N.calculated")
                    )
           )
    )),
  hr(),
  fixedRow(
    column(width = 5,
           plotOutput('P_N.uptake')
    ),
    column(width = 5, offset = 1,
           plotOutput('distPlot2')
           # DT::dataTableOutput("N_graphing")
    )
  )

))
