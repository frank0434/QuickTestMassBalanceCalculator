#
# This is the user-interface definition of a Shiny web application. You can



# Define UI for application that draws a histogram
source("global.R")

shinyUI(fluidPage(
  theme = "style.css",
  headerPanel('Quick Test Mass Calculator'),
  hr(),
  sidebarLayout(
    sidebarPanel(
      fixedRow(
        p(strong(h4("Input"))),


        hr(),
        column(6,
               selectInput(inputId = "input_system",
                           label = "System",
                           choices = input_systems)),
        column(6,
               selectInput(inputId = "input_crop", label = "Crop", choices = crops)),
        column(4,
               dateInput("input_PlantingDate", label = "Planting Date")),
        column(4,
               dateInput("Sampling.Date", label = "Sampling Date")),
        column(4,
               dateInput(inputId = "input_nextsamplingDate",
                         label = "Next Sampling Date")),
        column(6,
               selectInput("input_targetYield", label = "Target yield (t FW/ha)", choices =  c(""))),
        column(6,
               selectInput("input_componentYield", label = "Harvested component (t FW/ha)", choices =  c(""))
               # htmlOutput("Harvested_value")
               ),
        column(10,
               p(strong(h5("Sampling Depth"))),

               tags$form(
                 class="form-horizontal",

                 tags$div(
                   class="form-group",
                   tags$label(class = "col-sm-4 control-label", `for` = "depth.1"),

                 sliderInput(inputId = "depth.1", label = "Depth 1", min = 0, max = 30, value = 15,step = 5),
                 sliderInput(inputId = "depth.2", label = "Depth 2", min = 30, max = 60, value = 45,step = 5),
                 sliderInput(inputId = "depth.3", label = "Depth 3", min = 60, max = 90, value = 75,step = 5)
               )))
        )
      ) ,

  mainPanel(
    sidebarLayout(
      sidebarPanel(width = 6,
                   p(strong(h4("Input"))),


                   hr(),
                   fixedRow(
                     column(6,
                            p(strong(h5("Soil Texture"))),
                            verticalLayout(
                              selectInput(inputId = "Texture.1", label = "Depth 1", choices = ""),
                              selectInput(inputId = "Texture.2", label = "Depth 2", choices = ""),
                              selectInput(inputId = "Texture.3", label = "Depth 3", choices = "")
                            )),
                     column(6,
                            p(strong(h5("Soil Moisture"))),
                            verticalLayout(
                              selectInput(inputId = "Moisture.1", label = "Depth 1", choices = ""),
                              selectInput(inputId = "Moisture.2", label = "Depth 2", choices = ""),
                              selectInput(inputId = "Moisture.3", label = "Depth 3", choices = "")
                            )),

                     column(6,
                            p(strong(h5("QtestNitrate"))),
                            verticalLayout(
                              numericInput(inputId = "Qtest1", label = "Depth 1",value = 0),
                              numericInput(inputId = "Qtest2", label = "Depth 2", value = 0),
                              numericInput(inputId = "Qtest3", label = "Depth 3",value = 0)
                            )),
                     column(6,
                            p(strong(h5("AMN"))),
                            verticalLayout(
                              numericInput(inputId = "AMN1", label = "Depth 1",value = 0),
                              textInput(inputId = "AMN2", label = "Depth 2", placeholder = "Place holder"),
                              textInput(inputId = "AMN3", label = "Depth 3", placeholder = "Place holder")
                            ))

                   )),
      mainPanel(
        p(strong(h4("output"))),


        hr(),
        width = 6,
        # column(12,
               # htmlOutput("N_uptake_estimated")),
        DT::dataTableOutput("N_inCrop"),
        DT::dataTableOutput("N_require"),

        # column(12,
        #        htmlOutput("period")),
        br(),

        DT::dataTableOutput("N.calculated"),
        hr(),

    fixedRow(
      column(width = 8,
             plotOutput('P_N.uptake')
      ),
      column(width = 8,
             plotOutput('distPlot2')
             # DT::dataTableOutput("N_graphing")
      )
    ))
    )




  )
  )
  )
  )

  # column(7,
  #          p(strong(h4("Soil Nitrogen"))),
  #          hr(),
  #          p(em("Nitrate Quick Test")),
  #
