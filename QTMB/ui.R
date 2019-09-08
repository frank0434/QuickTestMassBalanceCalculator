#
# This is the user-interface definition of a Shiny web application. You can



# Define UI for application that draws a histogram

shinyUI(fluidPage(
  theme = "style.css",
  headerPanel('Quick Test Mass Calculator'),
  hr(),

  fluidRow(
    column(5,
           fixedRow(
             p(strong(h4("Crop"))),
             hr(),
             column(6,
                    selectInput(inputId = "1",
                                label = "System",
                                choices = c("Intensive vegetable production","2","3"))),
             column(4,
                    selectInput(inputId = "2", label = "Crop", choices = c("Wheat_Autumn","2","3"))),
             column(6,
                    dateInput("date1", label = "Planting Date")),
             column(4,
                    selectInput("date1", label = "Target yield (t FW/ha)", choices = c(1:100))),
             column(12,
                    numericInput("date1", label = "Estimated seasonal N uptake (kg/ha)", value = 0))
           ),

           hr(),
           fixedRow(
             p(strong(h4("Seasonal N Balance"))),
             hr(),
             column(6,
                    numericInput(inputId = "1", label = "Soil N supply",value = 0)),
             column(4,
                    numericInput(inputId = "2", label = "Remaining Crop N requirement",value = 0)),
             column(6,
                    textInput("3", label = "Net", value = paste0(000,"surplus") ))
           ),
           hr(),
           fixedRow(
             p(strong(h4("Next sampling date (SD)"))),
             hr(),
             column(12,
                    dateInput(inputId = "1",
                              label = "Next SD")),
             column(6,
                    numericInput(inputId = "2",
                                 label = "Crop N Requirement Until next SD (kg/ha)", value = 0)),
             column(6,
                    numericInput("date1", label = "Net (kg/ha)",value = 0 ))


           )

    ),
    column(7,
           p(strong(h4("Soil Nitrogen"))),
           hr(),
           p(em("Nitrate Quick Test")),

           dateInput("date1", label = "Sampling Date"),
           br(),
           fixedRow(
             column(4,
                    p(strong(h5("Sampling Depth"))),
                    verticalLayout(
                      sliderInput(inputId = "Sampling depth 1", label = "depth1", min = 0, max = 30, value = 15,step = 5),
                      sliderInput(inputId = "Sampling depth 2", label = "depth2", min = 30, max = 60, value = 45,step = 5),
                      sliderInput(inputId = "Sampling depth 2", label = "depth3", min = 60, max = 90, value = 75,step = 5)
                    )
             ),
             column(2,
                    p(strong(h5("Soil Texture"))),
                    verticalLayout(
                      selectInput(inputId = "1", label = "depth1", choices = c("1","2","3")),
                      selectInput(inputId = "2", label = "depth2", choices = c("1","2","3")),
                      selectInput(inputId = "3", label = "depth3", choices = c("1","2","3"))
                    )
             ),
             column(2,
                    p(strong(h5("Soil Moisture"))),
                    verticalLayout(
                      selectInput(inputId = "1", label = "depth1", choices = c("1","2","3")),
                      selectInput(inputId = "2", label = "depth2", choices = c("1","2","3")),
                      selectInput(inputId = "3", label = "depth3", choices = c("1","2","3"))
                    )
             ),
             column(2,
                    p(strong(h5("QtestNitrate"))),
                    verticalLayout(
                      numericInput(inputId = "1", label = "depth1",value = 0),
                      numericInput(inputId = "2", label = "depth2", value = 0),
                      numericInput(inputId = "3", label = "depth3",value = 0)
                    )
             ),
             column(2,
                    p(strong(h5("AMN"))),
                    verticalLayout(
                      numericInput(inputId = "1", label = "depth1",value = 0),
                      numericInput(inputId = "2", label = "depth2", value = 0),
                      numericInput(inputId = "3", label = "depth3",value = 0)
                    )
             ),
             column(10,

                    splitLayout(
                      numericInput(inputId = "1", label = "depth1",value = 0),
                      numericInput(inputId = "2", label = "depth2", value = 0),
                      numericInput(inputId = "3", label = "depth3",value = 0)
                    )
             ),
             column(2,

                    splitLayout(

                      numericInput(inputId = "3", label = "Total",value = 0)
                    )
             ),
             column(10,

                    splitLayout(
                      numericInput(inputId = "1", label = NULL,value = 0),
                      numericInput(inputId = "2", label = NULL, value = 0),
                      numericInput(inputId = "3", label = NULL,value = 0)
                    )
             ),
             column(10,

                    splitLayout(
                      numericInput(inputId = "1", label = NULL,value = 0),
                      numericInput(inputId = "2", label = NULL, value = 0),
                      numericInput(inputId = "3", label = NULL,value = 0)
                    )
             ),

             column(2,

                    splitLayout(

                      numericInput(inputId = "3", label = NULL,value = 0)
                    )
             )

           )
    )),
  hr(),
  fixedRow(
    column(width = 5,
           plotOutput('plot')
    ),
    column(width = 5, offset = 1,
           plotOutput('plot2')
    )
  )

))
