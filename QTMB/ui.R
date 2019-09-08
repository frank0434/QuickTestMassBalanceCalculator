#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
dataset <- diamonds

yi <- fluidPage(theme = "bootstrap.min.css",

                  titlePanel("QTMB"),
                  splitLayout(
                    verticalLayout(
                      wellPanel(h4("Diamonds Explorer"),

                                br(),
                                checkboxInput('jitter', 'Jitter'),
                                checkboxInput('smooth', 'Smooth')
                                )
                      ),
                    verticalLayout(
                      wellPanel(h4("Soil Nitrogen"),
                                p(em("Nitrate Quick Test")),

                                verticalLayout(
                                  column(
                                    width = 12,
                                    tags$form(
                                      class="form-horizontal",
                                      tags$div(
                                        class="form-group",
                                        tags$label(class = "col-sm-2 control-label", "Sampleing Date:"),
                                        column(width = 6, dateInput("date1", label = NULL))
                                      ))),
                                  br(),
                                  column(
                                    width = 12,
                                    tags$form(
                                      class="form-horizontal",
                                      tags$div(
                                        class="form-group",
                                        tags$label(class = "col-sm-3 control-label", br(), "Sampling depth start (cm)"),
                                        column(width = 2, textInput(inputId = "Sampling depth start (cm)1", label = "0-15cm", value = "0")),
                                        column(width = 2, textInput(inputId = "Sampling depth start (cm)2", label = "15-30cm", value = "15")),
                                        column(width = 2, textInput(inputId = "Sampling depth start (cm)3", label = "30-90cm", value = "30"))
                                      ),

                                      tags$div(
                                        class="form-group",
                                        tags$label(class = "col-sm-3 control-label",  "Sampling depth end (cm)"),
                                        column(width = 2, textInput(inputId = "Sampling depth end (cm)1", label = NULL, value = "15")),
                                        column(width = 2, textInput(inputId = "Sampling depth end (cm)2", label = NULL, value = "30")),
                                        column(width = 2, textInput(inputId = "Sampling depth end (cm)3", label = NULL, value = "90"))
                                      ),
                                      br(),
                                    column(
                                      width = 12,
                                      tags$form(
                                        class="form-horizontal",
                                        tags$div(
                                          class="form-group",
                                          tags$label(class = "col-sm-2 control-label", "Soil texture"),
                                          column(width = 2, textInput(inputId = "Sampling depth start (cm)1", label = NULL, value = "0")),
                                          column(width = 2, textInput(inputId = "Sampling depth start (cm)2", label = NULL, value = "15")),
                                          column(width = 2, textInput(inputId = "Sampling depth start (cm)3", label = NULL, value = "30"))
                                          ),
                                        tags$div(
                                          class="form-group",
                                          tags$label(class = "col-sm-2 control-label",  "Soil moisture"),
                                          column(width = 2, textInput(inputId = "Sampling depth end (cm)1", label = NULL, value = "15")),
                                          column(width = 2, textInput(inputId = "Sampling depth end (cm)2", label = NULL, value = "30")),
                                          column(width = 2, textInput(inputId = "Sampling depth end (cm)3", label = NULL, value = "90"))
                                          )
                                        )
                                      ),
                                    br(),
                                    column(
                                      width = 12,
                                      tags$form(
                                        class="form-horizontal",
                                        tags$div(
                                          class="form-group",
                                          tags$label(class = "col-sm-4 control-label", "Quick test nitrate (mg/L)"),
                                          column(width = 2, textInput(inputId = "Quick test nitrate (mg/L)1", label = NULL, value = "0")),
                                          column(width = 2, textInput(inputId = "Quick test nitrate (mg/L)2", label = NULL, value = "15")),
                                          column(width = 2, textInput(inputId = "Quick test nitrate (mg/L)3", label = NULL, value = "30"))
                                        ),
                                        tags$div(
                                          class="form-group",
                                          tags$label(class = "col-sm-4 control-label",  "Quick test nitrate-N (mg/kg DM)"),
                                          column(width = 2, textInput(inputId = "Quick test nitrate-N (mg/kg DM)1", label = NULL, value = "15")),
                                          column(width = 2, textInput(inputId = "Quick test nitrate-N (mg/kg DM)2", label = NULL, value = "30")),
                                          column(width = 2, textInput(inputId = "Quick test nitrate-N (mg/kg DM)3", label = NULL, value = "90"))
                                        )
                                      )
                                    )

                                    ))
                                    )),

                                br()
                      )
                      )
                    )

