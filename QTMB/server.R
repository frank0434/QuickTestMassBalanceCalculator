#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {


# CROP PANEL----

  ## crop selction and the yield options
  crop_filtered <- reactive({
    crop.yield %>%
      filter(Crop == input$input_crop)
  })


  observe({
    updateSelectInput(session, "input_targetYield", choices = crop_filtered()$Yield.value)

  })

  # report back to the Estimated seasonal N uptake (kg/ha)
  crop_filtered_1row <- reactive({
    crop.yield %>%
      filter(Crop == input$input_crop, Yield.value == input$input_targetYield)
  })

  output$N_uptake_estimated <- renderText({
    paste("<b>Estimated seasonal N uptake (kg/ha): ", crop_filtered_1row()$Seasonal.N.uptake, "</b>")
  })

  veges <- reactive({
    if(input$input_crop %in% crop.para$Crop){
      crop.yield %>%
        filter(Crop == input$input_crop, Yield.value == input$input_targetYield)
    }
    })

  output$Harvested_value <- renderText({
    paste("<b>Harvested component (t FW/ha): ", veges()$Harvested.value, "</b>")})




# Seasonal N balance PANEL ------------------------------------------------





# SOIL NITROGEN PANEL -----------------------------------------------------

  DAP <- reactive({
    DAP <- as.Date(input$Sampling.Date) - as.Date(input$input_PlantingDate)
    DAP <- ifelse(DAP < 0, 0, DAP)
  })

  output$DAP <- renderText({
    paste("<b>Days after planting (t FW/ha): ", DAP(), "</b>")
    })

  N_remain <- reactive({
    N_remain <- setDT(crop_filtered_1row())[, N_remain := round(Seasonal.N.uptake - (C + A)/(1 + exp(-B*(DAP() - M))), digits = 0)]$N_remain

  })
  output$N_inCrop <- DT::renderDataTable({
    # critical calculation ----
    # Remaining.CropN.requirement = estimated.seasonal.Nuptake - (C+A)/(1 + exp(-B*(DAP - M)))
    tab <- tibble(`Seasonal N Balance`= c("Soil N supply",
                                          "Remaining crop N requirement",
                                          "Net"),
                  `kg N/ha` = c("Place HOlder", N_remain(), "waiting"))
    tab <- DT::datatable(tab,
                         rownames = FALSE,
                         options = list(dom = 't',
                                        columnDefs = list(list(className = 'dt-right', targets = '_all'))),
                         colnames = c("", colnames(tab)[ncol(tab)]))
    })

  output$N_require <- DT::renderDataTable({
    # critical calculation ----
    # Crop.N.requirement.NextSD = Remaining.CropN.requirement - (Seasonal.N.uptake - (C + A)/(1 + exp(-B*(DAP.of.NextSD-M))))
    DAP <- as.Date(input$input_nextsamplingDate) - as.Date(input$input_PlantingDate)
    DAP <- ifelse(DAP < 0, 0, DAP)

    N_require <- N_remain() -
      (setDT(crop_filtered_1row())[, N_require :=
                                     round(Seasonal.N.uptake - (C + A)/(1 + exp(-B*(DAP - M))),
                                           digits = 0)]$N_require)
    net <- ifelse(is.na(input$input_nextsamplingDate), NA, N_remain() - N_require)
    tab <- tibble(i1 = c("Crop N Requirement until next SD", "Net"),
                  `kg N/ha` = c( N_require, net))
    tab <- DT::datatable(tab,
                         rownames = FALSE,
                         options = list(dom = 't',
                                        columnDefs = list(list(className = 'dt-right', targets = '_all'))),
                         colnames = c("", colnames(tab)[ncol(tab)]))

  })




  output$distPlot <- renderPlot({
    plot(rnorm(100))

  })
  output$distPlot2 <- renderPlot({
    plot(rnorm(100))

  })

})
