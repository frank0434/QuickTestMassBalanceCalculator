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
    df <- crop.yield %>%
      filter(Crop == input$input_crop)
    df
  })


  observe({
    updateSelectInput(session, "input_targetYield", choices = crop_filtered()$Yield.value)
    updateSelectInput(session, "Texture.1", choices = soil.Texture())
    updateSelectInput(session, "Texture.2", choices = soil.Texture())
    updateSelectInput(session, "Texture.3", choices = soil.Texture())
    updateSelectInput(session, "Moisture.1", choices = soil.Moisture())
    updateSelectInput(session, "Moisture.2", choices = soil.Moisture())
    updateSelectInput(session, "Moisture.3", choices = soil.Moisture())

  })

  # report back to the Estimated seasonal N uptake (kg/ha)
  crop_filtered_1row <- reactive({
    df <- crop.yield %>%
      filter(Crop == input$input_crop, Yield.value == input$input_targetYield)
    df
  })

  output$N_uptake_estimated <- renderText({
    paste("<b>Estimated seasonal N uptake (kg/ha): ", crop_filtered_1row()$Seasonal.N.uptake, "</b>")
  })

  veges <- reactive({
    if(input$input_crop %in% crop.para$Crop){
      df <- crop.yield %>%
        filter(Crop == input$input_crop, Yield.value == input$input_targetYield)
      df
    }
    })

  output$Harvested_value <- renderText({
    paste("<b>Harvested component (t FW/ha): ", veges()$Harvested.value, "</b>")})




# Seasonal N balance PANEL ------------------------------------------------



  DAP_SD <- reactive({
    DAP <- as.Date(input$Sampling.Date) - as.Date(input$input_PlantingDate)
    DAP <- ifelse(DAP < 0, 0, DAP)
  })
  DAP_nextSD <- reactive({
    DAP <- as.Date(input$input_nextsamplingDate) - as.Date(input$input_PlantingDate)
    DAP <- ifelse(DAP < 0, 0, DAP)
  })

  output$DAP <- renderText({
    paste("<b>Days after planting (t FW/ha): ", DAP_SD(), "</b>")
    })

  N_remain <- reactive({
    N_remain <- setDT(crop_filtered_1row())[, N_remain := round(Seasonal.N.uptake - (C + A)/(1 + exp(-B*(DAP_SD() - M))), digits = 0)]$N_remain

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

    N_require <- N_remain() -
      (setDT(crop_filtered_1row())[, N_require :=
                                     round(Seasonal.N.uptake - (C + A)/(1 + exp(-B*(DAP_nextSD() - M))),
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

  output$P_N.uptake <- renderPlot({
    # whole crop n uptake plot ----
    df <- tibble(DAP_annual = seq(0, 365, by = 1),  list(crop_filtered_1row()))

    # critical calculation for plot 1 ----
    df <- unnest(df) %>%
      mutate(Predicted.N.Uptake = (A+C)/(1+exp(-B*(DAP_annual - M))),
             Predicted.N.Uptake = ifelse(Predicted.N.Uptake <0, 0, Predicted.N.Uptake),
             Remaining.N.Requirement = N_remain() - Predicted.N.Uptake) %>%
      filter(Remaining.N.Requirement >= 0 ) %>%
      mutate(N_SD = ifelse(DAP_annual == DAP_SD(), Predicted.N.Uptake, NA),
             N_nextSD = ifelse(DAP_annual == DAP_nextSD(), Predicted.N.Uptake, NA))

    input$input_nextsamplingDate
    input$Sampling.Date
    #plotting
    size = 5
    P <- df %>%
      ggplot(aes(x = DAP_annual)) +
      # geom_point(aes(y = Predicted.N.Uptake)) +
      geom_line(aes(y = Predicted.N.Uptake, color = "Predicted.N.Uptake"))+
      geom_point(aes(y = N_SD, shape = "Sampling date"), size = size)+
      geom_point(aes(y = N_nextSD, shape = "Next sampling date"),size = size)+
      scale_shape_manual(name = "",values =  c(`Sampling date` = 16, `Next sampling date` = 6))+
      scale_color_manual(name = "", values = "red") +
      labs(title = "Estimated whole crop N uptake",
           x = "Days after planting",
           y  = "Whole crop N uptake (kg/ha)")+
      theme_classic() +
      theme(strip.background = element_blank(),
            plot.background = element_rect(inherit.blank = T),
            panel.background = element_rect(fill = "white", colour = "black"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "grey"),
            text = element_text(family = "sans", size = 16),
            plot.title = element_text(hjust = 0.5)
            )

    P





  })




  # SOIL NITROGEN PANEL -----------------------------------------------------

  soil.Texture <- reactive({
    st <- soil$Texture %>%
      unique()
  })
  soil.Moisture <- reactive({
    st <- soil$Moisture %>%
      unique()
  })

  # N test calculation----
  output$N.calculated <- DT::renderDataTable({

    # critical calculation ----
    # Quick test nitrate-N (mg/kg DM) = Quick test nitrate (mg/L)/CF
    # Mineral N supply (kg/ha) = Quick test nitrate (mg/L) * CF2/ Ammonium_N_factor(0.95)
    # CF <=30, CF = filter(Texture, Moisture, depth)
    # CF >31 & CF <=60,  CF = filter(Texture, Moisture, depth)
    # CF %in% 60-90, CF = filter(Texture, Moisture, depth)
    # BD same rules as for CF, filtering by textture and mositure and depth

    # Remaining ON supply (kg/ha) =  IF(ISNUMBER( $O$26),   ($AA$9*$AA$16)-DAYS($O$10,$K$11)*$AA$16,($AA$9*$AA$11)-(DAYS($O$10,$K$11)*$AA$11))

    # Data supply rate (kg N/day) =
    N_require <- N_remain() -
      (setDT(crop_filtered_1row())[, N_require :=
                                     round(Seasonal.N.uptake - (C + A)/(1 + exp(-B*(DAP_nextSD() - M))),
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


  output$distPlot2 <- renderPlot({
    plot(rnorm(100))

  })

})
