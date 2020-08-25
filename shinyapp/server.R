#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Note:
# 1. use reactive to make inputs avaiable to the whole app
# 2. reactive objects have to be sourced with empty brackets behind
# 3. be careful with `observer` sessions since some of them can cancel each other
#    and leave no options in the UI
# 4. `validate` with `need` call provide the neat way to pass feedback message to the UI
# 5. `trycatch` only report messages in the log not in the front end


# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
# Crop tab ----

  paddock.status <- reactive({
    input$FallowOrCropping
  })

  ## crop type filter from the display name
  input_crop <- reactive({
    ### only need the vector value
      subset(crop.para, Crop_name_display == input$input_crop)$Crop
  })
  # output$cropname <- reactive({
  #   input_crop()
  #   })

  # Show the marketable yield
  output$vegeOrNot <- reactive({
    as.integer(input_crop() %in% marketable.yield.crops)})
  # Use output need to turn off the suspendWhenHidden
  outputOptions(output, "vegeOrNot", suspendWhenHidden = FALSE)

  ## crop selction and the yield options
  crop_filtered <- reactive({
    ### filter by the crop type
    df <- crop.yield %>%
      filter(Crop == input_crop())
    df
  })

  output$crop_filtered <- renderTable(crop_filtered())

  ## crop harvest componet unit and name filtered
  crop.para_filtered <- reactive({
    ### filter by the crop type
    df <- crop.para %>%
      filter(Crop == input_crop())
    df
  })

  ## crop parameters: N.uptake, B M C A
  crop_filtered_1row <- reactive({
    ### filter by the crop type and the user seleted marketable yield
    df <- crop.yield %>%
      filter(Crop == input_crop(), Yield.value == input$input_targetYield)
    df
    validate(
      need(nrow(df) != 0 , "Please choose a target yield.\r\n")
    )
    df
  })

  ## paddock information for multi tests
  paddock <- reactive({
    validate(
      need(!is.null(input$input_paddock.id), "")
    )
    as.character(input$input_paddock.id)
  })

  samplingDate <- reactive({
    samplingDate <- input$Sampling.Date
    validate(
      need(!is.na(samplingDate) | !is.na(samplingDate), "Sampling Date must not be empty.\r\n")
    )
    samplingDate
    })

  nextSamplingDate <- reactive({
    samplingDate <- input$input_nextsamplingDate
    validate(
      need(!is.na(samplingDate) | !is.na(samplingDate), "Next sampling Date must not be empty.\r\n")
    )
    samplingDate
  })

  plantingDate <- reactive({
    samplingDate <- input$input_PlantingDate
    validate(
      need(!is.na(samplingDate) | !is.na(samplingDate), "Planting Date must not be empty.\r\n")
    )
    samplingDate
  })

  ## make a data frame to feed into the downloadable report
  crop_info_reactive <- reactive({
    tab <- tibble(" " = c("Crop Selected",
                          "Farm System",
                          "Target Total Yield",
                          "Target Marketable Yield",
                          "Planting Date",
                          "Sampling Date",
                          "Next Sampling/Side dressing Date"),
                  "  " = c(input$input_crop,
                           input$input_system,
                           input$input_targetYield,
                           input$input_componentYield,
                           format(as.Date(plantingDate(), format = "%F"), "%d %B %Y"),
                           format(as.Date(samplingDate(), format = "%F"), "%d %B %Y"),
                           format(as.Date(nextSamplingDate(), format = "%F"), "%d %B %Y")))
  })

  ## the crop life time - it is calcuated from the consumption of estimated whole season N uptake
  ## the number of growing days depends on the crop
  crop_period <- reactive({
    if(paddock.status() == "Cropping"){
      nrow(subset(Crop_N_graphing(), Remaining.N.Requirement != 0))
    } else {
      # if the paddock is fallow, use the days differences between two sampling dates
      DAP_nextSD()
    }

  })

  # report back to the Estimated seasonal N uptake (kg/ha)
  Seasonal.N.uptake <- reactive({crop_filtered_1row()$Seasonal.N.uptake})

  # days after planting information ----
  DAP_SD <- reactive({
    if(paddock.status() == "Cropping"){
      DAP <- as.Date(samplingDate()) - as.Date(plantingDate())
      DAP <- ifelse(DAP < 0, 0, DAP)
    } else{
      # fallow situation will be no planting date
      # but how can be so sure that the organic N pool will have that much N?
      DAP <- as.integer(0)
    }

  })
  DAP_nextSD <- reactive({
    if(paddock.status() == "Cropping"){
      DAP <- as.Date(nextSamplingDate()) - as.Date(plantingDate())
      DAP <- ifelse(DAP < 0, 0, DAP)
    } else{
      DAP <- as.Date(input$input_nextsamplingDate_fallow) - as.Date(input$Sampling.Date_fallow)
      DAP <- ifelse(DAP < 0, 0, DAP)
      }
    })



# Soil tab ----
  # process sampling depth ----
  #take the radiobutton inputs as the key
  top_layer <- reactive({
    # only need to end of sampling for each layer
    evaluate_inputDepth(input$samplingDepth1.2)
    })
  middle_layer <- reactive({
    evaluate_inputDepth(input$samplingDepth2.2)
    })

  deep_layer <- reactive({
    evaluate_inputDepth(input$samplingDepth3.2)
    })

  # process the first sample depth to have the soil sample length
  sample.length1 <- reactive({
    if(!is.na(top_layer())){
      validate(
        need(is.numeric(top_layer()), "Sampling depth must be a number.")
      )
      as.integer(top_layer())}
  })

  sample.length2 <-  reactive({
    if(!is.na(middle_layer())& middle_layer () > top_layer()){
      validate(
        need(is.numeric(middle_layer()), "Sampling depth must be a number."),
        need(middle_layer() > top_layer(), "The second sampling depth must be greater than the first one.")
      )
      as.integer(middle_layer() - top_layer())}
  })

  sample.length3 <- reactive({

    if(!is.na(deep_layer()) & deep_layer() > middle_layer()){
      validate(
        need(is.numeric(deep_layer()), "Sampling depth must be a number."),
        need(deep_layer() > top_layer(), "The third sampling depth must be greater than the first one."),
        need(deep_layer() > middle_layer(), "The third sampling depth must be greater than the first one.")
        )
      as.integer(deep_layer() - middle_layer())
      }
    })

  # quick test results ----

  Qtest1 <- reactive({
    val_layer1 <- as.integer(input$Qtest1)
    val_layer1 <- ifelse(is.na(val_layer1) | is.null(val_layer1), 0, val_layer1)
    validate(
      need(val_layer1 >= 0, warning_soil.tab)
    )
    val_layer1
  })

  Qtest2 <- reactive({
    val_layer2 <- as.integer(input$Qtest2)
    val_layer2 <- ifelse(is.na(val_layer2) | is.null(val_layer2), 0, val_layer2)
    validate(
      need(val_layer2 >= 0, warning_soil.tab)
    )
    val_layer2
  })

  Qtest3 <- reactive({
    val_layer3 <- as.integer(input$Qtest3)
    val_layer3 <- ifelse(is.na(val_layer3) | is.null(val_layer3), 0, val_layer3)
    validate(
      need(val_layer3 >= 0, warning_soil.tab)
    )
    val_layer3
  })

  # quick test nitrate-N (mg/kg DM) calculation----
  ## Quick test nitrate-N (mg/kg DM) = Quick test nitrate (mg/L) (user input)/CF
  # Mineral N supply (kg/ha) = Quick test nitrate (mg/L) * CF2/ Ammonium_N_factor(0.95)
  # CF , CF = filter(Texture, Moisture, depth1)
  # BD same rules as for CF, filtering by textture and mositure and depth
  # CF2  = 1/(CF/(BD * (depth/10)))
  soil_filter <- reactive({
    validate(
      need(!is.null(paddock.status()), warning_paddockstatus)
    )

    Texture1 = input$Texture.1
    Texture2 = input$Texture.2
    Texture3 = input$Texture.3

    Moisture1 = input$Moisture.1
    Moisture2 = input$Moisture.2
    Moisture3 = input$Moisture.3


    if(top_layer() != 0 & middle_layer() != 0 & deep_layer() != 0){
      obj <- c(Texture1, Texture2, Texture3, Moisture1, Moisture2, Moisture3)
      validate(
        need(sum(sapply(obj, is.null)) == 0, warning_soil.reminder ),
        need(middle_layer() > top_layer() & deep_layer() > middle_layer(), warning_wronginput)
      )
      df.1 <- soil.para.filters(soil = soil, start = "0", end = top_layer(),
                                inputTexture = Texture1,
                                inputMoisture = Moisture1,
                                inputQtest = Qtest1(),
                                sampleLength = sample.length1())
      df.2 <- soil.para.filters(soil = soil, start = top_layer(), end = middle_layer(),
                                inputTexture = Texture2,
                                inputMoisture = Moisture2,
                                inputQtest = Qtest2(),
                                sampleLength = sample.length2())
      df.3 <- soil.para.filters(soil = soil, start = middle_layer(), end = deep_layer(),
                                inputTexture = Texture3,
                                inputMoisture = Moisture3,
                                inputQtest = Qtest3(),
                                sampleLength = sample.length3())
      df <- bind_rows(df.1, df.2, df.3) %>%
        mutate(qTestN.mg.kg = round(qtest_user.input/CF, digits = 0),
               CF2 = round(1/(CF/(Bulk.density*(Sample.length/10))), digits = 2),
               MineralN = round(qtest_user.input*CF2/0.95, digits = 0))
    } else if (top_layer() != 0 & middle_layer() != 0){
      obj <- c(Texture1, Texture2, Moisture1, Moisture2)
      validate(
        need(sum(sapply(obj, is.null)) == 0, warning_soil.reminder),
        need(middle_layer() > top_layer(), warning_wronginput)
        # need(sum(sapply(obj, is.null)) == 4, warning_soil.reminder)
      )
      df.1 <- soil.para.filters(soil = soil, start = "0", end = top_layer(),
                                inputTexture = Texture1,
                                inputMoisture = Moisture1,
                                inputQtest = Qtest1(),
                                sampleLength = sample.length1())
      df.2 <- soil.para.filters(soil = soil, start = top_layer(), end = middle_layer(),
                                inputTexture = Texture2,
                                inputMoisture = Moisture2,
                                inputQtest = Qtest2(),
                                sampleLength = sample.length2())
      df <- bind_rows(df.1, df.2) %>%
        mutate(qTestN.mg.kg = round(qtest_user.input/CF, digits = 0),
               CF2 = round(1/(CF/(Bulk.density*(Sample.length/10))), digits = 2),
               MineralN = round(qtest_user.input*CF2/0.95, digits = 0))

    } else if (top_layer() != 0){
      obj <- c(Texture1,  Moisture1)
      validate(
        need(sum(sapply(obj, is.null)) == 0, "Please check if all soil information has been filled.")
      )
      df <- soil.para.filters(soil = soil, start = "0", end = top_layer(),
                              inputTexture = Texture1,
                              inputMoisture = Moisture1,
                              inputQtest = Qtest1(),
                              sampleLength = sample.length1()) %>%
      mutate(qTestN.mg.kg = round(qtest_user.input/CF, digits = 0),
               CF2 = round(1/(CF/(Bulk.density*(Sample.length/10))), digits = 2),
               MineralN = round(qtest_user.input*CF2/0.95, digits = 0))
    } else {
      validate(
        need(!is.null(Texture1), warning_report.tab)
      )
      # df <-  tibble(Texture = " ",
      #               Mositure = " ",
      #               Sampling.Depth = " ",
      #               MineralN = "Please check the soil tab",
      #               Description = "Please check the soil tab.")
    }

    df
  })


  # AMN N supply calculation -----
  # if(!is.na(test result))
  # Remaining ON supply (kg/ha) = (crop period - DAP_SD)*data supply rate
  # if(is.na(test result))
  # Remaining ON supply (kg/ha) = (crop period - DAP_SD )* Default Supply Rate (kg N/day)
  # Data supply rate (kg N/day) = test result * converison coefficient/crop period



  AMN.supply.rate <- reactive({
    # the raw data is from the excel file. two small to keep in a tab in the sqlite file
    # crop system default AMN reserves
    validate(
      need(crop_period() > 0, "AMN release curve requires that the next sampling date is greater than the sampling date.")
    )

    AMN.result <- as.integer(input$AMN1.1)
    AMN.result <- ifelse(is.na(AMN.result) | is.null(AMN.result), 0, AMN.result)
    ture_period <- crop_period()
    converisonF <- ifelse(crop_period() >= 100, 0.9,
                          ifelse(crop_period() < 40, 0.3, 0.5))

    if(AMN.result > 0){
      DataSupplyRate = AMN.result * converisonF / crop_period()
      return(DataSupplyRate)
    } else {
      AMN_default <- switch(input$input_system,
                            "Mixed cropping/arable" = as.integer(90),
                            "Intensive vegetable production" = as.integer(50),
                            "Pasture conversion" = as.integer(180)
      )

      DefaultSupplyRate = AMN_default * converisonF / crop_period()
      return(DefaultSupplyRate)
    }
  })

  AMN_supply <- reactive({
    # the raw data is from the excel file. two small to keep in a tab in the sqlite file
    # crop system default AMN reserves
    validate(
      need(crop_period() > 0, "AMN release curve requires that the next sampling date is greater than the sampling date.")
      )
     AMN_remaining = round(crop_period() * AMN.supply.rate() - DAP_SD() * AMN.supply.rate() , digits = 0)

  })

  # # debugging AMN supply -----
  # output$df_AMN <-  renderText({AMN_supply()})
  # output$df_days <-  renderText({DAP_SD()})
  # output$crop_period <- renderText({crop_period()})
  # output$AMN.supply.rate <- renderText({AMN.supply.rate()})
  # # debugging the issue 14----
  # output$df_graph <- DT::renderDataTable({Crop_N_graphing()})
  # output$df_graph2 <- DT::renderDataTable({crop_filtered_1row()})

  # total N supply from soil - minN + AMN
  Soil_N_supply <- reactive({
    validate(
      need(is.data.frame(soil_filter()), warning_report.tab)
    )
    sn <- sum(soil_filter()$MineralN, na.rm = TRUE) + AMN_supply() # AMN_supply has 3 element.
    sn
  })

# updating user selection -----
  observe({
    # Marketable Yield dropdown list - key component
    # updating by crop name
    updateSelectInput(session,
                      "input_targetYield",
                      label = crop.para_filtered()$Yield.parameter,
                      choices = crop_filtered()$Yield.value)
  })
  observe({
    updateSelectInput(session, "input_componentYield",
                      label = crop.para_filtered()$Harvested.parameter,
                      choices = crop_filtered_1row()$Harvested.value)
  })
  observeEvent(input$samplingDepth1.2, {
    updateNumericInput(session, inputId = "samplingDepth2.1", value = input$samplingDepth1.2)
  })
  observeEvent(input$samplingDepth2.2, {
    updateNumericInput(session, inputId = "samplingDepth3.1", value = input$samplingDepth2.2)
  })

  # back and forward for main tabs: crop, soil and report -----
  observeEvent(
    input$JumpToSoil_fallow, {
      updateTabsetPanel(session, inputId = "app.tabs",
                        selected = "soil.info")
    })
  observeEvent(
    input$JumpToSoil, {
      updateTabsetPanel(session, inputId = "app.tabs",
                        selected = "soil.info")
  })
  observeEvent(
    input$JumpToCrop, {
      updateTabsetPanel(session, inputId = "app.tabs",
                        selected = "crop.info")
    })
  observeEvent(
    input$JumpToReport, {
      updateTabsetPanel(session, inputId = "app.tabs",
                        selected = "report.tab")
    })
  observeEvent(
    input$JumpToSoil2, {
      updateTabsetPanel(session, inputId = "app.tabs",
                        selected = "soil.info")
    })
  # radiobuttom 0-30 cm tab back and forward switches ----
  observeEvent(
    input$nextLayer.1, {
      updateTabsetPanel(session, "soil.tabset.layer.1",
                        selected = "Panel.2.1")
    })

  observeEvent(
    input$nextLayer.2, {
      updateTabsetPanel(session, inputId = "soil.tabset.layer.1",
                        selected = "Panel.1")
    })

  # radiobuttom 0-15 cm tab back and forward switches ----
  observeEvent(
    input$nextLayer.1.1, {
      updateTabsetPanel(session, inputId = "soil.tabset.layer.1.1",
                        selected = "Panel.1.2")
    })
  observeEvent(
    input$nextLayer.1.2, {
      updateTabsetPanel(session, inputId = "soil.tabset.layer.1.1",
                        selected = "Panel.1.1")
    })
  observeEvent(
    input$nextLayer.1.3, {
      updateTabsetPanel(session, inputId = "soil.tabset.layer.1.1",
                        selected = "Panel.2")
    })
  observeEvent(
    input$nextLayer.1.4, {
      updateTabsetPanel(session, inputId = "soil.tabset.layer.1.1",
                        selected = "Panel.1.2")
    })
  # refresh button in crop and soil tabs----

  observeEvent(input$refresh, {
    shinyjs::js$refresh()
  })
  observeEvent(input$refresh.soil, {
    shinyjs::js$refresh()
  })


  # Crop alone remaining N required (critical calculation) ----
  remaining.crop.N.requirement <- reactive({
    # Remaining.CropN.requirement = estimated.seasonal.Nuptake - C+A/(1 + exp(-B*(DAP - M)))
    N_remain <- Crop_N_graphing() %>%
      filter(DAP_annual == DAP_SD()) %>%
      .$Remaining.N.Requirement %>%
      round(., digits = 0)
  })

  # seasonal N net
  net.whole.season <- reactive({
    net = Soil_N_supply() - crop_filtered_1row()$Seasonal.N.uptake
    net = ifelse(net > 0, paste0(net, "(surplus)"), paste0(net, "(deficit)"))
  })

  # crop n requirement unitl next sampling date, a value -----
  crop.N.req.until.next.SD <- reactive({
    if(samplingDate() > nextSamplingDate()){
      val <- "NA. Sampling date must be smaller than the next sampling date."
    } else {
      na.omit(Crop_N_graphing()$N_nextSD) - na.omit(Crop_N_graphing()$N_SD)
    }
  })

  net.to.next.SD <- reactive({
    validate(
      need(!is.na(Soil_N_supply()), message = FALSE),
      need(!is.na(crop.N.req.until.next.SD()), message = FALSE)
    )
      net <- Soil_N_supply() - crop.N.req.until.next.SD()
      net <- round(net, digits = 0)
      net <- ifelse(net > 0, "No extra N needed until the next sampling/side dressing date", as.numeric(-net))

  })

  # Crop N requirement table ----
  N_crop <- reactive({
    validate(
      need(nextSamplingDate() >= samplingDate(),
           "Sampling date must be smaller than the next sampling date."),
      need(!is.na(Seasonal.N.uptake()) &&!is.null(remaining.crop.N.requirement()),
           "Sampling date is out of range!")
    )

    tab <- tibble(`Seasonal N Balance`= c("Estimated Total Crop N uptake",
                                          # "Estimated Plant Avilable N",
                                          "Remaining crop N requirement"
                                          # "N Required to Reach Target Yield"
                                          ),
                  `kg N/ha` = c(Seasonal.N.uptake(),
                                # Soil_N_supply(),
                                remaining.crop.N.requirement()
                                # paste0(Soil_N_supply() - Seasonal.N.uptake(), "(",x,")")
                                ))
    })
  report.tab_2 <- reactive({
    validate(
      need(nextSamplingDate() >= samplingDate(), ""),
      need(!is.null(remaining.crop.N.requirement), "Next sampling date is out of growing period.")
    )

    x <- ifelse(Soil_N_supply() > remaining.crop.N.requirement(), "Surplus", "Deficit")
    tab <- tibble(`Seasonal N Balance`= c("Estimated Plant Available N",
                                          "N Required to Reach Target Yield",
                                          paste0("N Required to Next SD (", input$input_nextsamplingDate, ")")),
                  `kg N/ha` = c(Soil_N_supply(),
                                paste0(abs(Soil_N_supply() - remaining.crop.N.requirement()), "(",x,")"),
                                net.to.next.SD()
                                ))
    })
  # Quick test result + AMN supply table
  table_soil_N <- reactive({
    validate(
      need(is.data.frame(soil_filter()), warning_report.tab),
      need(nrow(soil_filter()) > 0, "Please provide quick test information.")
    )
    no.ofRows <- nrow(soil_filter())-1
    times <- ifelse(no.ofRows < 0 ,0 , no.ofRows)
    table <- soil_filter() %>%
      select(Texture, Moisture, Sampling.Depth, QTest.Results = qtest_user.input, MineralN) %>%
      mutate(AMN = as.integer(c(AMN_supply(), rep(0, times))),
             SubTotal = AMN + MineralN,
             AMN = ifelse(AMN == 0, NA, AMN)
             )
    })
  # key intermediate data - crop growing period and N uptake to draw 1st plot ----
  Crop_N_graphing <- reactive({
    if(paddock.status() == "Cropping"){
      #construct a df for the annual crop growing period and the paras from the model
      df <- tibble::tibble(DAP_annual = seq(0, 365, by = 1),  list(crop_filtered_1row()))

      #expand the model paras and calculate the curve
      df <- unnest(df,cols = c(`list(crop_filtered_1row())`)) %>%
        # calculate the curve
        mutate(Predicted.N.Uptake = A+C/(1+exp(-B*(DAP_annual - M))),
               Predicted.N.Uptake = ifelse(Predicted.N.Uptake < 0, 0, Predicted.N.Uptake),
               Remaining.N.Requirement = crop_filtered_1row()$Seasonal.N.uptake - Predicted.N.Uptake,
               Predicted.N.Uptake = round(Predicted.N.Uptake, digits = 0),
               Remaining.N.Requirement = round(Remaining.N.Requirement, digits = 0)) %>%
        # add the sampling dates
        mutate(Remaining.N.Requirement = ifelse(Remaining.N.Requirement > 0 , Remaining.N.Requirement, 0),
               N_SD = ifelse(DAP_annual == DAP_SD(), Predicted.N.Uptake, NA),
               N_nextSD = ifelse(DAP_annual == DAP_nextSD(), Predicted.N.Uptake, NA))
    }
    })

  # 1st graph, line plot for N estimation ----
  N_uptake_reactive <- reactive({
    #plotting
    ## graphy customisation
    size = 5

    # filter the x axis to bring the resolution up
    ## The
    rle <- rle(Crop_N_graphing()$Remaining.N.Requirement)

    nrows.non0 <- nrow(filter(Crop_N_graphing(), Remaining.N.Requirement != min(rle$values)))

    # slice xx days more to see the palateu
    df <- Crop_N_graphing() %>%
      slice(1:(nrows.non0 + 30))

    # Add validation to make sure planting date is earlier than sampling dates
    validate(
      need(plantingDate() <= samplingDate(),
           "The sampling date needs to be greater than the planting date.
Please check the dates in crop info tab.
Please use the fallow option if you only want to know the nitrogen status in the soil.")
    )

    maxN <- max(df$Remaining.N.Requirement)
    ylim <- maxN + 20

    P <-  df %>%
      ggplot(aes(x = DAP_annual)) +
      # geom_point(aes(y = Predicted.N.Uptake)) +
      geom_line(aes(y = Predicted.N.Uptake, color = "Predicted.N.Uptake"))+
      geom_point(aes(y = N_SD, shape = "Sampling date"), size = size, na.rm=TRUE)+
      geom_point(aes(y = N_nextSD, shape = "Next sampling date"),size = size, fill = "#FF0000",na.rm=TRUE)+
      scale_shape_manual(name = "",values =  c(`Sampling date` = 16, `Next sampling date` = 24))+
      scale_color_manual(name = "", values = "#FF0000") +
      labs(title = "Estimated whole crop N uptake",
           x = "Days after planting",
           y  = "Whole crop N uptake (kg/ha)",
           caption = "More accurate results could be obtained from Lab tests or a more sophisticated biophysical model (e.g.APSIM).")+
      scale_x_continuous(breaks = seq(0, max(df$DAP_annual), by = 30)) +
      # scale_y_continuous(expand = c(0,0.5), limits = c(0, ylim), breaks = seq(0, ylim, by = 40)) +
      theme_qtmb() +
      annotate("text",
               x = median(df$DAP_annual),
               y = max(df$Predicted.N.Uptake)/2 - 3,
               # vjust = 0.3,
               alpha = 0.5, size = 5.5,
               label = "bold(\"This graph is only an indicator of crop N uptake.\")",
               parse = TRUE)

    noLinesCrop <- c("Kale", "Fodder_Beet")
    if(!input_crop() %in% noLinesCrop){
      P +
        geom_hline(yintercept = maxN, colour = "#000000",size = 1) +
        annotate("text",
                 x = 0,
                 y = maxN,
                 size = 5.5,
                 label = paste("N required to reach target yield:", round(maxN, digits = 0)),
                 vjust = 1,
                 hjust = "inward")
      } else {
        P
        }
  })
  # 2nd graph, bar plot for different depths ----
  N_supply_depth <- reactive({
    # maybe we can cut this plot down to make the app mobile friendly !!!
    validate(
      need(is.data.frame(soil_filter()), warning_report.tab),
      need(!is.null(top_layer()), warning_report.tab)
    )
    validate(
      need(nextSamplingDate() >= samplingDate() | input$input_nextsamplingDate_fallow >= input$Sampling.Date_fallow,
           "Sampling date must be smaller than the next sampling date.")
    )
    depths <- soil_filter() %>%
      select(MineralN, Sampling.Depth) %>%
      mutate(Depth = paste(Sampling.Depth, "cm"))

    # profile sampling depth
    profile.depth <-  gsub("-.+-", "-", x = paste(depths$Depth, collapse = "-"))

    # summarised the numbers
    total <- soil_filter() %>%
      select(MineralN) %>%
      dplyr::summarise(MineralN = sum(MineralN, na.rm = TRUE)) %>%
      mutate(Depth = paste("Total", profile.depth))


    # draw a bar graph
    p <- bind_rows(depths, total) %>%
      # mutate(Depth = as.factor(Depth),
      #        Depth = factor(Depth, levels = levels(Depth)[c("")])) # attempt to fix the order of layers
      ggplot(aes(Depth, MineralN, fill = Depth)) +
      geom_col(width = 0.5) +
      labs(title = "Estimated  soil mineral N supply (from nitrate QT)",
           x = "",
           y  = "Soil mineral N supply (kg/ha)")+
      theme_qtmb() +
      scale_y_continuous(expand = c(0,0), limits = c(0, max(total$MineralN) + 5 ))

    p
    })
# Report tab ----
  output$soil_filtered <- DT::renderDataTable({
    # if the user select nothing in the soil tab, show nothing to the user.
    validate(
      need(!is.null(top_layer()), warning_report.tab)
    )
    tab <- table_soil_N()%>%
      mutate(SubTotal = as.character(SubTotal)) %>%
      add_row(.,
              Texture = "",
              Moisture = "",
              Sampling.Depth = "",
              QTest.Results = as.integer(""),
              MineralN = as.integer(""),
              AMN = as.integer(""),
              SubTotal = paste0("Total plant available N: ",
                                sum(as.numeric(table_soil_N()$SubTotal),
                                    na.rm = TRUE), "kg/ha"))%>%
      rename('Sampling.Depth (cm)' = Sampling.Depth,
             'QTest.Results (mg/L)' = QTest.Results,
             'Mineral N (kg/ha)' = MineralN,
             'AMN (kg/ha)' = AMN,
             'SubTotal (kg/ha)' = SubTotal)



    colnames(tab)[5] <- add.questionMark(colnames(tab)[5],
                                         soil.tab.tooltips[1])
    colnames(tab)[6] <-add.questionMark(colnames(tab)[6],
                                        soil.tab.tooltips[2])

    tab <- DT::datatable(tab,
                         escape = FALSE,
                         options = list(dom = 't',
                                        columnDefs = list(list(className = 'dt-left', targets = '_all'))),
                         rownames = FALSE)

  })

  output$report.table2 <- DT::renderDataTable({

    tab2 <- report.tab_2()

    for(i in seq_len(length(report.tab2.tooltips))){

      tab2[i, 1] <- paste0(tab2[i, 1],
                           "<a title='",
                           report.tab2.tooltips[i],
                           "'><img src='blue_question_mark.png' height='13px'/>
                           </a>")
    }

    DT::datatable(tab2,
                  options = list(dom = 't',
                                 columnDefs = list(list(className = 'dt-left', targets = '_all'))),
                  rownames = FALSE,escape = FALSE,
                  colnames = c("", colnames(N_crop())[ncol(N_crop())])) %>%
      DT::formatStyle(#https://rstudio.github.io/DT/010-style.html
        'Seasonal N Balance',
        target = 'row',
        backgroundColor = DT::styleEqual("N Required to Reach Target Yield", "yellow")
      )

  })

  output$distPlot2 <- renderPlot({
    N_supply_depth()
  })

  output$N_inCrop <- DT::renderDataTable({
    validate(
      need(!is.null(top_layer()), warning_report.tab)
    )
    tab <- N_crop()
    tab[2,1] <- add.questionMark(tab[2,1], report.tab1.tooltips)

    tab <- DT::datatable(tab,
                         escape = FALSE,
                         rownames = FALSE,
                         options = list(dom = 't', #https://datatables.net/reference/option/dom
                                        columnDefs = list(list(className = 'dt-left', targets = '_all'))),
                         colnames = c("", colnames(N_crop())[ncol(N_crop())]))

  })

  output$P_N.uptake <- renderPlot({
    validate(
      need(!is.null(top_layer()), warning_report.tab),
      need(crop_period() > DAP_nextSD() & crop_period() > DAP_SD(),
           paste0("Your next sampling date must be within the crop growing period (within ", crop_period() ," days)."))
    )
        N_uptake_reactive()

  })

  output$report <- downloadHandler(

    # For PDF output, change this to "report.pdf"
    filename = function() { # https://shiny.rstudio.com/gallery/download-knitr-reports.html
      paste('my-report',  "pdf",sep = '.')
      #       switch(
      #   input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      # ))
    },

    content = function(file) {

      if(paddock.status() == "Cropping"){
        params <- list(crop_info = crop_info_reactive(),
                       Soil_N_supply = table_soil_N(),
                       p_N_uptake = N_uptake_reactive(),
                       p_N_supply = N_supply_depth(),
                       tab_NCrop = N_crop(),
                       tab_N_requirements = report.tab_2())

        src <- normalizePath('report.Rmd')
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd', overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        # params <- list(n = input$slider)

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        out <- rmarkdown::render('report.Rmd',output_format = pdf_document(),
                                 # switch(
                                 #   input$format,
                                 #   PDF = pdf_document(), HTML = html_document(), Word = word_document()
                                 # ),
                                 params = params,
                                 envir = new.env(parent = globalenv())
        )
        file.rename(out, file)
      } else{
        params <- list(Soil_N_supply = table_soil_N(),
                       p_N_supply = N_supply_depth())

        src <- normalizePath('report_fallow.Rmd')
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report_fallow.Rmd', overwrite = TRUE)

        out <- rmarkdown::render('report_fallow.Rmd',output_format = pdf_document(),
                                 # switch(
                                 #   input$format,
                                 #   PDF = pdf_document(), HTML = html_document(), Word = word_document()
                                 # ),
                                 params = params,
                                 envir = new.env(parent = globalenv())
        )
        file.rename(out, file)
        }

    }
  )

  output$qTestResults.csv <- downloadHandler(


    filename = function() {
      "my Quick Test Result.csv"
      # ,sep = '.')
      #        switch(
      #   input$format_data, csv = 'csv', Excel = "xlsx"
      # ))
    },

    content = function(file) {
      # the content

      if(paddock.status()=="Cropping"){
        df <- table_soil_N() %>%
          mutate(Paddock = paddock(),
                 SamplingDate = samplingDate(),
                 NextSamplingDate = nextSamplingDate(),
                 PlantingDate = plantingDate()) %>%
          select(PlantingDate, SamplingDate, NextSamplingDate, Sampling.Depth, everything())

        #unit

        unitLine <- ",,,(cm),(),(),(mg/L),(kg/ha),(kg/ha),(kg/ha),()"

      } else{
        df <- table_soil_N() %>%
          mutate(Paddock = paddock(),
                 SamplingDate = samplingDate(),
                 NextSamplingDate = nextSamplingDate()) %>%
          select(SamplingDate, NextSamplingDate, Sampling.Depth, everything())
        #unit

        unitLine <- ",,(cm),(),(),(mg/L),(kg/ha),(kg/ha),(kg/ha),()"

      }


      # if(input$format_data == "Excel"){
      #   openxlsx::write.xlsx(x = df, file = file)
      # } else{
        f <- file(file, "w")
        cat("Texture,More detials refers to https://soils.landcareresearch.co.nz/understanding-soils/get-dirty/", "\r",file = f)
        cat("Moisture,", "\r", file = f)
        cat("Sampling.Depth,Layers in cm", "\r", file = f)
        cat("QTest.Results,quick test result", "\r", file = f)
        cat("MineralN,Mineral Nitrogen Test", "\r", file = f)
        cat("AMN,Anaerobic Mineralisable N Test", "\r", file = f)
        cat("SubTotal,Total N (MineralN + AMN) in each sampled layer.", "\r", file = f)
        cat("Paddock,Paddock name if have multiple tests.", "\r\r\n", file = f)

        cat(format(unitLine), "\r",file = f)
        write.table(df, file = f, append = TRUE, quote = FALSE, sep = ",", eol = "\r", row.names=F)
        close(f)

        # }
    }


  )
# the end of the server----

})
