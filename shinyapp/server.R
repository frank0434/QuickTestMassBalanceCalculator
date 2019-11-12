#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("functions.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {


  rv <- reactiveValues(page = 1)

# Crop tab ----

  ## crop type filter from the display name
  input_crop <- reactive({
    ### only need the vector value
      subset(crop.para, Crop_name_display == input$input_crop)$Crop
  })

  ## crop selction and the yield options
  crop_filtered <- reactive({
    ### filter by the crop type
    df <- crop.yield %>%
      filter(Crop == input_crop())
    df
  })

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
      filter(Crop == input_crop(), Harvested.value == input$input_componentYield)
    df
  })

  crop_info_reactive <- reactive({
    tab <- tibble(" " = c("Crop Selected",
                          "Farm System",
                          "Target Harvested Fresh Yield",
                          "Planting Date",
                          "Sampling Date",
                          "Next Sampling/Side dressing Date"),
                  "  " = c(input$input_crop,
                           input$input_system,
                           input$input_componentYield,
                           as.character(input$input_PlantingDate),
                           as.character(input$Sampling.Date),
                           as.character(input$input_nextsamplingDate)))
  })
  crop_period <- reactive({
    max(Crop_N_graphing()$DAP_annual)
  })

  # report back to the Estimated seasonal N uptake (kg/ha)
  Seasonal.N.uptake <- reactive({crop_filtered_1row()$Seasonal.N.uptake})

  # days after planting information ----
  DAP_SD <- reactive({
    DAP <- as.Date(input$Sampling.Date) - as.Date(input$input_PlantingDate)
    DAP <- ifelse(DAP < 0, 0, DAP)
  })
  DAP_nextSD <- reactive({
    DAP <- as.Date(input$input_nextsamplingDate) - as.Date(input$input_PlantingDate)
    DAP <- ifelse(DAP < 0, 0, DAP)
  })


# Soil tab ----

  # process sampling depth ----

  top_layer <- reactive({input$samplingDepth})


  depth.15.1 <- reactive({
    if(top_layer() == layer.1.1){
          as.integer(15)
    }
  })

  depth.15.2 <- reactive({
    if(top_layer() == layer.1.1){
      as.integer(15)
    }
  })



  depth.30.2 <- reactive({
    if(top_layer() == layer.1.1){
      as.integer(30)
    } else if(top_layer == layer.1){
      as.integer(30)
    }
  })

  depth.30.1 <- reactive({
    if(top_layer() == layer.1){
      as.integer(30)
    }
  })

  # quick test results ----
  Qtest.15.1 <- reactive({
    if(top_layer() == layer.1.1){
      val0_15 <- as.integer(input$Qtest1.1)
      if(val0_15 < 0){
        # feature to be added ~ if user type in negative values shold give a warning!!!
        val0_15 = 0
        } else if (is.na(val0_15)){
          val0_15 = 0
          } else{
            val0_15
          }
      }
    })

  Qtest.15.2 <- reactive({
    if(top_layer() == layer.1.1){
        val15_30 <- as.integer(input$Qtest1.2)
        if(val15_30 < 0){
          # feature to be added ~ if user type in negative values shold give a warning!!!
          print("Quick test resutls must be 0 or positive numbers.") # the print here won't be seen by users!!!
        } else if (is.na(val15_30)){
          val15_30 = 0
        } else{
          val15_30
        }
    }
  })

  Qtest.30.2 <- reactive({
    if(top_layer() == layer.1.1){
        val30_60 <- as.integer(input$Qtest2)

        if(val30_60 < 0){
          print("Quick test resutls must be 0 or positive numbers.")
        } else if (is.na(val30_60)){
          val30_60 = 0
        } else{
          val30_60
        }
    } else if(top_layer() == layer.1){
      val30_60 <- as.integer(input$Qtest2)
      if(val30_60 < 0){
        print("Quick test resutls must be 0 or positive numbers.")
      } else if (is.na(val30_60)){
        val30_60 = 0
      } else{
        val30_60
      }
    }
  })
  Qtest.30.1 <- reactive({
    if(top_layer() == layer.1){
    val0_30 <- as.integer(input$Qtest1)
    if(val0_30 < 0){
      print("Quick test resutls must be 0 or positive numbers.")
      } else if (is.na(val0_30)){
        val0_30 = 0
        } else{
          val0_30
        }
    }
  })

  ## 60-90 is diabled
  # depth.30.3
  # Texture.3
  # Moisture.3
  # Qtest.30.3 # currently disabled


  # quick test nitrate-N (mg/kg DM) calculation----
  ## Quick test nitrate-N (mg/kg DM) = Quick test nitrate (mg/L) (user input)/CF
  # Mineral N supply (kg/ha) = Quick test nitrate (mg/L) * CF2/ Ammonium_N_factor(0.95)
  # CF , CF = filter(Texture, Moisture, depth1)

  # BD same rules as for CF, filtering by textture and mositure and depth

  # CF2  = 1/(CF/(BD * (depth/10)))
  soil_filter <- reactive({


    if(top_layer() == layer.1.1){
      df.1.1 <- soil %>%
        filter(Texture == input$Texture.1.1,
               Moisture == input$Moisture.1.1,
               Sampling.Depth == "0-30") %>%
        mutate(qtest_user.input = Qtest.15.1(),
               Sample.length = as.integer(15),
               Sampling.Depth = "0-15")
      df.1.2 <- soil %>%
        filter(Texture == input$Texture.1.2,
               Moisture == input$Moisture.1.2,
               Sampling.Depth == "0-30") %>%
        mutate(qtest_user.input = Qtest.15.2(),
               Sample.length = as.integer(15),
               Sampling.Depth = "0-15")
      # depth 0-15 & 15-30
      df.1 <- bind_rows(df.1.1, df.1.2)
    } else if (top_layer() == layer.1){
      # depth 0-30
      df.1 <- soil %>%
        filter(Texture == input$Texture.1,
               Moisture == input$Moisture.1,
               Sampling.Depth == "0-30") %>%
        mutate(qtest_user.input = Qtest.30.2(),
               Sample.length = as.integer(30))
    }


    # depth 30-60
    if(!is.null(top_layer())){
      df.2 <- soil %>%
        filter(
          Texture == input$Texture.2,
          Moisture == input$Moisture.2,
          Sampling.Depth == "30-60") %>%
        mutate(qtest_user.input = Qtest.30.2(),
               Sample.length = as.integer(30))
    }


    #depth 60-90 disabled.
    # df.3 <- cf_filter(soil, input$Texture.3, input$Moisture.3, 60, zz = input$depth.3, a = input$Qtest3)

    if(exists("df.1") & exists("df.2")){
        df <- bind_rows(df.1,df.2) %>%
          mutate(qTestN.mg.kg = round(qtest_user.input/CF, digits = 0),
                 CF2 = round(1/(CF/(Bulk.density*(Sample.length/10))), digits = 2),
                 MineralN = round(qtest_user.input*CF2/0.95, digits = 0))
      } else if (exists("df.1") & !exists("df.2")){
          df <- df.1 %>%
            mutate(qTestN.mg.kg = round(qtest_user.input/CF, digits = 0),
                   CF2 = round(1/(CF/(Bulk.density*(Sample.length/10))), digits = 2),
                   MineralN = round(qtest_user.input*CF2/0.95, digits = 0))
      } else if (!exists("df.1") & exists("df.2")){
        df <-  df.2 %>%
          mutate(qTestN.mg.kg = round(qtest_user.input/CF, digits = 0),
                 CF2 = round(1/(CF/(Bulk.density*(Sample.length/10))), digits = 2),
                 MineralN = round(qtest_user.input*CF2/0.95, digits = 0))
      } else {
        df <-  tibble(Texture = " ",
                      Mositure = " ",
                      Sampling.Depth = " ",
                      Description = "Please check the soil tab.")
        }


    df
  })


  # AMN N supply calculation -----
  # if(!is.na(test result))
  # Remaining ON supply (kg/ha) = (crop period - DAP_SD)*data supply rate
  # if(is.na(test result))

  #  Remaining ON supply (kg/ha) = (crop period - DAP_SD )* Default Supply Rate (kg N/day)


  # Data supply rate (kg N/day) = test result * converison coefficient/crop period

  AMN_supply <- reactive({

    AMN_default <- switch (input$input_system,
                           "Mixed cropping/arable" = as.integer(90),
                           "Intensive vegetable production" = as.integer(50),
                           "Pasture conversion" = as.integer(180)
                           )
    if(input$AMN1.1!=0){
      converisonF <- ifelse(crop_period() >= 100, 0.9,
                            ifelse(crop_period() < 40, 0.3, 0.5))
      AMN_supply1 = (crop_period() - DAP_SD()) * (input$AMN1.1 * converisonF/crop_period())
      AMN_supply = round(AMN_supply1, digits = 0)
    } else if(input$AMN1 != 0){
      converisonF <- ifelse(crop_period() >= 100, 0.9,
                            ifelse(crop_period() < 40, 0.3, 0.5))
      AMN_supply1 = (crop_period() - DAP_SD()) * (input$AMN1 * converisonF/crop_period())
      AMN_supply = round(AMN_supply1, digits = 0)
    } else {
      converisonF <- ifelse(crop_period() >= 100, 0.9,
                            ifelse(crop_period() < 40, 0.3, 0.5))
      AMN_supply1 = (crop_period() - DAP_SD()) * (AMN_default * converisonF/crop_period())
      AMN_supply = round(AMN_supply1, digits = 0)
    }

  })


  # total N supply from soil - minN + AMN
  Soil_N_supply <- reactive({
    sn <- sum(soil_filter()$MineralN, na.rm = TRUE) + AMN_supply() # AMN_supply has 3 element.
    sn
  })




# updating user selection -----

  observe({
    # Marketable Yield dropdown list - key component
    updateSelectInput(session,
                      "input_componentYield",
                      label = crop.para_filtered()$Harvested.parameter,
                      choices = crop_filtered()$Harvested.value)



  })

  # back and forward for main tabs: crop, soil and report -----
  observeEvent(
    input$JumpToSoil, {
      updateTabsetPanel(session, "app.tabs",
                        selected = "soil.info")
  })
  observeEvent(
    input$JumpToCrop, {
      updateTabsetPanel(session, "app.tabs",
                        selected = "crop.info")
    })
  observeEvent(
    input$JumpToReport, {
      updateTabsetPanel(session, "app.tabs",
                        selected = "report.tab")
    })
  observeEvent(
    input$JumpToSoil2, {
      updateTabsetPanel(session, "app.tabs",
                        selected = "soil.info")
    })
  # radiobuttom 0-30 cm tab back and forward switches ----
  observeEvent(
    input$nextLayer.1, {
      updateTabsetPanel(session, "soil.tabset.layer.1",
                        selected = "Panel.2")
    })

  observeEvent(
    input$nextLayer.2, {
      updateTabsetPanel(session, "soil.tabset.layer.1",
                        selected = "Panel.1")
    })

  # radiobuttom 0-15 cm tab back and forward switches ----
  observeEvent(
    input$nextLayer.1.1, {
      updateTabsetPanel(session, "soil.tabset.layer.1.1",
                        selected = "Panel.1.2")
    })
  observeEvent(
    input$nextLayer.1.2, {
      updateTabsetPanel(session, "soil.tabset.layer.1.1",
                        selected = "Panel.1.1")
    })
  observeEvent(
    input$nextLayer.1.3, {
      updateTabsetPanel(session, "soil.tabset.layer.1.1",
                        selected = "Panel.2")
    })
  observeEvent(
    input$nextLayer.1.4, {
      updateTabsetPanel(session, "soil.tabset.layer.1.1",
                        selected = "Panel.1.2")
    })
  observeEvent(input$resetSoil,{
    shinyjs::reset("app.tabs")
  })

# Seasonal N balance PANEL -------

  # Crop alone remaining N required (critical calculation) ----
  remaining.crop.N.requirement <- reactive({
    # Remaining.CropN.requirement = estimated.seasonal.Nuptake - (C+A)/(1 + exp(-B*(DAP - M)))
    N_remain <- crop_filtered_1row() %>%
      mutate(N_remain = round(Seasonal.N.uptake - (C + A)/(1 + exp(-B*(DAP_SD() - M))), digits = 0))
  })

  # seasonal N net
  net.whole.season <- reactive({
    net = Soil_N_supply() - remaining.crop.N.requirement()
    net = ifelse(net > 0, paste0(net, "(surplus)"), paste0(net, "(deficit)"))
  })

  # crop n requirement unitl next sampling date, a value -----
  crop.N.req.until.next.SD <- reactive({
    if(input$Sampling.Date >= input$input_nextsamplingDate){
      val <- "NA.sampling date is greater than the next sampling date."
    } else {
      N_require <- crop_filtered_1row() %>%
        mutate(N_require = round(Seasonal.N.uptake - (C + A)/(1 + exp(-B*(DAP_nextSD() - M))), digits = 0)) %>%
        .$N_require
      remaining.crop.N.requirement()$N_remain - N_require
    }
  })

  net.to.next.SD <- reactive({
    if(input$input_nextsamplingDate > input$Sampling.Date){
      Soil_N_supply() - crop.N.req.until.next.SD()
    }
  })

  # Crop N requirement table ----
  N_crop <- reactive({
    tab <- tibble(`Seasonal N Balance`= c("Crop N requirement until next sampling/side dressing date",
                                          "Remaining crop N requirement",
                                          "Estimated total N uptake",
                                          "Soil Plant Avilable N to Plants"),
                  `kg N/ha` = c(crop.N.req.until.next.SD(),
                                remaining.crop.N.requirement()$N_remain,
                                Seasonal.N.uptake(),
                                Soil_N_supply()))

  })




  Crop_N_graphing <- reactive({
    # whole crop n uptake plot ----
    df <- tibble(DAP_annual = seq(0, 365, by = 1),  list(crop_filtered_1row()))

    # critical calculation for plot 1 ----
    df <- unnest(df) %>%
      mutate(Predicted.N.Uptake = (A+C)/(1+exp(-B*(DAP_annual - M))),
             Predicted.N.Uptake = ifelse(Predicted.N.Uptake <0, 0, Predicted.N.Uptake),
             Remaining.N.Requirement = remaining.crop.N.requirement()$N_remain - Predicted.N.Uptake) %>%
      filter(Remaining.N.Requirement > 0 ) %>%
      mutate(N_SD = ifelse(DAP_annual == DAP_SD(), Predicted.N.Uptake, NA),
             N_nextSD = ifelse(DAP_annual == DAP_nextSD(), Predicted.N.Uptake, NA))
  })

  # 1st graph, line plot for N estimation ----
  N_uptake_reactive <- reactive({
    #plotting
    size = 5
    P <- Crop_N_graphing() %>%
      ggplot(aes(x = DAP_annual)) +
      # geom_point(aes(y = Predicted.N.Uptake)) +
      geom_line(aes(y = Predicted.N.Uptake, color = "Predicted.N.Uptake"))+
      geom_point(aes(y = N_SD, shape = "Sampling date"), size = size,  na.rm=TRUE)+
      geom_point(aes(y = N_nextSD, shape = "Next sampling date"),size = size,  na.rm=TRUE)+
      scale_shape_manual(name = "",values =  c(`Sampling date` = 16, `Next sampling date` = 6))+
      scale_color_manual(name = "", values = "red") +
      labs(title = "Estimated whole crop N uptake",
           x = "Days after planting",
           y  = "Whole crop N uptake (kg/ha)")+
      theme_qtmb()


    P
  })


  # 2nd graph, bar plot for different depths ----
  N_supply_depth <- reactive({
    if(is.null(top_layer())){
      cat("User hasn't selected any soil layer.\r\n")
    } else {
      depths <- soil_filter() %>%
        select(MineralN, Sampling.Depth) %>%
        mutate(Depth = paste0(Sampling.Depth, "cm"))

        total <- soil_filter() %>%
          select(MineralN) %>%
          dplyr::summarise(MineralN = sum(MineralN, na.rm = TRUE)) %>%
          mutate(Depth = "Total")
        p <- bind_rows(depths, total) %>%
          ggplot(aes(Depth, MineralN, fill = Depth)) +
          geom_col(width = 0.5) +
          labs(title = "Estimated  soil mineral N supply (from nitrate QT)",
               x = "",
               y  = "Soil mineral N supply (kg/ha)")+
          theme_qtmb() +
          scale_y_continuous(expand = c(0,0), limits = c(0, max(total$MineralN) + 5 ))

        p
      }
    })


  # output section ----

  output$soil_filtered <- DT::renderDataTable({
    # if the user select onthing in the soil tab, show nothing to the user.
    if(is.null(top_layer())){
      cat("Please fill the soil info tab first.\r\n")

    } else {
    tab <- DT::datatable(soil_filter() %>%
                           select(Texture, Moisture, Sampling.Depth, QTest.Results = qtest_user.input, MineralN),
                         options = list(dom = 't',
                                        columnDefs = list(list(className = 'dt-right', targets = '_all'))),
                         rownames = FALSE)
    }
  })
  output$AMN <- renderText({AMN_supply()}) # AMN supply output to UI

  output$period <- renderText({
    paste("<b>The Crop Period: ", crop_period(), "</b>")
  })

  output$distPlot2 <- renderPlot({
    N_supply_depth()
  })

  output$DAP <- renderText({
    paste("<b>Days after planting (t FW/ha): ", DAP_SD(), "</b>")
  })

  output$N_inCrop <- DT::renderDataTable({
    if(is.null(top_layer())){
      cat("Please fill the soil info tab first.\r\n")

    } else {
    tab <- DT::datatable(N_crop(),
                         rownames = FALSE,
                         options = list(dom = 't',
                                        columnDefs = list(list(className = 'dt-right', targets = '_all'))),
                         colnames = c("", colnames(N_crop())[ncol(N_crop())]))
    }
  })


  output$N_graphing <- DT::renderDataTable({
    DT::datatable(Crop_N_graphing())
  })

  output$P_N.uptake <- renderPlot({
    if(is.null(top_layer())){
      cat("Please fill the soil info tab first.\r\n")

    } else {
    N_uptake_reactive()
      }
  })

  output$warning <- renderText({
    if(is.null(top_layer())){
      "Please fill the soil info tab first."
    }
  })

  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() { # https://shiny.rstudio.com/gallery/download-knitr-reports.html
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    content = function(file) {
      params <- list(crop_info = crop_info_reactive(),
                     Soil_N_supply = Soil_N_supply(),
                     p_N_uptake = N_uptake_reactive(),
                     p_N_supply = N_supply_depth(),
                     tab_NCrop = N_crop())

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
      out <- rmarkdown::render('report.Rmd',
                        switch(
                          input$format,
                          PDF = pdf_document(), HTML = html_document(), Word = word_document()
                        ),
                        params = params,
                        envir = new.env(parent = globalenv())
                        )
      file.rename(out, file)
    }
  )
# the end of the server----

})
