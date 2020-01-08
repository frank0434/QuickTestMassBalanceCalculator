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

source("functions.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
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

  ## paddock information for multi tests
  paddock <- reactive({
    validate(
      need(!is.null(input$input_paddock.id), "")
    )
    as.character(input$input_paddock.id)
  })

  ## make a data frame to feed into the downloadable report
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
                           format(as.Date(input$input_PlantingDate, format = "%F"), "%d %B %Y"),
                           format(as.Date(input$Sampling.Date, format = "%F"), "%d %B %Y"),
                           format(as.Date(input$input_nextsamplingDate, format = "%F"), "%d %B %Y")))
  })

  ## the crop life time - it is calcuated from the consumption of estimated whole season N uptake
  ## the number of growing days depends on the crop
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
  #take the radiobutton inputs as the key
  top_layer <- reactive({input$samplingDepth})

  # if the user chose 0-15 then only process and show the appropriate sampling depth
  depth.15.1 <- reactive({
    if(top_layer() == layer.1.1){
          as.integer(15) # constrain the type
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
    # if user choose one of the top layer, proceed.
    # otherwise, pass the message to the report tab in the UI via Qtest.15.1 or other objects
    validate(
      need(top_layer() == layer.1.1, "Please chose the sampling method.")
    )
    val0_15 <- as.integer(input$Qtest1.1)
    validate(
      need(val0_15 >= 0, warning_soil.tab)
    )
    val0_15
    })

  Qtest.15.2 <- reactive({

    validate(
      need(top_layer() == layer.1.1, "Please chose the sampling method.")
    )
    val15_30 <- as.integer(input$Qtest1.2)
    validate(
      need(val15_30 >= 0, warning_soil.tab)
    )
    val15_30
  })

  Qtest.30.2 <- reactive({
    # two conditionalPanle both have the 30-60 layer - need to provide twice
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
      val30_60 <- as.integer(input$Qtest2.1)
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
    validate(
      need(top_layer() == layer.1, "Please chose the sampling method.")
    )
    val0_30 <- as.integer(input$Qtest1)
    validate(
      need(val0_30 >= 0, warning_soil.tab)
    )
    val0_30
  })

  ## 60-90 is disabled

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
               Sampling.Depth = "15-30")
      # depth 0-15 & 15-30
      df.1 <- bind_rows(df.1.1, df.1.2)
      df.2 <- soil %>%
        filter(
          Texture == input$Texture.2,
          Moisture == input$Moisture.2,
          Sampling.Depth == "30-60") %>%
        mutate(qtest_user.input = Qtest.30.2(),
               Sample.length = as.integer(30))
    } else if (top_layer() == layer.1){
      # depth 0-30
      df.1 <- soil %>%
        filter(Texture == input$Texture.1,
               Moisture == input$Moisture.1,
               Sampling.Depth == "0-30") %>%
        mutate(qtest_user.input = Qtest.30.1(),
               Sample.length = as.integer(30))
      df.2 <- soil %>%
        filter(
          Texture == input$Texture.2.1,
          Moisture == input$Moisture.2.1,
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
  # Remaining ON supply (kg/ha) = (crop period - DAP_SD )* Default Supply Rate (kg N/day)
  # Data supply rate (kg N/day) = test result * converison coefficient/crop period

  AMN_supply <- reactive({
    # the raw data is from the excel file. two small to keep in a tab in the sqlite file
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
    # updating by crop name
    updateSelectInput(session,
                      "input_componentYield",
                      label = crop.para_filtered()$Harvested.parameter,
                      choices = crop_filtered()$Harvested.value)
  })

  # back and forward for main tabs: crop, soil and report -----
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
  # refresh button ----
  observeEvent(input$refresh, {
    shinyjs::js$refresh()
  })

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
      val <- "NA. Sampling date must be smaller than the next sampling date."
    } else {
      N_require <- crop_filtered_1row() %>%
        mutate(N_require = round(Seasonal.N.uptake - (C + A)/(1 + exp(-B*(DAP_nextSD() - M))), digits = 0)) %>%
        .$N_require
      remaining.crop.N.requirement()$N_remain - N_require
    }
  })

  net.to.next.SD <- reactive({
    # if(input$input_nextsamplingDate > input$Sampling.Date){
      net <- Soil_N_supply() - crop.N.req.until.next.SD()
      net <- ifelse(net > 0, "No extra N needed to next sampling/side dressing date", as.numeric(-net))
    # } else {
      # val <- "NA. Sampling date must be smaller than the next sampling date."
    # }
  })

  # Crop N requirement table ----
  N_crop <- reactive({
    validate(
      need(input$input_nextsamplingDate > input$Sampling.Date, "Sampling date must be smaller than the next sampling date.")
    )

    tab <- tibble(`Seasonal N Balance`= c("Estimated Total Crop N uptake",
                                          # "Estimated Plant Avilable N",
                                          "Remaining crop N requirement"
                                          # "N Required to Reach Target Yield"
                                          ),
                  `kg N/ha` = c(Seasonal.N.uptake(),
                                # Soil_N_supply(),
                                Crop_N_graphing() %>%
                                  filter(DAP_annual == DAP_SD()) %>%
                                  .$Remaining.N.Requirement %>%
                                  round(.)
                                # paste0(Soil_N_supply() - Seasonal.N.uptake(), "(",x,")")
                                ))
    })
  report.tab_2 <- reactive({
    validate(
      need(input$input_nextsamplingDate > input$Sampling.Date, "")
    )

    x <- ifelse(Soil_N_supply() > Seasonal.N.uptake(), "Surplus", "Deficit")
    tab <- tibble(`Seasonal N Balance`= c("Estimated Plant Available N",
                                          "N Required to Reach Target Yield",
                                          paste0("N Required to Next SD (", input$input_nextsamplingDate, ")")),
                  `kg N/ha` = c(Soil_N_supply(),
                                paste0(Soil_N_supply() - Seasonal.N.uptake(), "(",x,")"),
                                net.to.next.SD()
                                ))
    })
  # Quick test result + AMN supply table
  table_soil_N <- reactive({
    validate(
      need(nrow(soil_filter()) > 0, "Please provide quick test information.")
    )
    no.ofRows <- nrow(soil_filter())-1
    times <- ifelse(no.ofRows < 0 ,0 , no.ofRows)
    table <- soil_filter() %>%
      select(Texture, Moisture, Sampling.Depth, QTest.Results = qtest_user.input, MineralN) %>%
      mutate(AMN = c(AMN_supply(), rep(0, times)),
             SubTotal = as.numeric(AMN) + MineralN)
    })
  # key intermediate data - crop growing period and N uptake to draw 1st plot ----
  Crop_N_graphing <- reactive({

    df <- tibble::tibble(DAP_annual = seq(0, 365, by = 1),  list(crop_filtered_1row()))
    df <- unnest(df,cols = c(`list(crop_filtered_1row())`)) %>%
      mutate(Predicted.N.Uptake = A+C/(1+exp(-B*(DAP_annual - M))),
             Predicted.N.Uptake = ifelse(Predicted.N.Uptake <0, 0, Predicted.N.Uptake),
             Remaining.N.Requirement = remaining.crop.N.requirement()$N_remain - Predicted.N.Uptake) %>%
      filter(Remaining.N.Requirement > 0 ) %>%
      mutate(N_SD = ifelse(DAP_annual == DAP_SD(), Predicted.N.Uptake, NA),
             N_nextSD = ifelse(DAP_annual == DAP_nextSD(), Predicted.N.Uptake, NA))
  })

  # debugging the issue 14----
  output$df_graph <- DT::renderDataTable({Crop_N_graphing()})
  output$df_graph2 <- DT::renderDataTable({crop_filtered_1row()})
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
    # maybe we can cut this plot down to make the app mobile friendly !!!
    validate(
      need(!is.null(top_layer()), warning_report.tab)
    )
    validate(
      need(input$input_nextsamplingDate > input$Sampling.Date, "Sampling date must be smaller than the next sampling date.")
    )
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
    })
# Report tab ----
  output$soil_filtered <- DT::renderDataTable({
    # if the user select nothing in the soil tab, show nothing to the user.
    validate(
      need(!is.null(top_layer()), warning_report.tab)
    )

    tab <- DT::datatable(table_soil_N()%>%
                           add_row(.,
                                   Texture = "",
                                   Moisture = "",
                                   Sampling.Depth = "",
                                   QTest.Results = "",
                                   MineralN = "",
                                   AMN = "",
                                   SubTotal = paste0("Total plant available N: ",sum(.$SubTotal), "kg/ha"))%>%
                           rename('Sampling.Depth (cm)' = Sampling.Depth,
                                  'QTest.Results (mg/L)' = QTest.Results,
                                  'Mineral N (kg/ha)' = MineralN,
                                  'AMN (kg/ha)' = AMN,
                                  'SubTotal (kg/ha)' = SubTotal),
                         options = list(dom = 't',
                                        columnDefs = list(list(className = 'dt-left', targets = '_all'))),
                         rownames = FALSE)

  })

  output$report.table2 <- DT::renderDataTable({
    DT::datatable(report.tab_2(),
                  options = list(dom = 't',
                                 columnDefs = list(list(className = 'dt-left', targets = '_all'))),
                  rownames = FALSE,
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
    tab <- DT::datatable(N_crop(),
                         rownames = FALSE,
                         options = list(dom = 't', #https://datatables.net/reference/option/dom
                                        columnDefs = list(list(className = 'dt-left', targets = '_all'))),
                         colnames = c("", colnames(N_crop())[ncol(N_crop())])) %>%
      DT::formatStyle(#https://rstudio.github.io/DT/010-style.html
        'Seasonal N Balance',
        target = 'row',
        backgroundColor = DT::styleEqual("N Required to Reach Target Yield", "yellow")
      )

  })

  output$P_N.uptake <- renderPlot({
    validate(
      need(!is.null(top_layer()), warning_report.tab),
      need(crop_period() > DAP_nextSD() & crop_period() > DAP_SD(),
           "Your next sampling date must be within the crop growing period.")
    )
        N_uptake_reactive()

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
                     Soil_N_supply = table_soil_N(),
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

  output$qTestResults.csv <- downloadHandler(


    filename = function() {
      paste0('my Quick Test Result', sep = '.', switch(
        input$format_data, csv = 'csv', Excel = "xlsx"
      ))
    },
    content = function(file) {
      if(input$format_data == "Excel"){
        openxlsx::write.xlsx(x = table_soil_N() %>%
                               mutate(Paddock = paddock()), file = file)
      } else{
        write.csv(table_soil_N()%>%
                    mutate(Paddock = paddock()), file,row.names = FALSE)
      }
    }


  )
# the end of the server----

})
