## System
conn <- dbConnect(RSQLite::SQLite(), "shinyapp/qtmb_data.sqlite3")
df <- crop.yield %>%
  filter(Crop == the_crop)

df$Harvested.value
the_crop <- subset(crop.para, Crop_name_display == "Barley grain - Autumn sown")$Crop
ui <- fluidPage(
  p("The checkbox group controls the select input"),
  checkboxGroupInput("inCheckboxGroup", "Input checkbox",
                     c("Item A", "Item B", "Item C")),
  selectInput("inSelect", "Select input",
              c("Item A", "Item B", "Item C"))
)

server <- function(input, output, session) {
  observe({
    x <- input$inCheckboxGroup

    # Can use character(0) to remove all choices
    if (is.null(x))
      x <- character(0)

    # Can also set the label and select items
    updateSelectInput(session, "inSelect",
                      label = paste("Select input label", length(x)),
                      choices = x,
                      selected = tail(x, 1)
    )
  })
}

shinyApp(ui, server)

df <- readRDS("data/df.rds")

shinyServer(function(input, output) {

  datasetInput <- reactive({

    switch(input$var,
           "ALL" = df,
           "A2" = df %>% filter(country == "A2"),
           "AE" = df %>% filter(country == "AE"))


    switch(input$plat,
           "ALL" = df,
           "Android" = df %>% filter(platform == "Android"),
           "IPhonePlayer" = df %>% filter(platform == "IPhonePlayer"))



  })


  output$view <- renderTable({head(datasetInput())})

})

vv <- iris$Sepal.Length
names(vv) <- vv
names(vv)
vv["5.9"]


# conditional panel -------------------------------------------------------

sidebarPanel(
  selectInput(
    "plotType", "Plot Type",
    c(Scatter = "scatter",
      Histogram = "hist")),

  # Only show this panel if the plot type is a histogram
  conditionalPanel(
    condition = "input.plotType == 'hist'",
    selectInput(
      "breaks", "Breaks",
      c("Sturges",
        "Scott",
        "Freedman-Diaconis",
        "[Custom]" = "custom")),

    # Only show this panel if Custom is selected
    conditionalPanel(
      condition = "input.breaks == 'custom'",
      sliderInput("breakCount", "Break Count", min=1, max=1000, value=10)
    )
  )
)



ui <- fluidPage(sidebarLayout(
  sidebarPanel(
    sliderInput("controller", "Controller", 1, 3, 1)
  ),
  mainPanel(
    tabsetPanel(id = "inTabset",
                tabPanel(title = "Panel 1", value = "panel1", "Panel 1 content"),
                tabPanel(title = "Panel 2", value = "panel2", "Panel 2 content"),
                tabPanel(title = "Panel 3", value = "panel3", "Panel 3 content")
    )
  )
))

server <- function(input, output, session) {
  observeEvent(input$controller, {
    updateTabsetPanel(session, "inTabset",
                      selected = paste0("panel", input$controller)
    )
  })
}

shinyApp(ui, server)



library(shiny)
library(shinyjs)

ui <- tagList(
  useShinyjs(),
  navbarPage(
    "shinyjs with navbarPage",
    tabPanel("tab1",
             actionButton("button", "Click me"),
             div(id = "hello", "Hello!")),
    tabPanel("tab2")
  )
)

server <- function(input, output, session) {
  observeEvent(input$button, {
    toggle("hello")
  })
}

shinyApp(ui, server)





#add net row to the summary table -----

p <- iris %>%
  ggplot( aes(Sepal.Length, Sepal.Width, color= Species)) +
  geom_point() +
  scale_color_continuous()
class(p)
p
str(p)
p == "Error: Discrete value supplied to continuous scale"
is.na(p)
