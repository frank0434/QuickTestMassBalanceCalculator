library(shiny)

# b64 <- base64enc::dataURI(file="shinyapp/tests/blue_question_mark.png", mime="image/png")

x<-data.frame(a=c("srinivas asfsis asdfsadf","vassri asdf asdfasdf","csdasdsriasfasf"))
x$a<-gsub("sri","<a target='_blank' href='#' title='Hi, Im a tooltip thingy.!'><img src='blue_question_mark.png' height='13px'/></a>",x$a)
shinyApp( ui = fluidPage(shiny::dataTableOutput("table1")),
          server = function(input, output) {
            output$table1<-shiny::renderDataTable(x, escape=FALSE)
          }
)



