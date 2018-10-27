library(shiny)
library(r2d3)

ui <- fluidPage(
  inputPanel(
    sliderInput("bar_max", label = "Max:",
                min = 0.1, max = 1.0, value = 0.2, step = 0.1)
  ),
  d3Output("d3")
)

server <- function(input, output) {
  output$d3 <- renderD3({
    r2d3(
      runif(5, 0, input$bar_max),
      script = system.file("examples/baranims.js", package = "r2d3")
    )
  })
}

shinyApp(ui = ui, server = server)

