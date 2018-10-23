#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tagList(
    tags$head(
      tags$link(rel="stylesheet", type="text/css",href="style.css")
    )
  ),
  
  div(class = "login",
      uiOutput("uiLogin"),
      textOutput("pass"),
      tags$head(tags$style("#pass{color: red;"))
  ), 
  
  fluidRow(
    column(3,
           div(class = "span1", 
               uiOutput("obs")
           )
    ),
    column(8,
           div(class = "logininfo",
               uiOutput("userPanel")
           ),
           hr(),
           div(class = "DataTable", 
               uiOutput('dataTable')
           ) 
    ) 
  ) 
  
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  USER <- reactiveValues(Logged = FALSE , session = session$user) 
  
  source("www/Login.R", local = TRUE)
  
  getDat <- eventReactive(input$search,{
    withProgress(
      message = 'Calculation in progress',
      detail = 'get iris data', value=0 , {
        
        setSpecies <- isolate(input$selectSpecies)
        
        incProgress(0.5)
        
        if (!is.null(setSpecies)) {
          Dat <- iris[which(iris$Species %in% setSpecies),] 
        } else {
          Dat <- NULL
        }
        
        setProgress(1)
      })
    return(Dat)
  }) 
  
  output$obs <- renderUI({ 
    if (USER$Logged == TRUE) { 
      list(
        selectizeInput(
          'selectSpecies', 'Select iris Species', choices = as.character(unique(iris$Species)), multiple = TRUE
        ),
        actionButton('search', 'Search')
      )
    } 
  }) 
  
  output$dataTable <- renderUI({ 
    if (USER$Logged == TRUE) { 
      dataTableOutput('table')
    }
  })
  
  output$table <- renderDataTable(
    getDat(),
    options = list(
      pageLength = 100,
      lengthMenu = c(50,100,200,500)
    ) 
  )
  
}


# Run the application 
shinyApp(ui = ui, server = server)

