#RShinyTutorial
library(shiny)

ui <- navbarPage(title = "Random generator",
  tabPanel(title = "Plotting",
           fluidRow(
                    column(2, tags$h1(style = "font-family:Impact", "R Shiny")),
                    HTML("<h2>Tutorial</h2>"),
                    tags$a(href = "https://shiny.rstudio.com/", "RShiny"),
                    tags$strong("Tutorial begins below:"),
                    tags$img(
                      height = 100,
                      width = 300,
                      src = "http://www.rstudio.com/images/RStudio.2x.png"
                  ),
                  
                  
    tags$hr(),
    
    
    fluidRow(wellPanel(
      sliderInput(
        inputId = "num",
        label = "Choose a number",
        value = 25,
        min = 1,
        max = 100
      ),
      textInput(
        inputId = "title",
        label = "Write a title",
        value = "Histogram of Normal values"
      )
    )),
    actionButton(inputId = 'clicks',
                 label = "Show summary"),
    verbatimTextOutput("stats"),
    
    fluidRow(
      column(5, plotOutput("hist")),
      column(
        5,
        actionButton(inputId = "norm",
                     label = "Normal values for histogram"),
        actionButton(inputId = "unif",
                     label = "Uniform values for histogram"),
        plotOutput("other_hist")
      )
    )
  )
),
tabPanel(title = "Normal Plotting",
         plotOutput("hist1")),
tabPanel(title = "Uniform Plotting",
         plotOutput("other_hist1"))
)



server <- function(input, output) {
  eventReact <- eventReactive(input$clicks, {
    summary(data())
  })
  data <- reactive({
    rnorm(input$num)
  })
  
  
  
  rv <- reactiveValues(data = rnorm(100))
  observeEvent(input$norm, {
    rv$data <- rnorm(100)
  })
  observeEvent(input$unif, {
    rv$data <- runif(100)
  })
  

  output$hist <- renderPlot({
    title <- "Histogram"
    hist(data(), main = input$title)
  })
  
  output$hist1 <- renderPlot({
    title <- "Histogram"
    hist(data(), main = input$title)
  })
  
  output$other_hist <- renderPlot({
    title <- "Histogram"
    hist(rv$data, main = isolate(input$title))
  })
  
  output$other_hist1 <- renderPlot({
    title <- "Histogram"
    hist(rv$data, main = isolate(input$title))
  })
  
  output$stats <- renderPrint({
    eventReact()
  })
  observeEvent(input$clicks, {
    print(as.numeric(input$clicks))
  })
}

shinyApp(ui = ui, server = server)
