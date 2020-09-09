source("dataprocessing.R", local = TRUE)

#############################################################################################

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Indian Population"),
  dashboardSidebar(
    h4("Please select a variable to avoid errors:"),
    tags$hr(),
    radioButtons(
      "pop",
      h3("Variable:"),
      choices = list(
        "Population" = 'Population',
        "Urban Population" = 'Urban.population',
        "Rural Population" = 'Rural.population'
      ),
      selected = character(0)
    )
  ),
  dashboardBody(tabsetPanel(
    id = "tab_being_displayed",
    tabPanel("ggplot", withLoader(
      plotOutput(outputId = "ggplot", height = "80vh")
    ), loader = "loader10"),
    tabPanel(
      "Leaflet",
      h4("Please Wait, leaflet takes few seconds to load"),
      withLoader(leafletOutput(outputId = "leaflet", height = "80vh")),
      loader = "loader10"
    )
  ))
)

#############################################################################################

server <- function(input, output) {
  x <- reactive({
    combined_df %>% pull(input$pop)
  })
  output$ggplot <- renderPlot({
    ggplot(combined_df) +
      geom_sf(aes(fill = as.numeric(x()))) +
      scale_fill_gradient(low = "grey", high = "blue") +
      geom_text(
        data = location,
        aes(x = X, y = Y, label = abb),
        color = 'white',
        size = 2
      ) +
      theme_bw() +
      guides(fill = guide_legend(title = "Range Indicator"))
  })
  
  observe({
    req(input$tab_being_displayed == "Leaflet")
    x <- reactive({
      combined_df %>% pull(input$pop)
    })
    pal <-
      colorBin("RdYlBu",
               domain = as.numeric(x()),
               bins = 11)
    labels <- paste("<p>",
                    map$ST_NM,
                    "</p>",
                    "<p>",
                    input$pop,
                    ":",
                    " ",
                    x(),
                    "</p>",
                    sep = "")
    leafletProxy("leaflet") %>%
      addPolygons(
        data = map,
        weight = 1,
        smoothFactor = 0.5,
        color = "white",
        fillOpacity = 0.8,
        fillColor = ~ pal(as.numeric(x())),
        highlight = highlightOptions(
          weight = 1,
          color = "#666666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = lapply(labels, HTML)
      )
    leafletProxy("leaflet") %>%
      clearControls() %>%
      addLegend(
        pal = pal,
        values = input$pop,
        opacity = 0.7,
        position = "topright"
      )
  })
  
  output$leaflet <- renderLeaflet({
    leaflet(combined_df) %>%
      addTiles() %>%
      addProviderTiles("CartoDB") %>%
      setView(lng = 80,
              lat = 25,
              zoom = 4.4)
  })
}

shinyApp(ui = ui, server = server)