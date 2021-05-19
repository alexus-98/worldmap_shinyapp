Sys.setenv(LANG = 'en')

library(dplyr)
library(shiny)    # for shiny apps
library(leaflet)  # renderLeaflet function
library(spData)   # loads the world dataset 
library(sf)

world$lifeExp = round(world$lifeExp, 2)
ui = fluidPage(
  
  # App Title
  titlePanel('WorldMap'),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "life", "Life expectancy", 49, 84, value = 80),
      checkboxGroupInput('continents', 'Choose continents', choices = unique(world$continent))
      ),
    mainPanel(
      tabsetPanel(
        tabPanel('Map', leafletOutput(outputId = "map")),
        tabPanel('Table', DT::DTOutput('dt_countries'))
      )
    )
  )
)
server = function(input, output) {
  
  output$dt_countries <- DT::renderDT({
    world %>%
      filter(
        world$continent %in% input$continents,
        lifeExp < input$life
      ) %>%
      select(name_long, lifeExp) %>%
      arrange(desc(lifeExp))
  })
  
  output$map = renderLeaflet({
    #validate(
    #  need(world[world$lifeExp < input$life & world$continent %in% input$continents, ] != "", "No countries match choosen criteria!")
    #)
    leaflet() %>% addProviderTiles("Stamen.Toner") %>%
      addPolygons(data = world[world$lifeExp < input$life & world$continent %in% input$continents, ])
  })
}
shinyApp(ui, server)

