Sys.setenv(LANG = 'eng')

library(dplyr)
library(shiny)    # for shiny apps
library(leaflet)  # renderLeaflet function
library(spData)   # loads the world dataset 
library(sf)
library(shinydashboard)
library(shinyWidgets)

source("/Users/aleksander/Desktop/WNE/2nd Semester/Advanced Programming in R/Projekt/Poprawki_Ola/functions_map.R")


# Data preprocessing

world$lifeExp = round(world$lifeExp, 2)   
world$gdpPercap = round(world$gdpPercap)
world$pop = round(world$pop)

world = world %>% 
    left_join(worldbank_df, by = "iso_a2") %>%
    select(name_long, pop, gdpPercap, pop_growth, continent, lifeExp)

 ### UI ###

ui = dashboardPage(
    skin = "purple",
    dashboardHeader(title = "World Explorer"),
    dashboardSidebar(sidebarMenu(
        # https://fontawesome.com/icons?d=gallery
        menuItem("Main page", tabName = "main", icon = icon("globe-europe")),
        menuItem("Life expectancy", tabName = "tab1", icon = icon("baby")),
        menuItem("GDP", tabName = "tab2", icon = icon("money-bill-wave")),
        menuItem("Population", tabName = "tab3", icon = icon("users")),
        menuItem("Population Growth", tabName = "tab4", icon = icon("chart-line"))
    )),
    dashboardBody(
        
        shiny::tagList(
            tabItems(
            tabItem("main",
                        includeMarkdown("main.Rmd")),
            
        ### Life Expectancy ###
            
        tabItem(tabName = "tab1", 
                        
                fluidRow( 
                    box(width = 12,
                        sliderInput(inputId = "life", "Choose range of life expectancy", 49, 84, value = c(60,80), dragRange = T),
                        checkboxGroupInput('continents', 'Choose continents', choices = c("Africa", "Europe", "North America", "South America", "Asia", "Oceania"), inline = T),
                        height = 180),
                
                    box(
                        tabsetPanel(
                        tabPanel('Map', leafletOutput(outputId = "map")),
                        tabPanel('Table', DT::DTOutput('dt_countries'))),
                        width = 12)
                        )
                     ),
        
            
        ### GDP ###
        
        tabItem(tabName = "tab2",
                    
                fluidRow(
                    box(width = 12,
                    sliderInput(inputId = 'gdp', "Choose range of GDP per capita", 0, 100000, value = c(0, 20000), dragRange = T),
                    checkboxGroupInput('continents_gdp', 'Choose continents', choices = c("Africa", "Europe", "North America", "South America", "Asia", "Oceania"), inline = T),
                    height = 180
                        ),
                    box(
                        tabsetPanel(
                        tabPanel('Map', leafletOutput(outputId = 'map_gdp')),
                        tabPanel('Table', DT::DTOutput('countries_gdp'))),
                        width = 12)
                    
                    )
                ),
        
        ### Population ###
        
        tabItem(tabName = 'tab3',
                
                fluidRow(
                    box(width = 12,
                        numericRangeInput(inputId = 'pop', "Choose range of Population", value = c(10000, 2000000)),
                        checkboxGroupInput('continents_pop', 'Choose continents', choices = c("Africa", "Europe", "North America", "South America", "Asia", "Oceania"), inline = T),
                        height = 150),
                    box(
                        tabsetPanel(
                            tabPanel('Map', leafletOutput(outputId = 'map_pop')),
                            tabPanel('Table', DT::DTOutput('countries_pop'))), width = 12)
                    )
                ),
        
        ### Population Growth ###
        
        tabItem(tabName = 'tab4',
                
            sidebarLayout(
                sidebarPanel(
                        textInput(inputId = 'country', "Enter a country"),
                        numericInput(inputId = 'year', 'Enter a year', 2022)
                
            ),
                mainPanel(
                    textOutput('pop_growth')
                )
            )
        )
    )
)))

### SERVER ###

server = function(input, output) {
    
    ### Life Expectancy ###
    
    output$dt_countries <- DT::renderDT({
        world %>%
            filter(
                continent %in% input$continents,
                lifeExp > input$life[1],
                lifeExp < input$life[2]
            ) %>%
            select(name_long, lifeExp) %>%
            arrange(desc(lifeExp))
    })
    
    output$map = renderLeaflet({
        #validate(
        #  need(world[world$lifeExp < input$life & world$continent %in% input$continents, ] != "", "No countries match choosen criteria!")
        #)
        #leaflet() %>% addProviderTiles("Stamen.Toner") %>%
        #    addPolygons(data = world[world$lifeExp > input$life[1] & world$lifeExp < input$life[2] & world$continent %in% input$continents, ])
    
    labels <- sprintf(
        "<strong>%s</strong><br/>%g",
        world$name_long, world$lifeExp
    ) %>% lapply(htmltools::HTML)
    
    bins <- c(40, 50, 60, 70, 75, 80, Inf)
    pal <- colorBin("Blues", domain = world$lifeExp, bins = bins)
    
    lifeExp_map <- leaflet(world) %>%
        addProviderTiles("Stamen.Toner")
    
    lifeExp_map %>% 
        addPolygons(
            fillColor = ~pal(lifeExp),
            weight = 2,
            opacity = 3,
            color = "white",
            dashArray = "1",
            fillOpacity = 1.2,
            highlight = highlightOptions(
                weight = 2,
                color = "blue",
                fillOpacity = 0.7),
            label = labels,
            labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "2px 6px"),
                textsize = "10px")) %>%
        addLegend(pal = pal, values = ~lifeExp, opacity = 0.7, position = "bottomright")

})
    
    
    ### GDP ###
    
    output$countries_gdp <- DT::renderDT({
        world %>%
            filter(
                continent %in% input$continents_gdp,
                gdpPercap > input$gdp[1],
                gdpPercap < input$gdp[2]
            ) %>%
            select(name_long, gdpPercap, pop) %>%
            arrange(desc(gdpPercap))
    })
    
    
    output$map_gdp = renderLeaflet({
        
        labels <- sprintf(
            "<strong>%s</strong><br/>%g US dollars per capita",
            world$name_long, world$gdpPercap
        ) %>% lapply(htmltools::HTML)
        
        bins <- c(0, 1000, 5000, 10000, 25000, 45000, Inf)
        pal <- colorBin("Greens", domain = world$gdpPercap, bins = bins)
    
        gdp_map <- leaflet(world) %>%
            addProviderTiles("Thunderforest.Pioneer")
        
        gdp_map %>% 
            addPolygons(
                fillColor = ~pal(gdpPercap),
                weight = 2,
                opacity = 3,
                color = "white",
                dashArray = "1",
                fillOpacity = 1.2,
                highlight = highlightOptions(
                    weight = 2,
                    color = "blue",
                    fillOpacity = 0.7),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "2px 6px"),
                    textsize = "10px")) %>%
            addLegend(pal = pal, values = ~gdpPercap, opacity = 0.7, position = "bottomright")
        
    })
    
    
    ### Population ###
    
    output$countries_pop <- DT::renderDT({
        world %>%
            filter(
                continent %in% input$continents_pop,
                pop > input$pop[1],
                pop < input$pop[2]
            ) %>%
            select(name_long, pop, gdpPercap) %>%
            arrange(desc(pop))
    })
    
    
    output$map_pop = renderLeaflet({
        
        world$pop = world$pop / 1000000 # To present population in milions on map
        world$pop = round(world$pop, 3)
        
        # setting map options
        
        labels <- sprintf(
            "<strong>%s</strong><br/>%g milion people",
            world$name_long, world$pop
        ) %>% lapply(htmltools::HTML)
        bins <- c(0, 1, 10, 25, 50, 200, Inf)
        pal <- colorBin("YlOrRd", domain = world$pop, bins = bins)
        
        # plotting map
        
        gdp_map <- leaflet(world) %>%
            addProviderTiles("Stamen.Toner")
        
        gdp_map %>% 
            addPolygons(
                fillColor = ~pal(pop),
                weight = 2,
                opacity = 3,
                color = "white",
                dashArray = "1",
                fillOpacity = 1.2,
                highlight = highlightOptions(
                    weight = 2,
                    color = "blue",
                    fillOpacity = 0.7),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "2px 10px"),
                    textsize = "10px")) %>%
            addLegend(pal = pal, values = ~pop, opacity = 0.7, position = "bottomright", title = 'Population in millions')
        
        
    })
    
    # Population Growth
    
    pop <- eventReactive(input$country, {
        
        future_pop(input$country, input$year)
        
    })
    
    output$pop_growth <- renderPrint({
        pop()
    })
    
}
shinyApp(ui, server)

