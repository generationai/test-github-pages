# Copy the code below:


library(leaflet)

library(shiny)

library(dplyr)

library(geojsonio)

library(shinywidgets)

library(hrbrthemes)

library(plotly)

leaflet() %>%
  addTiles()
#_____________________________________________________________________#
counties_coord <- geojson_read("https://raw.githubusercontent.com/generationai/test-github-pages/master/gz_2010_us_050_00_500k.json", what = "sp")

leaflet(counties_coord) %>%
  addTiles() %>%
  addPolygons()
#_____________________________________________________________________#

leaflet(counties_coord) %>%
  addTiles() %>%
  addPolygons(
    color = "#444444",
    weight = 0.3)
#_____________________________________________________________________#

recent_covid_counties <- read.csv('https://raw.githubusercontent.com/generationai/test-github-pages/master/covid_county.csv')

leaflet(counties_coord) %>% 
  addTiles() %>%
  addPolygons(
    color = "#444444",
    fillColor = ~colorQuantile("YlOrRd", recent_covid_counties$cases)(recent_covid_counties$cases), 
    weight = 0.3
  )
#_____________________________________________________________________#

ui <- fluidPage(
  h2("This is an app!")
)

server <- function(input, output){
  
}

shinyApp(ui = ui, server = server)

#_____________________________________________________________________#

ui <- fluidPage(
  h2("This is an app!"),
  leafletOutput("mymap")
)

server <- function(input, output){
  output$mymap <- renderLeaflet({
    leaflet(counties_coord) %>% 
      addTiles() %>%
      addPolygons(
        color = "#444444",
        fillColor = ~colorQuantile("YlOrRd", recent_covid_counties$cases)(recent_covid_counties$cases), 
        weight = 0.3
      )
  })
}

shinyApp(ui = ui, server = server)
#_____________________________________________________________________#

new_cases <- read.csv("https://raw.githubusercontent.com/generationai/test-github-pages/master/new_cases.csv") %>%
  mutate(date = as.Date(date))

ui <- fluidPage(
  h2("This is my map"),
  selectInput("state_selected", 
              "pick a state",
              choice = unique(new_cases$state),
              selected = 'alabama'),
  plotlyOutput("barplot"),
  leafletOutput("mymap")
)

server <- function(input, output){
  output$mymap <- renderLeaflet({
    leaflet(counties_coord) %>%
      addTiles() %>%
      addPolygons(
        color = "#444444",
        fillColor = ~colorQuantile("YlOrRd", recent_covid_counties$cases)(recent_covid_counties$cases),
        weight = 1)
  })
  
  output$barplot <- renderPlotly({
    ggplotly(new_cases[new_cases$state==input$state_selected,] %>% 
               ggplot( aes(x=date, y=new_cases)) +
               geom_area(fill="#69b3a2", alpha=0.5) +
               geom_line(color="#69b3a2") +
               ylab("New Cases") +
               xlab("Date") +
               theme_ipsum())
  })
}
shinyApp(ui=ui, server = server)

