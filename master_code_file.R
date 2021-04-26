#_____________________________________________________________________#
#Beginning of code chunk #1

install.packages(c("leaflet", "shiny", "dplyr", "geojsonio", "shinyWidgets", "hrbrthemes", "plotly"))

library(leaflet)
library(shiny)
library(dplyr)
library(geojsonio)
library(shinyWidgets)
library(hrbrthemes)
library(plotly)

leaflet() %>%
  addTiles()

#End of code chunk #1
#_____________________________________________________________________#
#Beginning of code chunk #2

counties_coord <- geojson_read("https://raw.githubusercontent.com/generationai/test-github-pages/master/gz_2010_us_050_00_500k.json", what = "sp")

#Create a US map visualization, using the county outlines 
leaflet(counties_coord) %>% 
  addTiles() %>%
  setView(-98.5795, 39.828175, zoom = 4) %>%
  addPolygons()

#End of code chunk #2
#_____________________________________________________________________#
#Beginning of code chunk #3

leaflet(counties_coord) %>%
  addTiles() %>%
  setView(-98.5795, 39.828175, zoom = 4) %>%
  addPolygons(
    color = "#444444", #There are different color codes that can be found online
    weight = 0.3) #change this value to see how the thickness of the 
#county border changes

#End of code chunk #3
#_____________________________________________________________________#
#Beginning of code chunk #4

recent_covid_counties <- read.csv('https://raw.githubusercontent.com/generationai/test-github-pages/master/covid_county.csv')

leaflet(counties_coord) %>% 
  addTiles() %>%
  setView(-98.5795, 39.828175, zoom = 4) %>%
  addPolygons(
    color = "#444444",
    fillColor = ~colorQuantile("YlOrRd", recent_covid_counties$cases)
    (recent_covid_counties$cases), 
    weight = 0.3
  )

#End of code chunk #4
#_____________________________________________________________________#
#Beginning of code chunk #5

ui <- fluidPage(
  h2("This is an app!")
)

server <- function(input, output){
  
}

shinyApp(ui = ui, server = server)

#End of code chunk #5
#_____________________________________________________________________#
#Beginning of code chunk #6

ui <- fluidPage(
  h2("This is an app!"),
  leafletOutput("mymap")
)
server <- function(input, output){
  output$mymap <- renderLeaflet({
    leaflet(counties_coord) %>% 
      addTiles() %>%
      setView(-98.5795, 39.828175, zoom = 4) %>%
      addPolygons(
        color = "#444444",
        fillColor = ~colorQuantile("YlOrRd", recent_covid_counties$cases)(recent_covid_counties$cases), 
        weight = 0.3
      )
  })
}
shinyApp(ui = ui, server = server)

#End of code chunk #6
#_____________________________________________________________________#
#Beginning of code chunk #7

new_cases <- read.csv("https://raw.githubusercontent.com/generationai/test-github-pages/master/new_cases.csv") %>%
  mutate(date = as.Date(date))

#End of code chunk #7
#_____________________________________________________________________#
#Beginning of code chunk #8

new_cases <- read.csv("https://raw.githubusercontent.com/generationai/test-github-pages/master/new_cases.csv") %>%
  mutate(date = as.Date(date))

#try changing the state name to see a new graph
ggplotly(new_cases[new_cases$state == 'alabama',] %>% 
           ggplot( aes(x=date, y=new_cases)) +
           geom_area(fill="#69b3a2", alpha=0.5) +
           geom_line(color="#69b3a2") +
           ylab("New Cases") +
           xlab("Date") +
           theme_ipsum())

#End of code chunk #8
#_____________________________________________________________________#
#Beginning of code chunk #9

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

#End of code chunk #9
#_____________________________________________________________________#
#Beginning of code chunk #10

server <- function(input, output){
  output$mymap <- renderLeaflet({
    leaflet(counties_coord) %>%
      addTiles() %>%
      setView(-98.5795, 39.828175, zoom = 4) %>%
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

#End of code chunk #10
#_____________________________________________________________________#
#Beginning of code chunk #11

new_cases <- read.csv("https://raw.githubusercontent.com/generationai/test-github-pages/master/new_cases.csv") %>%
  mutate(date = as.Date(date))

ui <- fluidPage(
  h2("This is my map"),
  selectInput("state_selected", 
              "pick a state",
              choice = choice = str_to_title(unique(new_cases$state)),
              selected = 'alabama'),
  plotlyOutput("barplot"),
  leafletOutput("mymap")
)

server <- function(input, output){
  output$mymap <- renderLeaflet({
    leaflet(counties_coord) %>%
      addTiles() %>%
      setView(-98.5795, 39.828175, zoom = 4) %>%
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

#End of code chunk #11
#_____________________________________________________________________#





