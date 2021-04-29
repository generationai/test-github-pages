#Link to file
#https://raw.githubusercontent.com/generationai/test-github-pages/master/master_code_file.R

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

#Make an interactive US map
leaflet() %>% #call the leaflet package function "leaflet" 
  addTiles() #add tiles of every country/geographic feature

#End of code chunk #1
#_____________________________________________________________________#
#Beginning of code chunk #2

counties_coord <- geojson_read("https://raw.githubusercontent.com/generationai/test-github-pages/master/gz_2010_us_050_00_500k.json", what = "sp")

#Create a US map visualization, using the county outlines 
leaflet(data = counties_coord) %>% #input our GeoJSON to the leaflet met
  addTiles() %>%
  setView(-98.5795, 39.828175, zoom = 4) %>% #set our view to the center of the United States
  #Try these latitude and longitude coordinates: -77.007507, 38.900497 - what does it seem to be centered on?
  addPolygons() #Add shapes based on the GeoJSON file

#End of code chunk #2
#_____________________________________________________________________#
#Beginning of code chunk #3

leaflet(data = counties_coord) %>%
  addTiles() %>%
  setView(-98.5795, 39.828175, zoom = 4) %>% #change the zoom level to see an ideal zoom value
  addPolygons(
    color = "#444444", #There are different color codes that can be found online - try changing this to "gold"
    weight = 0.3) #change this value to see how the thickness of the 
#county border changes

#End of code chunk #3
#_____________________________________________________________________#
#Beginning of code chunk #4

recent_covid_counties <- read.csv('https://raw.githubusercontent.com/generationai/test-github-pages/master/covid_county.csv')

qpal <- colorQuantile("YlOrRd", recent_covid_counties$cases) #create a color palette

leaflet(data = counties_coord) %>% 
  addTiles() %>%
  setView(-98.5795, 39.828175, zoom = 4) %>%
  addPolygons(
    color = "#444444",
    fillColor = ~qpal(recent_covid_counties$cases), #fill our polygons with different colors based on the color palette
    weight = 0.3
  ) %>%
  addLegend(pal = qpal, values = ~recent_covid_counties$cases, position = "bottomright", opacity = .3, title = 'Number of COVID-19 Cases') #create a legend using our color palette

#End of code chunk #4
#_____________________________________________________________________#
#Beginning of code chunk #5

ui <- fluidPage(        #UI Component
  h1("This is an app!") #HTML header text 
  #, h2("App created by: ")
)

server <- function(input, output){  #Server Component
                                    #no interactivity or visualizations because this is empty
}

shinyApp(ui = ui, server = server)  #shinyApp() function component

#End of code chunk #5
#_____________________________________________________________________#
#Beginning of code chunk #6

qpal <- colorQuantile("YlOrRd", recent_covid_counties$cases)

ui <- fluidPage(
  h1("This is an app!"),
  leafletOutput("mymap") #tells the UI function to display the heatmap
)
server <- function(input, output){
  output$mymap <- renderLeaflet({ #we add this to render the leaflet heatmap
    leaflet(data = counties_coord) %>% 
      addTiles() %>%
      setView(-98.5795, 39.828175, zoom = 4) %>%
      addPolygons(
        color = "#444444",
        fillColor = ~qpal(recent_covid_counties$cases), 
        weight = 0.3
      ) %>%
      addLegend(pal = qpal, values = ~recent_covid_counties$cases, position = "bottomright", opacity = 1, title = 'Rates of COVID-19 Cases')
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

#try changing the state name (make sure it's lowercase) to see a new graph
ggplotly(new_cases[new_cases$state == 'alabama',] %>% #set the state we're looking at to Alabama
           ggplot( aes(x=date, y=new_cases)) + #set an x value of data and a y value of new cases
           geom_area(fill="#69b3a2", alpha=0.5) + #color in the area under the graph
           geom_line(color="#69b3a2") + #color in the line at the top
           ylab("New Cases") + #create a y axis label
           xlab("Date") + #create an x axis label
           theme_ipsum()) #change the fonts of the text displayed

#End of code chunk #8
#_____________________________________________________________________#
#Beginning of code chunk #9

new_cases <- read.csv("https://raw.githubusercontent.com/generationai/test-github-pages/master/new_cases.csv") %>%
  mutate(date = as.Date(date))

ui <- fluidPage(
  h1("This is my map"),
  selectInput("state_selected", #create an input selector widget 
              "pick a state",
              choice = unique(new_cases$state),
              selected = 'alabama'), #by default show the graph for the State of Alabama
  plotlyOutput("barplot"),
  leafletOutput("mymap")
)

#End of code chunk #9
#_____________________________________________________________________#
#Beginning of code chunk #10

server <- function(input, output){
  output$mymap <- renderLeaflet({
    leaflet(data = counties_coord) %>% 
      addTiles() %>%
      setView(-98.5795, 39.828175, zoom = 4) %>%
      addPolygons(
        color = "#444444",
        fillColor = ~qpal(recent_covid_counties$cases), 
        weight = 0.3
      ) %>%
      addLegend(pal = qpal, values = ~recent_covid_counties$cases, position = "bottomright", opacity = 1, title = 'Rates of COVID-19 Cases')
  })
  
  output$barplot <- renderPlotly({
    ggplotly(new_cases[new_cases$state==input$state_selected,] %>% #filter graph based on user's input
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
  h1("This is my map"),
  selectInput("state_selected", 
              "Pick a State:",
              choice = unique(new_cases$state),
              selected = 'alabama'),
  plotlyOutput("barplot"),
  leafletOutput("mymap")
)

server <- function(input, output){
  output$mymap <- renderLeaflet({
    leaflet(data = counties_coord) %>% 
      addTiles() %>%
      setView(-98.5795, 39.828175, zoom = 4) %>%
      addPolygons(
        color = "#444444",
        fillColor = ~qpal(recent_covid_counties$cases), 
        weight = 0.3
      ) %>%
      addLegend(pal = qpal, values = ~recent_covid_counties$cases, position = "bottomright", opacity = 1, title = 'Rates of COVID-19 Cases')
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
