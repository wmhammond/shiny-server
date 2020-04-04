#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(leaflet)
library(tidyverse)

library(magrittr)
library(rdrop2)
drop_auth(rdstoken = "droptoken.rds")
library(htmltools)
library(htmlwidgets)

jsfile <- "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js" 

df <- drop_read_csv("/OSU/Projects/GTM/shiny/GTM/GTM2.csv", header =T)
df$map.update<-factor(df$map.update)

# Define UI for application that draws a histogram
ui <- fillPage(
    leafletOutput("mymap", height = "100%"),
    tags$head(tags$script(src = jsfile)),
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}")
)

# Define server logic required to draw a histogram
server <- function(input,output, session){
    initial_lat = 0
    initial_lng = 0
    initial_zoom = 3
    data <- reactive({
        x <- df
    })
    
    output$mymap <- renderLeaflet({
        df <- data()
        pal <- colorFactor(c("red", "white", "black", "cyan"), domain=c("2010", "2014", "2015", "2018"))
        m <- leaflet(data = df) %>%
            setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom) %>%
            addTiles(options = providerTileOptions(minZoom = 3, maxZoom = 10, noWrap=TRUE,maxBounds = list(
                list(-90, -180),
                list(90, 180)), maxBoundsViscosity = 1.0)) %>%
            addEasyButton(easyButton(
                icon="fa-globe", title="World View",
                onClick=JS("function(btn, map){ map.setZoom(2); }"))) %>%
#            addControl(html = actionButton("zoomer1","", icon = icon("arrows-alt")), position = "topright") %>%
            addProviderTiles(providers$Esri.WorldImagery,
                             options = providerTileOptions(minZoom = 3, maxZoom = 10,noWrap=TRUE,maxBounds = list(
                                 list(-90, -180),
                                 list(90, 180)),maxBoundsViscosity = 1.0)) %>%
            addMiniMap(tiles = providers$Esri.WorldImagery, toggleDisplay = TRUE,
                       position = "bottomleft") %>%
            addCircleMarkers(lng = ~long,
                       lat = ~lat,
                       color = "NA",
                     # clusterOptions = markerClusterOptions(),
                       fillColor = ~pal(map.update),
                       stroke = TRUE, fillOpacity = 0.75, radius = 7,
                       popup = paste("ID: ", df$ID, "<br>",
                                     "Event Start", df$event.start, "<br>",
                                     "Event End", df$event.end, "<br>",
                                     "References:", df$reference, "<br>",
                                     "<a href =", df$doi, ">",df$doi, "</a>", "<br>"
                       )) %>%
            addLegend(
                "bottomleft",
                color = c("red", "white", "black", "cyan"), 
                labels = c("Allen et al. 2010", "IPCC 2014", "Allen et al. 2015", "Hartmann et al. 2018"),
                title = "Source:"
            ) %>%
            onRender(
                "function(el, x) {
            L.easyPrint({
              sizeModes: ['Current', 'A4Landscape', 'A4Portrait'],
              filename: 'mymap',
              exportOnly: true,
              hideControlContainer: true
            }).addTo(this);
            }"
            )
        m
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
