library(leaflet)
library(shiny)
library(data.table)

d <- fread("annual_all_2015.csv")

shinyServer(function(input, output) {
  output$map1 = renderLeaflet({
    leaflet() %>% addTiles() %>% setView(-97, 41, 4)
  })
  observeEvent(input$add, {
    leafletProxy('map1') %>% addMarkers(
      data = d,
      popup = ~sprintf('PM 2.5 = %s', round(`Arithmetic Mean`, 2)), layerId = rownames(d),
      clusterOptions = markerClusterOptions(), clusterId = 'cluster1'
    )
  })
  observeEvent(input$clear, {
    leafletProxy('map1') %>% clearMarkerClusters()
  })
})