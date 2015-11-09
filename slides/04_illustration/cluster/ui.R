library(shiny)
library(leaflet)

shinyUI(fluidPage(
  leafletOutput('map1'),
  actionButton('add', 'Add marker cluster'),
  actionButton('clear', 'Clear marker cluster')
))