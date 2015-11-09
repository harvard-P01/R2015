## ----eval=FALSE----------------------------------------------------------
## f <- "http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/annual_all_2015.zip"
## tmp <- tempfile()
## download.file(f, tmp)
## unzip(tmp, "annual_all_2015.csv")

## ----warning = FALSE-----------------------------------------------------
library(data.table)
dat <- fread("annual_all_2015.csv")
dat

## ------------------------------------------------------------------------
years <- 2010:2015
files <- paste0("http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/annual_all_",
                years, ".zip")
files

## ------------------------------------------------------------------------
download_epa_aqs <- function(years) {
  files <- paste0("http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/annual_all_",
                  years, ".zip")
  for (i in seq(years)) {
    tmp <- tempfile()
    download.file(files[i], tmp)
    unzip(tmp, paste0("annual_all_", years[i], ".csv"))
  }
}

## ----eval = FALSE--------------------------------------------------------
## download_epa_aqs(2000:2002)

## ------------------------------------------------------------------------
library(XML)
f <- "http://energyalmanac.ca.gov/electricity/web_qfer/Heat_Rates.php"
d <- readHTMLTable(f, which = 1)
head(d)

## ----warning=FALSE-------------------------------------------------------
library(rvest)
d <- read_html("https://en.wikipedia.org/wiki/List_of_highest-grossing_films") %>%
  html_node(".wikitable") %>% 
  html_table()

## ----warning=FALSE-------------------------------------------------------
head(d)

## ------------------------------------------------------------------------
read_html("http://www.imdb.com/title/tt0053779/") %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()

## ----eval=FALSE, engine="python"-----------------------------------------
## import imdb
## access = imdb.IMDb()
## movie = access.get_movie(53779)
## print movie # La dolce vita

## ----warning=FALSE-------------------------------------------------------
d <- fread("annual_all_2015.csv")
names(d)
d <- d[! `State Name` %in% c("Hawaii", "Puerto Rico", "Alaska")]
d <- d[`Parameter Code` == 88101]

## ------------------------------------------------------------------------
library(sp)
spD <- copy(d)
coordinates(spD) <- ~ Longitude + Latitude

## ------------------------------------------------------------------------
plot(spD)

## ------------------------------------------------------------------------
library(ggmap, quietly = TRUE)
usa <- map_data("state")
ggplot() + geom_point(data = d, aes(Longitude, Latitude))

## ------------------------------------------------------------------------
ggplot() +
  geom_polygon(data=usa, aes(x = long, y = lat, group = group),
               color = "grey", fill = "white") + 
  geom_point(data = d, aes(Longitude, Latitude))

## ------------------------------------------------------------------------
ggplot() +
  geom_polygon(data=usa, aes(x = long, y = lat, group = group),
               color = "grey", fill = "white") + 
  geom_point(data = d, aes(Longitude, Latitude)) +
  coord_map("polyconic")

## ------------------------------------------------------------------------
ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group),
               color = "grey", fill = "white") + 
  geom_point(data = d, aes(Longitude, Latitude)) +
  coord_map("polyconic", orientation = c(141, -74, 0))

## ------------------------------------------------------------------------
spD <- copy(d)
coordinates(spD) <- ~ Longitude + Latitude
proj4string(spD) <- CRS("+init=epsg:4326")
plot(spD)
spD2 <- spTransform(spD, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
plot(spD2)

## ---- warning=FALSE------------------------------------------------------
set.seed(02138)
small_d <- dplyr::sample_n(d, size = 100)
small_d <- dplyr::mutate(small_d,
                         popup_text = paste0(round(`Arithmetic Mean`, 2)))
library(leaflet)

## ---- warning=FALSE------------------------------------------------------
leaflet(data = small_d) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(~Longitude, ~Latitude, popup = ~popup_text)

## ------------------------------------------------------------------------
leaflet(data = d) %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions()
)

## ------------------------------------------------------------------------
library(googleVis)
op <- options(gvis.plot.tag='chart')

## ----warning=FALSE-------------------------------------------------------
D <- fread("annual_all_2015.csv")
small_d <- dplyr::mutate(small_d,
                         LatLong = paste(Latitude, Longitude, sep = ":"))

## ----results='asis'------------------------------------------------------
gvisGeoChart(small_d, "LatLong", options = list(region = "US"),
             hovervar = "popup_text")

## ----results='asis'------------------------------------------------------
gvisMap(small_d, "LatLong", options = list(region = "US"),
             tipvar = "popup_text")

## ----eval=FALSE----------------------------------------------------------
## shinyUI(fluidPage(
##   leafletOutput('map1'),
##   actionButton('add', 'Add marker cluster'),
##   actionButton('clear', 'Clear marker cluster')
## ))

## ----eval=FALSE----------------------------------------------------------
## d <- fread("annual_all_2015.csv")
## 
## shinyServer(function(input, output) {
##   output$map1 = renderLeaflet({
##     leaflet() %>% addTiles() %>% setView(-97, 41, 4)
##   })
##   observeEvent(input$add, {
##     leafletProxy('map1') %>% addMarkers(
##       data = d,
##       popup = ~sprintf('PM 2.5 = %s', round(`Arithmetic Mean`, 2)), layerId = rownames(d),
##       clusterOptions = markerClusterOptions(), clusterId = 'cluster1'
##     )
##   })
##   observeEvent(input$clear, {
##     leafletProxy('map1') %>% clearMarkerClusters()
##   })
## })

