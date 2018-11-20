library(mapsapi)
library(leaflet)
library(xml2)
library(yaml)

maps_api_key <- read_yaml("maps_api_key.yml")

# mapsapi
doc = mp_geocode(addresses = "Zurich Insurance Company Ltd Bratislava", key = maps_api_key)
pnt = mp_get_points(doc)
bounds = mp_get_bounds(doc)

leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = pnt)

leaflet() %>% 
  addTiles() %>%  
  addPolygons(data = bounds)


