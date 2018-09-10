library(rgdal)
library(leaflet)

shp <- readOGR(dsn = "Phase4-polygon.shp")
shp2 <- readOGR(dsn = "Phase4-polyline.shp")

proj4string(shp) <- CRS('+init=epsg:32648')
proj4string(shp2) <- CRS('+init=epsg:32648')
shp <- spTransform(shp, CRS('+init=epsg:4326'))
shp2 <- spTransform(shp2, CRS('+init=epsg:4326'))

leaflet() %>% 
  addPolygons(data=shp,
              fillColor = "transparent",
              weight = 1, smoothFactor = 1,
              fillOpacity = 0) %>% 
  addPolylines(data=shp2,
               fillColor = "transparent",
               weight = 1, smoothFactor = 1,
               fillOpacity = 0.1)# %>%
addTiles(urlTemplate = "http://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google', group = "Hybrid(map + satellite)")


library(leaflet.extras)

mydrawPolylineOptions <- function (allowIntersection = TRUE, 
    drawError = list(color = "#b00b00", timeout = 2500), 
    guidelineDistance = 20, metric = TRUE, feet = FALSE, zIndexOffset = 2000, 
    shapeOptions = drawShapeOptions(fill = FALSE), repeatMode = FALSE) {
    leaflet::filterNULL(list(allowIntersection = allowIntersection, 
        drawError = drawError, guidelineDistance = guidelineDistance, 
        metric = metric, feet = feet, zIndexOffset = zIndexOffset,
        shapeOptions = shapeOptions,  repeatMode = repeatMode)) }

leaflet() %>% 
  addPolygons(data=shp,
              fillColor = "transparent",
              weight = 1, smoothFactor = 1,
              fillOpacity = 0) %>% 
  addPolylines(data=shp2,
               fillColor = "transparent",
               weight = 1, smoothFactor = 1,
               fillOpacity = 0.1) %>% 
  addDrawToolbar(
    polylineOptions = mydrawPolylineOptions(metric=TRUE, feet=FALSE),
    editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions())) %>% 
  addMeasure(primaryLengthUnit="meters")


#plot crest and toe
crest <- readOGR(dsn = "crest-polyline.shp")
toe   <- readOGR(dsn = "toe-polyline.shp")

proj4string(crest) <- CRS('+init=epsg:32648')
proj4string(toe)   <- CRS('+init=epsg:32648')
crest <- spTransform(crest, CRS('+init=epsg:4326'))
toe   <- spTransform(toe, CRS('+init=epsg:4326'))

leaflet() %>% 
  setView(lat = 43.006, lng = 106.849, zoom = 14) %>%
  addPolylines(data=toe, color = "blue", dashArray = 3,
               fillColor = "transparent",
               weight = 1, smoothFactor = 1,
               fillOpacity = 0.1, group = "Crest and Toe") %>% 
  addPolylines(data=crest, color = "purple",
               fillColor = "transparent",
               weight = 1, smoothFactor = 1,
               fillOpacity = 0, group = "Crest and Toe") %>% 
  # addPolygons(data=shp,
  #             fillColor = "transparent",
  #             weight = 1, smoothFactor = 1, group = "Phase 4a") %>% 
  # addPolylines(data=shp2,
  #              fillColor = "transparent",
  #              weight = 1, smoothFactor = 1, group = "Phase 4a") %>% 
  addDrawToolbar(polylineOptions = mydrawPolylineOptions(metric=TRUE, feet=FALSE),
                 editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions())) %>% 
  addMeasure(primaryAreaUnit = "sqmeters", primaryLengthUnit="meters") %>% 
  addTiles(urlTemplate = "http://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google', group = "Hybrid(map + satellite)") %>% 
  addLayersControl(baseGroups = c("Crest and Toe"),
                   overlayGroups = c("Hybrid(map + satellite)"),
                   options = layersControlOptions(collapsed = FALSE))

