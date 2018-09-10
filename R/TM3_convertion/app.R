## app.R #
library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(sp)
library(dplyr)

list_kabupaten <- read.csv(file = here::here("list_kabupaten.csv"), header = T)
unique(list_kabupaten$Propinsi)
choices_prop = setNames(list_kabupaten$Propinsi,list_kabupaten$Propinsi)

ui <- dashboardPage(skin = "blue",
  dashboardHeader(disable = FALSE,
                  title = "BPN Map Viewer"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map View", tabName = "dashboard", icon = icon("map")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"),
               badgeLabel = "new", badgeColor = "green")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
                         .content-wrapper {
                           background-color: white !important;
                         }
                         .main-sidebar {
                           background-color: black !important;
                         }
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: black;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: black;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: black;
                              }
                       "))
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",

              fluidRow(
                column(width = 12,
                  fluidRow(
                    box(width = 3,
                        title = "Inputs", status = "primary", solidHeader = TRUE,
                        collapsible = FALSE,
                        textInput("Coordinates", "Coordinates from BPN (Sertifikat Tanah)*"),
                        h6(tags$i("*use comma separated format, example: 12345, 56789")),br(),
                        fluidRow(
                          column(width = 6,
                                 uiOutput("selectPropinsi")),
                          column(width = 6,
                                 uiOutput("selectKabupaten"))
                        )
                    ),
                    
                    box(width = 9,
                        title = "Map Preview", status = "primary", solidHeader = TRUE,
                        "Below are the map preview from your coordinates entry", br(),
                        "Click on the pin to get the converted coordinates",
                        htmlOutput("copy_coord"),
                        leafletOutput("mymap")
                    )))
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)

server <- function(input, output) {
  output$selectPropinsi <- renderUI(
    selectInput("selectProp","Provinsi:", choices= 
                  as.character(unique(unlist(list_kabupaten$Propinsi))),
                selected = "Bali")
  )
  
  filter_prop <- reactive({
    list_kabupaten %>% 
      filter(Propinsi %in% input$selectProp)
  })
  
  output$selectKabupaten <- renderUI(
    selectInput("selectKab","Kota/Kabupaten:", choices= 
                  as.character(unique(unlist(filter_prop()$Kota.atau.Kabupaten))),
                selected = "Denpasar")
  )
  
  filter_kab <- reactive({
    list_kabupaten %>% 
      filter(Kota.atau.Kabupaten %in% input$selectKab)
  })
  
  CRS_code <- reactive({
    # paste0('+proj=tmerc +lat_0=0 +lon_0=',filter_kab()$lng_adj, '+k=0.9999 +x_0=200000 +y_0=1500000 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
    paste0('+init=epsg:',filter_kab()$EPSG)
  })
  
  location <- reactive({
    lokasi <- data.frame(long = as.numeric(unlist(strsplit(input$Coordinates,","))[1]), lat = as.numeric(unlist(strsplit(input$Coordinates,","))[2]))
    coordinates(lokasi) <- ~long+lat
    proj4string(lokasi) <- CRS(CRS_code())
    spTransform(lokasi, CRS('+init=epsg:4326'))
  })
  
  output$mymap <- renderLeaflet({
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = "red"
    )
    coord_input <- input$Coordinates
    if (coord_input != ""){
      leaflet() %>%
        # addTiles(group = "OSM (default)") %>%
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google', group = "Google Map") %>% 
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=s,h&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google', group = "Hybrid(map + satellite)") %>%
        # addMarkers(lng=location()@coords[1], lat=location()@coords[2], popup=paste0("Coordinates: ",location()@coords[2],", ",location()@coords[1])) %>% 
        addAwesomeMarkers(lng=location()@coords[1], lat=location()@coords[2], icon=icons, label = paste0("Coordinates: ",location()@coords[2],", ",location()@coords[1])) %>% 
        addLayersControl(
          baseGroups = c("Google Map","Hybrid(map + satellite)"),
          options = layersControlOptions(collapsed = FALSE)
        )
    } else {
      leaflet() %>%
        setView(lat = -2.6000285, lng = 118.015776, zoom = 5) %>% 
        # addTiles(group = "OSM (default)") %>%
        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google', group = "Google Map") %>% 
        addTiles(urlTemplate = "http://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google', group = "Hybrid(map + satellite)") %>%
        addLayersControl(
          baseGroups = c("Google Map","Hybrid(map + satellite)"),
          options = layersControlOptions(collapsed = FALSE)
        )
    }
  })
  
  output$copy_coord <- renderUI({
    inputcheck <- input$Coordinates
    if (inputcheck=="") {
      return(NULL)
    } else {
    HTML(
      paste0("Coordinates: ",location()@coords[2],", ",location()@coords[1])
    )}
  })
  
}

shinyApp(ui, server)
