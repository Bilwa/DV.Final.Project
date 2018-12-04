#Clear the Environment
rm(list=ls()) 

#DATA
##CSVs
setwd("C:/Users/Owner/Desktop/Data Science/Data Viz/Data-Viz-2018-Fall-master/FinalProject")
parks <- read.csv("Parks_Locations_and_Features.csv")
facilities <- read.csv("Public_Facilities.csv")

##Zip Codes for Input Select Object
Zips <- c(as.list(as.integer(names(table(parks$Zip_Code)))),as.list(as.integer(names(table(facilities$POPL_ZIP))))) %>%
        unlist() %>%
        sort() %>%
        as.factor() %>%
        as.data.frame()


#APP
library(shiny)
library(ggplot2)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("South Bend Parks and Facilities"),
   
   mainPanel(
     selectInput('zip', 'Zip Code', unique(Zips$.),multiple = TRUE,selectize=TRUE),
      tabsetPanel(
        tabPanel("Map",leafletOutput("mymap")),
        tabPanel("Parks",plotOutput("barPar"),dataTableOutput('tabPar')),
        tabPanel("Facilities",plotOutput("barFac"),dataTableOutput("tabFac"))
      )
      
      )
   )

# Define server logic required to display map
server <- function(input, output) {
  
########PARKS and FACILITIES################
  ###Define the table as a SpatialPointsDataFrame
  parks.spatial <- SpatialPointsDataFrame(coords = parks[,c("Lon","Lat")], data = parks,
                                          proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  facilities.spatial <- SpatialPointsDataFrame(coords=facilities[,c("Lon","Lat")], data = facilities,
                                               proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  ###Define the Popup menu items
  parks.spatial$popup <- paste("<b>",parks.spatial$Park_Name,"</b><br>",
                               "Type: ",parks.spatial$Park_Type,"<br>",
                               "Zip Code: ",parks.spatial$Zip_Code,"<br>",
                               "Concessions: ",parks.spatial$Concessions,"<br>",
                               "Event Spaces: ",parks.spatial$Event_Space,"<br>",
                               "Shelters: ",parks.spatial$Shelter,"<br>",
                               "Structures: ",parks.spatial$Structure,"<br>",
                               "Loop Walks: ",parks.spatial$Loop_Walk,sep ="")
  
  facilities.spatial$popup <- paste("<b>",facilities.spatial$POPL_NAME,"</b><br>",
                                    "Type: ",facilities.spatial$POPL_TYPE,"<br>",
                                    "Zip Code: ",facilities.spatial$POPL_ZIP,sep ="")
  ###Define the color palettes
  pal3 <- colorFactor(palette = 'Greens', domain =parks$Park_Type)
  pal4 <- colorFactor(palette = c("red","tan","navy"), domain = c("FIRE STATION","POLICE STATION","LIBRARY"))
  
  #Define the leaflet map object
  mapParksFacs <- leaflet(parks)%>%#filter(parks,parks$Zip_Code==c(input$zip)))  %>%
    addProviderTiles(providers$Esri.WorldImagery)  %>%
    addProviderTiles(providers$Esri.NatGeoWorldMap, options = providerTileOptions(opacity = 1)) %>%
    addCircleMarkers(data = parks.spatial,popup = ~popup, color=~pal3(Park_Type), stroke = 0, fillOpacity = 1, radius = 6) %>%
    addLegend(pal=pal3,values=~Park_Type,title = "Park Type",position="topleft") %>%
    addCircleMarkers(data = facilities.spatial, popup = ~popup, color=~pal4(facilities$POPL_TYPE), stroke = 0, fillOpacity = 1, radius = ifelse(facilities$POPL_TYPE=="POLICE STATION",9,3)) %>%
    addLegend(pal=pal4,values=~facilities$POPL_TYPE,title = "Facility",position="topleft")
#########################################
  
###MAP###
   output$mymap <- renderLeaflet({
    mapParksFacs
   })
    
###To make the map reactive to inputs###
    observe({
      
      zip <- if(is.null(input$zip)) {
        unique(Zips$.)
      } else {
        input$zip
      }
      
      sites <- parks %>% 
        filter(parks$Zip_Code %in% zip)
      
      sites.spatial <- SpatialPointsDataFrame(coords = sites[,c("Lon","Lat")], data = sites,
                                              proj4string = CRS("+proj=longlat +datum=WGS84"))
      
      sites.spatial$popup <- paste("<b>",sites.spatial@data$Park_Name,"</b><br>",
                                   "Type: ",sites.spatial@data$Park_Type,"<br>",
                                   "Zip Code: ",sites.spatial@data$Zip_Code,"<br>",
                                   "Concessions: ",sites.spatial@data$Concessions,"<br>",
                                   "Event Spaces: ",sites.spatial@data$Event_Space,"<br>",
                                   "Shelters: ",sites.spatial@data$Shelter,"<br>",
                                   "Structures: ",sites.spatial@data$Structure,"<br>",
                                   "Loop Walks: ",sites.spatial@data$Loop_Walk,sep ="")
      
      facs <- facilities %>% 
        filter(facilities$POPL_ZIP %in% zip)
      
      facs.spatial <- SpatialPointsDataFrame(coords=facs[,c("Lon","Lat")], data = facs,
                                                   proj4string = CRS("+proj=longlat +datum=WGS84"))
      
      facs.spatial$popup <- paste("<b>",facs.spatial@data$POPL_NAME,"</b><br>",
                                        "Type: ",facs.spatial@data$POPL_TYPE,"<br>",
                                        "Zip Code: ",facs.spatial@data$POPL_ZIP,sep ="")
      
      leafletProxy('mymap') %>% 
        clearMarkers() %>% 
        addCircleMarkers(lng = sites.spatial@data$Lon,
                         lat = sites.spatial@data$Lat,
                         popup = sites.spatial@data$popup, 
                         color=pal3(sites.spatial@data$Park_Type),
                         stroke = 0, 
                         fillOpacity = 1,
                         radius = 6) %>%
        addCircleMarkers(lng = facs.spatial@data$Lon,
                        lat = facs.spatial@data$Lat,
                        popup = facs.spatial@data$popup, 
                        color=pal4(facs.spatial@data$POPL_TYPE),
                        stroke = 0, 
                        fillOpacity = 1,
                        radius = ifelse(facs.spatial@data$POPL_TYPE=="POLICE STATION",9,3))
    })
###End of reactive map code###
   
###To make other visualizations reactive###
    ###Parks###
    reactParks <- reactive({
      #req(input$zip)
      if(is.null(input$zip)) {
        df <- parks
      } else if(sum(parks$Zip_Code %in% input$zip)>0) {
        df <- parks[parks$Zip_Code %in% input$zip,]
      } else {
        df <- parks
      }
    })
    ###Facilities###
    reactFacs <- reactive({
      #req(input$zip)
      if(is.null(input$zip) | (sum(facilities$POPL_ZIP %in% input$zip)<1)) {
        df <- facilities
      } else {
        df <- facilities[facilities$POPL_ZIP %in% input$zip,]
      }
    })
    
###Park Bar Chart###
   output$barPar <- renderPlot({
     ggplot(reactParks(),aes(x=reorder(reactParks()$Park_Type,table(reactParks()$Park_Type)[reactParks()$Park_Type]),fill=reactParks()$Park_Type))+
       geom_bar()+
       coord_flip()+
       scale_fill_brewer(palette="Greens",guide=FALSE)+
       xlab("")+
       scale_y_continuous(position = "right")+
       ylab("Number of Parks")+
       theme_classic()
   })
   
###Park Data Table###
   output$tabPar <- renderDataTable({reactParks()})

###Facilities Bar Chart###    
   output$barFac <- renderPlot({
     ggplot(reactFacs(),aes(x=reorder(reactFacs()$POPL_TYPE,table(reactFacs()$POPL_TYPE)[reactFacs()$POPL_TYPE]),fill=reactFacs()$POPL_TYPE))+
       geom_bar()+
       coord_flip()+
       scale_fill_manual(values = c("FIRE STATION"="red","LIBRARY"="tan","POLICE STATION"="navy"),guide=FALSE)+
       xlab("")+
       scale_y_continuous(position = "right")+
       ylab("Number of Facilities")+
       theme_classic()     
     
   })
   
###Facilities Data Table###
   output$tabFac <- renderDataTable({reactFacs()})
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)
