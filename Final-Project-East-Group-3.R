#Clear the Environment
rm(list=ls()) 

library(shiny)
library(markdown)
library(ggplot2)
library(leaflet)
library(reshape2)
library(shiny)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(lubridate) 

#DATA
setwd("C:/Users/Owner/Desktop/Data Science/Data Viz/Data-Viz-2018-Fall-master/FinalProject")

##Parks and Facilities
parks <- read.csv("Parks_Locations_and_Features.csv")
facilities <- read.csv("Public_Facilities.csv")

##Abandoned Props
load("./Abandoned_Properties_Geocoded.v3.Rdata")
abandoned_properties <- apdf
abandoned_properties$Outcome_St <- as.character(abandoned_properties$Outcome_St)
abandoned_properties$Direction <- as.character(abandoned_properties$Direction)
abandoned_properties$Street_Nam <- as.character(abandoned_properties$Street_Nam)
abandoned_properties$Suffix <- as.character(abandoned_properties$Suffix)
abandoned_properties$Code_Enfor <- as.character(abandoned_properties$Code_Enfor)
abandoned_properties$Property_S <- as.character(abandoned_properties$Property_S)
abandoned_properties$Program_De <- as.character(abandoned_properties$Program_De)
abandoned_properties$Date_of_Ou <- as.Date(abandoned_properties$Date_of_Ou)
abandoned_properties$Council_Di <- as.character(abandoned_properties$Council_Di)
abandoned_properties$Zip_Code <- as.character(abandoned_properties$Zip_Code)
abandoned_properties$OBJECTID <- as.numeric(abandoned_properties$OBJECTID)
abandoned_properties$County_Tax <- as.character(abandoned_properties$County_Tax)
abandoned_properties$State_Parc <- as.character(abandoned_properties$State_Parc)
abandoned_properties$Address_Nu <- as.character(abandoned_properties$Address_Nu)
abandoned_properties$Structures <- as.character(abandoned_properties$Structures)
abandoned_properties[abandoned_properties$Outcome_St %in% NA,]$Outcome_St <- "Unknown"
#abandoned_properties[abandoned_properties$Structures %in% NA,]$Structures <- "Unknown"
#abandoned_properties[abandoned_properties$Zip_Code %in% NA,]$Zip_Code <- "Unknown"
colnames(abandoned_properties)[1] <- "Outcome"
#colnames(abandoned_properties)[10] <- "ZipCode"

##Shootings
#import the data
shootings <- read_csv("Criminally_Assaulted_Shootings.csv")
#remove missing values
shootings <- na.omit(shootings)
#create simple dates from date-time variable USER_Date
shootings$simple_date <- format(as.Date(shootings$USER_Date), '%A, %B %d, %Y')
#create weekday from USER_Date
shootings$weekday <- format(as.Date(shootings$USER_Date), '%A')
#define table as spatialpointsdf
shootings.spatial <- SpatialPointsDataFrame(coords = shootings[,c("X", "Y")], 
                                            data = shootings, 
                                            proj4string = CRS("+proj=longlat +dat=WGS84"))
#define pin marker popup
shootings.spatial$popup <- paste("<b> Fatality: </b>", shootings.spatial$USER_Fatal,"<br>",
                                 "<b> Date: </b>", shootings.spatial$simple_date, "<br>",
                                 "<b> Address: </b>", shootings.spatial$USER_Address)
#color palette
color_1 <- colorFactor(palette = c('navy', 'red'), domain = shootings$USER_Fatal)

##Street Lights
dat <- read.csv("Street_Lights.csv", na.strings=c(""," "))
load("./zipCodes.RData")
dat$zipCodes <- zipCodes

dat1 <- dat %>% 
  select(Lon,Lat, Ownership,Light_Presence,Service,Mast_Arm,Hand_Hold,Inspect_Date,Pole_Type,Address,Lumens)  %>% 
  mutate(Ownership = as.character(Ownership),
         Ownership = ifelse(is.na(Ownership), "Unknown", Ownership),
         Ownership = as.factor(Ownership))   %>% 
  mutate(insp = as.Date(Inspect_Date))

zip.list <- unique(dat$zipCodes)
zip.list <- zip.list[zip.list !="Postcode Not Found" ]
zip.list <- append(zip.list,"All")

dat1$popup <- paste("<b>",dat1$Ownership,"</b><br>",
                    "Service: ",dat1$Service,"<br>",
                    "Ownership: ",dat1$Ownership,sep ="")

pal <- colorFactor(palette = 'Set2', domain = dat1$Ownership)



##Zip Codes for Input Select Object
Zips <- c(as.list(as.integer(names(table(parks$Zip_Code)))),as.list(as.integer(names(table(facilities$POPL_ZIP))))) %>%
        unlist() %>%
        sort() %>%
        as.factor() %>%
        as.data.frame()

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

#APP

## Define UI for application
ui <- navbarPage("South Bend City Data",
                 
  ###############PARKS AND FACILITIES################ 
  tabPanel("Parks and Facilities",
         # Application title
         titlePanel("Parks and Facilities"),
         #Main panel graphics
         mainPanel(
           selectInput('zip', 'Zip Code', unique(Zips$.),multiple = TRUE,selectize=TRUE),
            tabsetPanel(
              tabPanel("Map",leafletOutput("ParkFacsMap")),
              tabPanel("Parks",plotOutput("barPar")),
              tabPanel("Parks Data",dataTableOutput("tabPar")),
              tabPanel("Facilities",plotOutput("barFac")),
              tabPanel("Facilities Data",dataTableOutput("tabFac"))
              
            )
            
            )
   ),
  
  ###############ABANDONED PROPS################
  tabPanel("Abandoned Properties",
           # Application title
           titlePanel("Abandoned Properties"),
          tabsetPanel(
           tabPanel("Map",
                   #dateRangeInput(inputId = "dates", label = "Date range", startview = "year",start="2013-01-01"),
                   leafletOutput(outputId = "apMap")
          ),
           tabPanel("Plot",
                   selectInput(inputId = "variable", label = "Variable:",choices = c("Outcome","Zip_Code")),
                   #selectInput(inputId = "variable", label = "Variable:",choices = c("Outcome")),
                   dateRangeInput(inputId = "dates", label = "Date range", startview = "year",start="2013-01-01"),
                   plotOutput(outputId = "distPlot")
          ),
          tabPanel("Raw Data",
                   dataTableOutput("apData")            
          )
          )
  ),
  
  ###############SHOOTINGS################
  tabPanel("Shootings",
           
         # Application title
         titlePanel("Criminally Assaulted Shootings"),
         
         #select dropdown picker for day of week
         pickerInput('weekday', 'Day of the Week:',
                     choices = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
                     options = list('actions-box' = TRUE, size = 10, 'selected-text-format' = "count > 3"),
                     multiple = TRUE),
         
         #sidebar layout details
         sidebarLayout(
           sidebarPanel(
             #slider adjustments
             sliderInput('year', "Year:",
                         min = min(shootings$USER_Year), max = max(shootings$USER_Year),
                         timeFormat = "%Y", sep = "", step = 1,ticks = 3,
                         value = c(max(shootings$USER_Year), max(shootings$USER_Year))
             )
           ),
           
           #tabs of the dashboard
           mainPanel(
             tabsetPanel(
               tabPanel("Map",leafletOutput("mymapShoot")),
               tabPanel("Graph", plotOutput("graphShoot")),
               tabPanel("Raw Data", DT::dataTableOutput("tableShoot"))
             )
           )
         )
  ),
  
  ###############STREET LIGHTS################
  tabPanel("Street Lights",
           # Application title
           titlePanel("Street Lights"),
           #sidebarPanel(
           #  selectInput(inputId = "zip_code",label = "Zip Code",choices = zip.list, selected = "All")
           #),
           
           tabsetPanel(
             tabPanel("Map",sidebarPanel(
               selectInput(inputId = "zip_code",label = "Zip Code",choices = zip.list, selected = "All")
             ), leafletOutput("myMapStreets")) ,
             tabPanel("Inspection",plotOutput("InspectPlotStreets")),
             tabPanel("Lumens",plotOutput("LumenPlotStreets")),
             tabPanel("Raw Data", dataTableOutput("table_street"))
           )
           
  )
)

## Define server logic
server <- function(input, output) {
  
########PARKS and FACILITIES################
    #Define the leaflet map object
    mapParksFacs <- leaflet(parks)%>%#filter(parks,parks$Zip_Code==c(input$zip)))  %>%
      addProviderTiles(providers$Esri.WorldImagery)  %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, options = providerTileOptions(opacity = 1)) %>%
      addCircleMarkers(data = parks.spatial,popup = ~popup, color=~pal3(Park_Type), stroke = 0, fillOpacity = 1, radius = 6) %>%
      addLegend(pal=pal3,values=~Park_Type,title = "Park Type",position="topleft") %>%
      addCircleMarkers(data = facilities.spatial, popup = ~popup, color=~pal4(facilities$POPL_TYPE), stroke = 0, fillOpacity = 1, radius = ifelse(facilities$POPL_TYPE=="POLICE STATION",9,3)) %>%
      addLegend(pal=pal4,values=facilities$POPL_TYPE,title = "Facility",position="topleft")
    
    ###MAP###
     output$ParkFacsMap <- renderLeaflet({
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
        
        if(sum(sites$Lat)==0) {
          return(NULL)
        } else {
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
        }
        
        facs <- facilities %>% 
          filter(facilities$POPL_ZIP %in% zip)
        
        facs.spatial <- SpatialPointsDataFrame(coords=facs[,c("Lon","Lat")], data = facs,
                                                     proj4string = CRS("+proj=longlat +datum=WGS84"))
        
        facs.spatial$popup <- paste("<b>",facs.spatial@data$POPL_NAME,"</b><br>",
                                          "Type: ",facs.spatial@data$POPL_TYPE,"<br>",
                                          "Zip Code: ",facs.spatial@data$POPL_ZIP,sep ="")
       
        if(sum(sites$Lat)==0) {
          leafletProxy('ParkFacsMap') %>% 
            clearMarkers() %>% 
          addCircleMarkers(lng = facs.spatial@data$Lon,
                           lat = facs.spatial@data$Lat,
                           popup = facs.spatial@data$popup, 
                           color=pal4(facs.spatial@data$POPL_TYPE),
                           stroke = 0, 
                           fillOpacity = 1,
                           radius = ifelse(facs.spatial@data$POPL_TYPE=="POLICE STATION",9,3))
        } else {
        leafletProxy('ParkFacsMap') %>% 
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
        }
      },autoDestroy = FALSE)
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
     output$tabPar <- renderDataTable({
       if(is.null(reactParks()))
         return(NULL)
       reactParks()
       })
  
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
     output$tabFac <- renderDataTable({
       if(is.null(reactFacs()))
         return(NULL)
       reactFacs()
       })
   
########ABANDONED PROPERTIES################   
   output$distPlot <-  renderPlot({
     ggplot(data=abandoned_properties,
            aes(x=abandoned_properties[,input$variable])) +
       geom_bar(stat="count") +
       xlab(input$variable)
     
   })
   output$apMap <-  renderLeaflet({
     abandoned_properties$popup <- paste("<b>", abandoned_properties$Outcome,"</b><br>",
                                         "Structures:", abandoned_properties$Structures,"<br>",
                                         "Address:", abandoned_properties$accuracy, sep = " ")
     pal <- colorFactor(palette = 'Set1', domain = abandoned_properties$Outcome)
     #leaflet(data= data())%>%
     leaflet(data = abandoned_properties)%>%
       addTiles()%>%
       #       addMarkers(~lon, ~lat, popup = ~OBJECTID)
       addMarkers(data = abandoned_properties,
                  popup = ~popup,
                  clusterOptions = markerClusterOptions()) %>%
       addCircleMarkers(data = abandoned_properties,
                        popup = ~popup,
                        color = ~pal(Outcome),
                        stroke = 0,
                        fillOpacity = 1,
                        radius = 5) %>%
       addLegend(pal = pal,
                 values = ~Outcome,
                 title = "Outcome",
                 position = "topright")
   })
   output$texty <- renderText(paste(input$dates[1]))
   
   output$apData <- renderDataTable({abandoned_properties})

########SHOOTINGS################   
   ##THE MAP
   #reactive data for the map
   map_data_react <- reactive({
     sub <- shootings.spatial[(shootings.spatial@data$USER_Year >= input$year[1] & 
                                 shootings.spatial@data$USER_Year <= input$year[2]),]
     if (length(input$weekday) > 0){
       sub <- sub[sub@data$weekday %in% input$weekday,]
     }
     return(sub)
   })
   
   
   #leaflet map
   output$mymapShoot <- renderLeaflet({
     shootings_data <- map_data_react()
     
     shootings_map <- leaflet(shootings) %>%
       addProviderTiles(providers$Esri.WorldImagery) %>%
       addProviderTiles(providers$Esri.NatGeoWorldMap, options = providerTileOptions(opacity = 1)) %>%
       addCircleMarkers(data = shootings_data, popup = ~popup, color=~color_1(USER_Fatal), stroke = 0, fillOpacity=1) %>%
       addLegend("bottomright", pal = color_1, values = ~USER_Fatal,
                 title = 'Fatality',
                 opacity = 1)
   })
   
   
   #THE HISTOGRAM
   #reactive data for the histogram
   #data <- reactive(input$year, {return(shootings[shootings$USER_Year >= input$year[1] & shootings$USER_Year <= input$year[2], ])
   # })
   
   output$graphShoot <- renderPlot(
     ggplot(data = map_data_react()@data, aes(x = USER_Fatal,fill=USER_Fatal)) + 
       geom_bar(stat = 'count') + 
       scale_fill_manual(values = c("Yes"="red","No"="navy"),guide=FALSE)+
       xlab('Fatality Status') + 
       ylab('Count of Status')
   )
   
   table_data_react <- reactive({
     sub <- shootings[(shootings$USER_Year >= input$year[1] & 
                                 shootings$USER_Year <= input$year[2]),]
     if (length(input$weekday) > 0){
       sub <- sub[sub$weekday %in% input$weekday,]
     }
     return(sub)
   })
   
   #THE DATA TABLE
   #data table output
   output$tableShoot <- renderDataTable({
     if(is.null(table_data_react()))
       return(NULL)
     #limited data for data table output
     shootings_limited <- table_data_react() %>% select(simple_date, USER_Year, weekday, USER_Address, USER_Fatal)
     #relabel variables
     colnames(shootings_limited) <- c('Day & Date', 'Year', 'Day of the Week', 'Address', 'Fatality')
     shootings_limited
   })
   
########STREET LIGHTS################   
   pal5 <- colorFactor(palette = c("lightseagreen", "salmon","plum","palegoldenrod"), domain = c("AEP","City of South Bend", "Unknown","Other" ))
   
   output$myMapStreets<- renderLeaflet({
     if (input$zip_code != "All"){
       dat1 <-filter(dat1,zipCodes == input$zip_code)
     }
     leaflet()  %>%
       addTiles()  %>%
       addCircleMarkers(data = dat1, popup = ~popup, color = ~pal5(dat1$Ownership), stroke = 0, fillOpacity = 1, radius = 4)  %>% 
       addLegend(pal=pal5,values=dat1$Ownership,title = "Ownership",position="topleft")
     
   })
   
   output$InspectPlotStreets<- renderPlot({
     
     dat2 <-  dat1 %>% 
       filter(!is.na(insp)) %>% 
       mutate(insp_year = year(insp)) %>% 
       group_by(insp_year) %>% 
       summarise(cnt=n()) %>% 
       filter(insp_year> 2000) 
     ggplot( data= dat2, aes(x= insp_year, y = cnt)) + geom_line(col = "red") + labs(y = "Number of inspections", x = "Year")
     
   })
   
   output$LumenPlotStreets<- renderPlot({
     dat %>% 
       filter(Service != 'Unknown') %>% 
       group_by(Lumens, Service) %>% 
       summarise(cnt = n()) %>% 
       filter(cnt>10) %>% 
       ggplot(aes(x=Lumens, y=cnt, fill = Service))+geom_col()+coord_flip()
   })
   
   dat_street <- dat %>% 
     select(zipCodes, Ownership,Service,Hand_Hold,Inspect_Date,Pole_Type,Lumens)
   
   output$table_street <- renderDataTable(dat_street)
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)
