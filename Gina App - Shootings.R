#data processing
    #set wd
    setwd("~/Google Drive/ND MS Data Science/Fall 2018: Data Visualization/Week 4 - Shiny Apps/")
    
    #import the data
    shootings <- read_csv("Criminally_Assaulted_Shootings.csv")
    
    #remove missing values
    shootings <- na.omit(shootings)
    
    #create simple dates from date-time variable USER_Date
    shootings$simple_date <- format(as.Date(shootings$USER_Date), '%A, %B %d, %Y')
    
    #create weekday from USER_Date
    shootings$weekday <- format(as.Date(shootings$USER_Date), '%A')
    

#load libraries
library(shiny)
library(ggplot2)
library(leaflet)
library(shinyWidgets)
library(DT)

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("South Bend Criminally Assaulted Shootings"),
  
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
            tabPanel("Map",leafletOutput("mymap")),
            tabPanel("Graph", DT::dataTableOutput("graph")),
            tabPanel("Raw Data", DT::dataTableOutput("table"))
                    )
                )
          )
        )

# Define server logic
server <- function(input, output) {
   
  ##THE MAP
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
  
  #reactive data for the map
  map_data_react <- reactive({
    shootings %>% filter(USER_Year > input$year) %>% filter(weekday > input$weekday)
  })
  
  
  #leaflet map
  shootings_map <- leaflet(shootings) %>%
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addProviderTiles(providers$Esri.NatGeoWorldMap, options = providerTileOptions(opacity = 1)) %>%
    addCircleMarkers(data = shootings.spatial, popup = ~popup, color=~color_1(USER_Fatal), stroke = 0, fillOpacity=1) %>%
    addLegend("bottomright", pal = color_1, values = ~USER_Fatal,
              title = 'Fatality',
              opacity = 1)
  
  #maplibrary(DT)
  output$mymap <- renderLeaflet({
    shootings_data <- map_data_react()
    
    shootings_map
  })
  
  
  #THE HISTOGRAM
  #reactive data for the histogram
  #data <- reactive(input$year, {return(shootings[shootings$USER_Year >= input$year[1] & shootings$USER_Year <= input$year[2], ])
  # })
  
  output$graph <- renderPlot(
    ggplot(data = shootings, aes(x = USER_Fatal)) + 
      geom_bar(stat = 'count') + 
      xlab('Fatality Status') + 
      ylab('Count of Status')
  )
 
  
  #THE DATA TABLE
  #limited data for data table output
  shootings_limited <- shootings %>% select(simple_date, USER_Year, weekday, USER_Address, USER_Fatal)
  #relabel variables
  colnames(shootings_limited) <- c('Day & Date', 'Year', 'Day of the Week', 'Address', 'Fatality')
  #data table output
  output$table <- renderDataTable(shootings_limited)
    
}
 
# Run the application 
shinyApp(ui = ui, server = server)










