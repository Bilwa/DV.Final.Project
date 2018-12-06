#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate) 
library(leaflet)

setwd("~/Documents/GitHub/DV.Final.Project/")
dat <- read.csv("Street_Lights.csv", na.strings=c(""," "))
load("~/Documents/MS - Data Science/Fall 2018/DV/Project/zipCodes.RData")
dat$zipCodes <- zipCodes

dat1 <- dat %>% 
  select(Lon,Lat, Ownership,Light_Presence,Service,Mast_Arm,Hand_Hold,Inspect_Date,Pole_Type,Address,Lumens)  %>% 
  mutate(Ownership = as.character(Ownership),
         Ownership = ifelse(is.na(Ownership), "Unknown", Ownership),
         Ownership = as.factor(Ownership))   %>% 
  mutate(insp = as.Date(Inspect_Date)) %>% 
  filter(!is.na(Ownership))

zip.list <- unique(dat$zipCodes)
zip.list <- zip.list[zip.list !="Postcode Not Found" ]
zip.list <- append(zip.list,"All")

dat1$popup <- paste("<b>",dat1$Ownership,"</b><br>",
                    "Service: ",dat1$Service,"<br>",
                    "Ownership: ",dat1$Ownership,sep ="")
pal <- colorFactor(palette = 'Set2', domain = dat1$Ownership)

 
ui <- fluidPage(
  
   # Application title
   titlePanel("Street Lights"),
   #sidebarPanel(
   #  selectInput(inputId = "zip_code",label = "Zip Code",choices = zip.list, selected = "All")
   #),
   
  tabsetPanel(
     tabPanel("Map",sidebarPanel(
       selectInput(inputId = "zip_code",label = "Zip Code",choices = zip.list, selected = "All")
     ), leafletOutput("myMap")) ,
   tabPanel("Inspection",plotOutput("InspectPlot")),
   tabPanel("Lumens",plotOutput("LumenPlot")),
   tabPanel("Raw Data", dataTableOutput("table_street"))
     
  )
)

# Define server logic  
server <- function(input, output) {
  
  pal4 <- colorFactor(palette = c("lightseagreen", "salmon","plum","palegoldenrod"), domain = c("AEP","City of South Bend", "Unknown","Other" )) 
  
   output$myMap<- renderLeaflet({
    if (input$zip_code != "All"){
      dat1 <-filter(dat1,zipCodes == input$zip_code)
    }
     
    leaflet()  %>%
    addTiles()  %>%
    addCircleMarkers(data = dat1, popup = ~popup, color = ~pal4(dat1$Ownership), stroke = 0, fillOpacity = 1, radius = 4)  %>% 
    addLegend(pal=pal4,values=dat1$Ownership,title = "Ownership",position="topleft") 
    
   })
   
   output$InspectPlot<- renderPlot({
     
    dat2 <-  dat1 %>% 
       filter(!is.na(insp)) %>% 
       mutate(insp_year = year(insp)) %>% 
       group_by(insp_year) %>% 
       summarise(cnt=n()) %>% 
       filter(insp_year> 2000) 
    ggplot( data= dat2, aes(x= insp_year, y = cnt)) + geom_line(col = "red") + labs(y = "Number of inspections", x = "Year")
     
   })
   
   
   output$LumenPlot<- renderPlot({
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

