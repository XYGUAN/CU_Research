# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)
library(leaflet)
library(ggvis)
library(dplyr)
load("~/Google Drive/Xunyang_Fall16Spring17/Hilsa fish research/3.Analysis/RScript/shinyapp/data/WaterLevelTimeSeries_No_Human.RData")
load("~/Google Drive/Xunyang_Fall16Spring17/Hilsa fish research/3.Analysis/RScript/shinyapp/data/WaterLevelTimeSeries_All.RData")
load("~/Google Drive/Xunyang_Fall16Spring17/Hilsa fish research/3.Analysis/RScript/shinyapp/data/Overview_DATA.RData")
Input_path <- "~/Google Drive/Xunyang_Fall16Spring17/Hilsa fish research/2.Data"

Names <- names(WaterLevel_No_Human_Mean_Monsoon)[2:6]
Names_NoHuman <- c("Bahadurabad", "Chilmari", "Kanaighat", "Sarighat", "Sherpur")

Analytics_Types <- c("Year vs. Monsoon",
                     "Year vs. NonMonsoon",
                     "Year vs. Production",
                     "Monsoon vs. Production",
                     "Monsoon vs. NonMonsoon"
)

Locations <- names(WaterLevel_All_Mean_Monsoon)[-1]

Production_Locations <- c("Noakhali"   ,             "Bhola"       ,            "Barisal"      ,          
                           "Lakshmipur"    ,          "Sariatpur"    ,           "Chandpur"    ,           
                           "Munshiganj"       ,       "Narayanganj"   ,          "Comilla"    ,            
                           "Narshingdi"       ,       "Brahmanbaria"  ,        "Kishoreganj" ,           
                           "Habiganj"      ,          "Sariatpur.1"  ,           "Madaripur"   ,           
                           "Munshiganj.1"   ,         "Dhaka"       ,            "Manikganj"   ,           
                           "Faridpur"       ,         "Rajbari"   ,              "Rajbari.1"  ,            
                           "Kushtia"       ,          "Pabna"     ,              "Natore"     ,            
                           "Rajshahi"     ,           "C..Nawabganj" ,           "Manikganj.1" ,           
                           "Pabna.1"    ,             "Tangail"      ,           "Sirajganj"   ,           
                           "Bogra"     ,              "Jamalpur"     ,           "Gaibandha"   ,           
                           "Jamalpur.1"    ,          "Gaibandha.1"  ,           "Kurigram")

navbarPage("Hilsa Fish",
           # Interactive map
           tabPanel("About",
                    h1("Introduction")
                    ),
           tabPanel("Interactive Map",
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map", width="100%", height="100%"), 
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Hilsa explorer"),
                                      
                                      radioButtons("types", "Analytical Types:",
                                                   c("Overview" = "overview",
                                                     "Correlation Analysis" = "correlation")),
                                      
                                      checkboxGroupInput('variables', 'Analytical Variables:',
                                                         c("Monsoon", "Non_Monsoon", "Production"), selected = "Monsoon"),

                                      selectInput("location", "Location", Names_NoHuman),
                                      sliderInput("year", "Year", 1970, 2016, value = 1970, step = 1),
                                      selectInput("size", "Size", Names_NoHuman, selected = "adultpop")
                                      
                        )
                    )
                    

           ),
           
           
           tabPanel("Plot Explorer",
                    fluidRow(
                      column(3,
                             wellPanel(
                               sliderInput("year", "Year Range", 1970, 2016, value = c(1970, 2016)),
                               selectInput("analyticsTypes", "Analytical Types", Analytics_Types, selected = "Year vs. Monsoon"), 
                               selectInput("targetLocation_WaterLevel", "Target Location Water Level", Locations, selected = "Amalshid"),
                               selectInput("targetLocation_Production", "Target Location Production", Production_Locations, selected = "Noakhali")
                               )
                             
                      ),
                      mainPanel(
                        plotOutput("plots")
                      )
                    )
                    
                    
#                     checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
#                                        choices = list("Bahadurabad" = "Bahadurabad", "Chilmari" = "Chilmari", "Kanaighat" = "Kanaighat", "Sarighat" = "Sarighat", "Sherpur" = "Sherpur"),
#                                        selected = 1)

              ),
           
           tabPanel("Data Explorer",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput('WaterLevel', 'WaterLevel', Names)
                      ),
                      mainPanel(
                        tableOutput('table')
                      )
                    )
           )

)



