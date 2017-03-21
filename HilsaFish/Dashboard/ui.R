library(ggplot2)
library(leaflet)
library(ggvis)
library(dplyr)
load("data/WaterLevelTimeSeries_No_Human.RData")
load("data/WaterLevelTimeSeries_All.RData")
load("data/Overview_DATA.RData")

Names <- names(WaterLevel_No_Human_Mean_Monsoon)[2:6]
Names_NoHuman <- c("Bahadurabad", "Chilmari", "Kanaighat", "Sarighat", "Sherpur")

Plot_Explorer_Input <- c("Year", "Monsoon", "Non_Monsoon", "Production")

Locations <- names(WaterLevel_All_Mean_Monsoon)[-1]

Production_Locations <- unique(Overview_DATA[Overview_DATA$Types == "Production",]$variable)
  
navbarPage("Hilsa Fish",
           # Interactive map
#            tabPanel("About",
#                     h1("Introduction")
#            ),
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
                               checkboxGroupInput('Plot_Options', 'Options',
                                                  c("Abline")),
                               selectInput("plot_x_input", "x_axis", Plot_Explorer_Input, selected = "Year"),
                               selectInput("plot_y_input", "y_axis", Plot_Explorer_Input, selected = "Monsoon"),
                               selectInput("targetLocation_WaterLevel", "Target Location Water Level", Locations, selected = "Amalshid"),
                               selectInput("targetLocation_Production", "Target Location Production", Production_Locations, selected = "Bangladesh_Inland")
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
