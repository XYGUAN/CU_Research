library(ggplot2)
library(leaflet)
library(ggvis)
library(dplyr)

navbarPage("Hilsa Fish",
           
           #################
           ##### About #####
           #################
           
            tabPanel("About",
                      includeMarkdown("Analysis/Introduction.md")
                       # tabPanel("PCA Analysis",
                       #          includeMarkdown("Analysis/PCA/PCA_Analysis.md")
                       #          )
                       ),
           
           ###############
           ##### Map #####
           ###############
           
           tabPanel("Interactive Map",
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map", width="100%", height="100%"),

                        absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                                      draggable = TRUE, top = 10, left = "auto", right = 10, bottom = "auto",
                                      width = 300, height = "100%", cursor = "move",

                                      h3("Correlational Analysis"),

                                      radioButtons("CorrelationType", "Correlational Types:",
                                                   c("WaterLevel: No_human vs. others" = "WL vs. WL",
                                                     "Production vs. WaterLevel" = "Production vs. WL",
                                                     "WaterLevel vs. Principle Components" = "WaterLevel vs. PCs", 
                                                     "Production vs. Principle Components" = "Production vs. PCs",
                                                     "Non_Human PCs vs. All PCs" = "PCs vs. PCs")),
                                      
                                      selectInput("locations", "Target Locations", c(Names_NoHuman)),
                                      # selectInput("location_PR", "Target Production Location", c("Bangladesh_Inland", "Bangladesh_Ocean")),
                                      # selectInput("location_PCA", "Target PCA Location", c(PCA_Locations, "Non_Human")),
                                      
                                      tags$body(paste(
                                        "Note: Please based on the chosen type to choose the Target variables",
                                        "E.X. If you choose Production vs. WaterLevel, then change location_PR",
                                        "Otherwise it doesn't work"
                                      ))
                    )
                    ) 
                    
           ),
           
           ################
           ##### Plot #####
           ################
           
           tabPanel("Plot Explorer",
                    fluidRow(
                      column(3,
                             wellPanel(
                               sliderInput("year", "Year Range", 1970, 2016, value = c(1970, 2016)),
                               radioButtons("Plot_Analytics_Types","Analytical Types:",
                                            c("Time-Series Analytics" = "plot_TimeSeries",
                                              "Variables Analytics" = "plot_VariableAnalytics")),
                               selectInput("plot_x_input", "x_axis", "Year", selected = "Year"),
                               selectInput("plot_y_input", "y_axis", Plot_Explorer_Input, selected = "Monsoon"),
                               selectInput("targetLocation_WaterLevel", "Target Location Water Level", Locations, multiple = TRUE, selectize = TRUE),
                               selectInput("targetLocation_Production", "Target Location Production", Production_Locations, multiple = TRUE, selectize = TRUE)
                             )
                             
                      ),
                      mainPanel(
                        plotOutput("plots")
                      )
                    )
                        ),
           
           
                    
                    #                     checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
                    #                                        choices = list("Bahadurabad" = "Bahadurabad", "Chilmari" = "Chilmari", "Kanaighat" = "Kanaighat", "Sarighat" = "Sarighat", "Sherpur" = "Sherpur"),
                    #                                        selected = 1)
                    
           ######################################################################## 
           
           
           #################
           ##### Table #####
           #################
           
           tabPanel("Data Explorer",
                    sidebarLayout(
                      sidebarPanel(

                        #boxes to choose from monsoon, non-monsoon, and production
                        checkboxGroupInput('type', 'Raw Data', names(table(Overview_DATA$Type)), selected = 'Monsoon'),
                      
                        #selectInput for all water level stations (for all variable attributes) : for monsoon and non monsoon
                        selectInput('name', 'Water Level Stations:',choices = c("All", names(table(Overview_DATA$variable[0:10250])))  ,  selected = "All", multiple = TRUE, selectize = TRUE),
                        
                        #selectInput for all production stations
                        selectInput('names', 'Production Stations:', choices = c("All", names(table(Overview_DATA$variable[10250:10781])))  ,  selected = NULL, multiple = TRUE, selectize = TRUE)
                        #leave option for all names
                        #first is actual variable that will be used in serve , second is shown to user, third is options
                      ),
                      mainPanel(
                        #adds tabs to the main panel to choose between geo info or overview data
                        tabsetPanel(
                          tabPanel("Raw Data", tableOutput('table')),
                          tabPanel("Geological Data", tableOutput('table2'))
                          
                          #trying to get search button to work for years, but was crashing program
                          #tabPanel(selectizeInput('year', 'Search for year',  names(table(Overview_DATA$Year)), selected = NULL, multiple = FALSE,
                          # options = list(
                          # placeholder = 'Please select an option below',
                          # onInitialize = I('function() { this.setValue(""); }')
                          # )))
                        )
                      )
                    ) 
           
)

)

