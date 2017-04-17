library(ggplot2)
library(leaflet)
library(ggvis)
library(dplyr)

navbarPage("Hilsa Fish",
           
           #################
           ##### About #####
           #################
           
            tabPanel("About",
                     navlistPanel(
                       "Table of Content",
                       tabPanel("Introduction",
                                includeMarkdown("Introduction.md")
                       ),
                       tabPanel("PCA Analysis",
                                includeMarkdown("Analysis/PCA/PCA_Analysis.md")
                                ),
                       tabPanel("User Manual",
                                includeMarkdown("Analysis/PCA/PCA_Analysis.md")
                       )
                       )
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
                                                     "Production vs. Principle Components" = "Production vs. PCs")),
                                      
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
                        ),
           
           
                    
                    #                     checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
                    #                                        choices = list("Bahadurabad" = "Bahadurabad", "Chilmari" = "Chilmari", "Kanaighat" = "Kanaighat", "Sarighat" = "Sarighat", "Sherpur" = "Sherpur"),
                    #                                        selected = 1)
                    
           ######################################################################## 
           
           
           #################
           ##### Table #####
           #################
           
           tabPanel("Raw data",
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
