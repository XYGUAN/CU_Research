##### This RScript includes the server in the Shiny application #####


#######################
##### Preparation #####
#######################
# load the packages
library(ggplot2)
library(reshape2)
require(gdata)
library(ggmap)
library(leaflet)

source("function.R")

####################
##### Function #####
####################

function(input, output, session) {
  
  ###############
  ##### Map #####
  ###############
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 91.4125, lat = 23.8103, zoom = 7)
  })

  observe({
  a <- input$CorrelationType
  
  updateSelectInput(session, "locations",
                    label = "Select input label",
                    choices = c(Names_list[[a]])
  )
  })

  observe({

     if(input$CorrelationType == "WL vs. WL"){
        SelectedName <- input$locations
        if(SelectedName %in% Names_list[["WL vs. WL"]]){
        ID <- which(Names_NoHuman %in% SelectedName)
        Data_Temp <- WaterLevel_All_Mean_Monsoon[-which(Names_AllWaterLevel %in% Names_NoHuman)[-ID]]
        Data_Temp <- Data_Temp[-1]
        Data_Temp <- Data_Temp[,colSums(is.na(Data_Temp))<(nrow(Data_Temp)-10)]
        
        DATA_COR_MAP <- CorrelationMap(Data_Temp, SelectedName, GeoInfo)
        PlotMap(DATA_COR_MAP,input,SelectedName)
        }
     }

    if(input$CorrelationType == "Production vs. WL"){ 
      
      # Production and WaterLevel
        SelectedName <- input$locations
        if(SelectedName %in% Names_list[["Production vs. WL"]]){
        Data_Temp <- dcast(Overview_DATA[Overview_DATA$Types == "Monsoon" | Overview_DATA$variable == SelectedName,], Year ~ variable)
        Data_Temp <- Data_Temp[-1]
        Data_Temp <- Data_Temp[,colSums(is.na(Data_Temp))<(nrow(Data_Temp)-10)]
        
        DATA_COR_MAP <- CorrelationMap(Data_Temp, SelectedName, GeoInfo)
        PlotMap(DATA_COR_MAP,input,SelectedName)
        }
        
    }
    
    
    if(input$CorrelationType == "WaterLevel vs. PCs"){ 
      SelectedName <- input$locations
      if(SelectedName %in% Names_list[["WaterLevel vs. PCs"]]){
        if(SelectedName == "Non_Human"){
            SelectedName <- PCA_Locations_NoHuman
          }
      Data_Temp <- dcast(Overview_DATA[Overview_DATA$Types == "Monsoon", ], Year ~ variable)
      Data_Temp <- Data_Temp[-1]
      Data_Temp <- Data_Temp[,colSums(is.na(Data_Temp))<(nrow(Data_Temp)-10)]
      DATA_COR_MAP <- CorrelationMap(Data_Temp, SelectedName, GeoInfo)
      PlotMap(DATA_COR_MAP,input,SelectedName)
      }
    }
    
    if(input$CorrelationType == "Production vs. PCs"){ 
      # Production and WaterLevel
      SelectedName <- input$locations
      if(SelectedName %in% Names_list[["Production vs. PCs"]]){
      Data_Temp <- Overview_DATA[Overview_DATA$Types == "Monsoon" | Overview_DATA$variable == SelectedName, ]
      Data_Temp <- Data_Temp[Data_Temp$variable %in% c(PCA_Locations, PCA_Locations_NoHuman, SelectedName), ]
      Data_Temp <- dcast(Data_Temp, Year ~ variable)
      Data_Temp <- Data_Temp[-1]
      Data_Temp <- Data_Temp[,colSums(is.na(Data_Temp))<(nrow(Data_Temp)-10)]
      DATA_COR_MAP <- CorrelationMap(Data_Temp, SelectedName, GeoInfo)
      PlotMap(DATA_COR_MAP,input,SelectedName)
      }
      }
      
    
})

  
  
  #################
  ##### Plots #####
  #################
  
  ################ Original Plot ################
  observe({
  b <- input$Plot_Analytics_Types
  
  updateSelectInput(session, "plot_x_input",
                    label = "x_axis",
                    choices = c(Names_list_Plot[[b]]))
  })
                  
  output$plots <- renderPlot({
    ################################################################################ 
    # if(xvar_name == "Year"){
    #   DATA_PLOT <- Overview_DATA[Overview_DATA$Types == yvar_name,]
    #   if(yvar_name == "Production" & xvar_name == "Year"){
    #     DATA_PLOT <- DATA_PLOT[DATA_PLOT$variable == input$targetLocation_Production,]
    #     DATA_PLOT <- DATA_PLOT[!is.na(DATA_PLOT$value),]
    #     DATA_PLOT$Year <- as.numeric(DATA_PLOT$Year)
    #     PLOT <- ggplot(DATA_PLOT, aes(x = Year, y = value)) + geom_point() + 
    #       geom_smooth(method = "lm") +
    #       xlab(xvar_name) + ylab(yvar_name) + 
    #       ggtitle(paste("The relationship between", yvar_name, "and", xvar_name, "in", input$targetLocation_Production))
    #   }else{
    #     DATA_PLOT <- DATA_PLOT[DATA_PLOT$variable == input$targetLocation_WaterLevel,]
    #     DATA_PLOT <- DATA_PLOT[!is.na(DATA_PLOT$value),]
    #     DATA_PLOT$Year <- as.numeric(DATA_PLOT$Year)
    #     PLOT <- ggplot(DATA_PLOT, aes(x = Year, y = value)) + geom_point() +
    #       geom_smooth(method = "lm") + 
    #       xlab(xvar_name) + ylab(yvar_name) + 
    #       ggtitle(paste("The relationship between", yvar_name, "and", xvar_name, "in", input$targetLocation_WaterLevel))
    #     
    #   }
    #   PLOT
    # }
    # 
    # if(xvar_name == "Monsoon" & yvar_name == "Production"){
    #   DATA_PLOT_X <- Overview_DATA[Overview_DATA$Types == xvar_name,]
    #   DATA_PLOT_Y <- Overview_DATA[Overview_DATA$Types == yvar_name,]
    #   DATA_PLOT_X <- DATA_PLOT_X[DATA_PLOT_X$variable == input$targetLocation_WaterLevel,]
    #   DATA_PLOT_Y <- DATA_PLOT_Y[DATA_PLOT_Y$variable == input$targetLocation_Production,]
    #   DATA_PLOT <- data.frame(DATA_PLOT_Y$Year, DATA_PLOT_Y$value)
    #   names(DATA_PLOT) <- c("Year", "Production")
    #   DATA_PLOT$WL <- NA
    #   for(i in 1:nrow(DATA_PLOT)){
    #     DATA_PLOT$WL[i] <- DATA_PLOT_X$value[which(DATA_PLOT_X$Year %in% DATA_PLOT$Year[i])]
    #   }
    #   PLOT <- ggplot(DATA_PLOT, aes(x = WL, y = Production)) + geom_point() + 
    #     geom_smooth(method = "lm") +
    #     xlab(xvar_name) + ylab(yvar_name) + 
    #     ggtitle(paste("The relationship between", yvar_name, "and", xvar_name, "in", input$targetLocation_Production))
    #   PLOT
    # }
    # PLOT
    ################################################################################ 
    xvar_name <- input$plot_x_input
    yvar_name <- input$plot_y_input
    Target_Waterlevel <- input$targetLocation_WaterLevel
    Target_Production <- input$targetLocation_Production
    Year_low <- input$year[1]
    Year_high <- input$year[2]
    Data_Temp <- Overview_DATA[Overview_DATA$Year >= Year_low & Overview_DATA$Year <= Year_high, ]
    if(yvar_name == "Monsoon"){
      Plot_Exploration(x_variables = "Year", y_variables = Target_Waterlevel, DATA = Data_Temp[Data_Temp$Types == "Monsoon",])
    }else if(yvar_name == "Non_Monsoon"){
        Plot_Exploration(x_variables = "Year", y_variables = Target_Waterlevel, DATA = Data_Temp[Data_Temp$Types == "Non_Monsoon",])
    }else{
      Plot_Exploration(x_variables = "Year", y_variables = Target_Production, DATA = Data_Temp)
    }
    
  })

  
  
  
  
  ################################################################

      
      
      
    
    
    # if(xvar_name == "Monsoon" & yvar_name == "Production"){
    #   DATA_PLOT_X <- Overview_DATA[Overview_DATA$Types == xvar_name,]
    #   DATA_PLOT_Y <- Overview_DATA[Overview_DATA$Types == yvar_name,]
    #   DATA_PLOT_X <- DATA_PLOT_X[DATA_PLOT_X$variable == input$targetLocation_WaterLevel,]
    #   DATA_PLOT_Y <- DATA_PLOT_Y[DATA_PLOT_Y$variable == input$targetLocation_Production,]
    #   DATA_PLOT <- data.frame(DATA_PLOT_Y$Year, DATA_PLOT_Y$value)
    #   names(DATA_PLOT) <- c("Year", "Production")
    #   DATA_PLOT$WL <- NA
    #   for(i in 1:nrow(DATA_PLOT)){
    #     DATA_PLOT$WL[i] <- DATA_PLOT_X$value[which(DATA_PLOT_X$Year %in% DATA_PLOT$Year[i])]
    #   }
    #   PLOT <- ggplot(DATA_PLOT, aes(x = WL, y = Production)) + geom_point() + 
    #       geom_smooth(method = "lm") +
    #       xlab(xvar_name) + ylab(yvar_name) + 
    #       ggtitle(paste("The relationship between", yvar_name, "and", xvar_name, "in", input$targetLocation_Production))
    #   PLOT
    #   }
    # PLOT
    # })

  
  #################
  ##### table #####
  #################
  #creating table for the overview data
  output$table <- renderTable  ({
    # TABLE
    #table is updated every time the boxes are checked
    Table <- Overview_DATA[Overview_DATA$Type == input$type,]
    
    #if production is chose
    if (input$type == 'Production' || is.null(input$type)){
      
      #if initial input for the names is all, just show the table
      if (input$names =="All") {Table}
   
      #if not, show the location user is looking for
      else {Table[Table$variable == input$names,]}
    }
    
    #if he chose monsoon or non-monsoon, do the same thing
    else{
      
      if (input$name == "All") {Table}
      else { Table[Table$variable == input$name,]}
   
    }

  })
  
  
  #second table for the geological data
  output$table2 <- renderTable  ({
    # TABLE
    #to add other options, change name each time and only in the end show the table
    Table3 <- GeoInfo
    Table3
  })
}
