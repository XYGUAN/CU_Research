##### This RScript includes the server in the Shiny application #####

##### Preparation #####
# load the packages
library(ggplot2)
library(reshape2)
require(gdata)
library(ggmap)
library(leaflet)

# load the data
raw_data <- read.xls("data/Locations.xlsx", sheet = 1, header = TRUE)
names(raw_data) <- c("Location", "lon", "lat")
load("data/WaterLevelTimeSeries_No_Human.RData")
load("data/WaterLevelTimeSeries_All.RData")
load("data/Overview_DATA.RData")

Catchfish_River <- read.xls("data/catch data.xls", sheet = 3)
Catchfish_River$Average <- apply(Catchfish_River[1:6], 1, mean)
Catchfish_River$Total <- apply(Catchfish_River[1:6], 1, sum)
Catchfish_River$Year <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014)
Names_NoHuman <- c("Bahadurabad", "Chilmari", "Kanaighat", "Sarighat", "Sherpur")
DATA <- cbind(Catchfish_River, WaterLevel_No_Human_Mean_Monsoon[32:45,2:6])


function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 90.4125, lat = 23.8103, zoom = 7)
  })
  
  observe({
    # Overview
    if(input$types == "overview"){
      Target_Variabels <- input$variables
      Data_Temp <- Overview_DATA[Overview_DATA$Types %in% Target_Variabels,]
      Selected_Year <- input$year
      Data_Temp <- Data_Temp[Data_Temp$Year == Selected_Year,]
      Data_Temp <- Data_Temp[-1]
      DATA_COR_MAP <- Data_Temp
      DATA_COR_MAP$lon <- NA
      DATA_COR_MAP$lat <- NA
      for(i in 1:nrow(DATA_COR_MAP)){
        Index <- which(raw_data$Location %in% DATA_COR_MAP$variable[i])
        if(length(Index) != 0){
          DATA_COR_MAP$lon[i] <- raw_data$lon[Index]
          DATA_COR_MAP$lat[i] <- raw_data$lat[Index]
        }
      }
      DATA_COR_MAP <- na.omit(DATA_COR_MAP)
      DATA_COR_MAP$value[DATA_COR_MAP$Types == "Production"] <- DATA_COR_MAP$value[DATA_COR_MAP$Types == "Production"] *10
      DATA_COR_MAP$value[DATA_COR_MAP$Types != "Production"] <- DATA_COR_MAP$value[DATA_COR_MAP$Types != "Production"] *500
      value <- DATA_COR_MAP$value
      colorData <- as.factor(DATA_COR_MAP$Types)
      pal <- colorFactor("Spectral", colorData)
      leafletProxy("map", data = DATA_COR_MAP) %>%
        clearShapes() %>% 
        addCircles(~lon, ~lat, radius=value,
                   stroke=FALSE, fillOpacity=0.4, fillColor = pal(colorData)) %>%
        addLegend("bottomleft", pal=pal, values=colorData, title="Types",
                  layerId="colorLegend")
      
    }
    
      # Correlation Analysis
      if(input$types == "correlation"){
        SelectedName <- input$location
        ID <- which(Names_NoHuman %in% SelectedName)
        Data_Temp <- WaterLevel_All_Mean_Monsoon[-which(names(WaterLevel_All_Mean_Monsoon) %in% Names_NoHuman)[-ID]]
        Data_Temp <- Data_Temp[-1]
        Data_Temp <- Data_Temp[,colSums(is.na(Data_Temp))<(nrow(Data_Temp)-10)]
        DATA_COR_MAP <- data.frame(names(Data_Temp))
        names(DATA_COR_MAP) <- "Location"
        DATA_COR_MAP$correlation_abs <- NA
        DATA_COR_MAP$p_value <- NA
        for(i in 1:length(Data_Temp)){
          DATA_COR_MAP$correlation_abs[i] <- cor(Data_Temp[i], Data_Temp[SelectedName], use = "complete")
          if(!is.na(DATA_COR_MAP$correlation_abs[i])){
            DATA_COR_MAP$p_value[i] <- cor.test(as.numeric(unlist(Data_Temp[i])), as.numeric(unlist(Data_Temp[SelectedName])))$p.value
          }
        }
        DATA_COR_MAP$lon <- NA
        DATA_COR_MAP$lat <- NA
        for(i in 1:nrow(DATA_COR_MAP)){
          Index <- which(raw_data$Location %in% DATA_COR_MAP$Location[i])
          if(length(Index) != 0){
            DATA_COR_MAP$lon[i] <- raw_data$lon[Index]
            DATA_COR_MAP$lat[i] <- raw_data$lat[Index]
          }
        }
        DATA_COR_MAP <- na.omit(DATA_COR_MAP)
        DATA_COR_MAP$COR <- NA
        for(i in 1:nrow(DATA_COR_MAP)){
          if(DATA_COR_MAP$correlation_abs[i]<0 & DATA_COR_MAP$p_value[i] < 0.01){
            DATA_COR_MAP$COR[i] <- "Negative and significant"
          }
          if(DATA_COR_MAP$correlation_abs[i]>0 & DATA_COR_MAP$p_value[i] < 0.01){
            DATA_COR_MAP$COR[i] <- "Positive and significant"
          }
          if(DATA_COR_MAP$correlation_abs[i]<0 & DATA_COR_MAP$p_value[i] > 0.01){
            DATA_COR_MAP$COR[i] <- "Negative and insignificant"
          }
          if(DATA_COR_MAP$correlation_abs[i]>0 & DATA_COR_MAP$p_value[i] > 0.01){
            DATA_COR_MAP$COR[i] <- "Positive and insignificant"
          }
          if(DATA_COR_MAP$Location[i] == SelectedName){
            DATA_COR_MAP$COR[i] <- SelectedName
          }
        }
        DATA_COR_MAP$correlation_abs <- abs(DATA_COR_MAP$correlation_abs)
        
        correlation_abs <- DATA_COR_MAP$correlation_abs * 15000
        colorData <- factor(DATA_COR_MAP$COR, levels = c("Positive and significant", "Positive and insignificant", "Negative and significant", "Negative and insignificant", SelectedName))
        pal <- colorFactor(c("blue", "green", "red", "orange", "black"), colorData)
        
        leafletProxy("map", data = DATA_COR_MAP) %>%
          clearShapes() %>% 
          addCircles(~lon, ~lat, radius=correlation_abs,
                     stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData), layerId = DATA_COR_MAP$Location) %>%
          addLegend("bottomleft", pal=pal, values=colorData, title="Types",
                    layerId="colorLegend")
        
        
        # Show a popup at the given location
        showCorrelationPopup <- function(id, lat, lng) {
          Correlation <- round(DATA_COR_MAP$correlation_abs[DATA_COR_MAP$Location == id],5)
          p_value <- round(DATA_COR_MAP$p_value[DATA_COR_MAP$Location == id],5)
          content <- as.character(tagList(
            tags$strong("Location:", id), tags$br(),
            sprintf("Correlation is: %s", Correlation), tags$br(),
            sprintf("p_value is: %s", p_value)
          ))
          leafletProxy("map") %>% addPopups(lng, lat, content)
        }
        
        # When map is clicked, show a popup with city info
        observe({
          leafletProxy("map") %>% clearPopups()
          event <- input$map_shape_click
          if (is.null(event))
            return()
          
          isolate({
            showCorrelationPopup(event$id, event$lat, event$lng)
          })
        })
      }
      
  }
  
  )


  
  output$plots <- renderPlot({
    xvar_name <- input$plot_x_input
    yvar_name <- input$plot_y_input
    Target_Waterlevel <- input$targetLocation_WaterLevel
    Target_Production <- input$targetLocation_Production
    
    if(xvar_name == "Year"){
      DATA_PLOT <- Overview_DATA[Overview_DATA$Types == yvar_name,]
      if(yvar_name == "Production" & xvar_name == "Year"){
        DATA_PLOT <- DATA_PLOT[DATA_PLOT$variable == input$targetLocation_Production,]
        DATA_PLOT <- DATA_PLOT[!is.na(DATA_PLOT$value),]
        DATA_PLOT$Year <- as.numeric(DATA_PLOT$Year)
        PLOT <- ggplot(DATA_PLOT, aes(x = Year, y = value)) + geom_point() + 
          geom_smooth(method = "lm") +
          xlab(xvar_name) + ylab(yvar_name) + 
          ggtitle(paste("The relationship between", yvar_name, "and", xvar_name, "in", input$targetLocation_Production))
      }else{
        DATA_PLOT <- DATA_PLOT[DATA_PLOT$variable == input$targetLocation_WaterLevel,]
        DATA_PLOT <- DATA_PLOT[!is.na(DATA_PLOT$value),]
        DATA_PLOT$Year <- as.numeric(DATA_PLOT$Year)
        PLOT <- ggplot(DATA_PLOT, aes(x = Year, y = value)) + geom_point() +
          geom_smooth(method = "lm") + 
          xlab(xvar_name) + ylab(yvar_name) + 
          ggtitle(paste("The relationship between", yvar_name, "and", xvar_name, "in", input$targetLocation_WaterLevel))
        
      }
      PLOT
      # if(input$Plot_Options == "Abline"){
      #   PLOT + geom_smooth()
      # }else{
      #   PLOT
      # }
    }
    
    if(xvar_name == "Monsoon" & yvar_name == "Production"){
      DATA_PLOT_X <- Overview_DATA[Overview_DATA$Types == xvar_name,]
      DATA_PLOT_Y <- Overview_DATA[Overview_DATA$Types == yvar_name,]
      DATA_PLOT_X <- DATA_PLOT_X[DATA_PLOT_X$variable == input$targetLocation_WaterLevel,]
      DATA_PLOT_Y <- DATA_PLOT_Y[DATA_PLOT_Y$variable == input$targetLocation_Production,]
      DATA_PLOT <- data.frame(DATA_PLOT_Y$Year, DATA_PLOT_Y$value)
      names(DATA_PLOT) <- c("Year", "Production")
      DATA_PLOT$WL <- NA
      for(i in 1:nrow(DATA_PLOT)){
        DATA_PLOT$WL[i] <- DATA_PLOT_X$value[which(DATA_PLOT_X$Year %in% DATA_PLOT$Year[i])]
      }
      PLOT <- ggplot(DATA_PLOT, aes(x = WL, y = Production)) + geom_point() + 
          geom_smooth(method = "lm") +
          xlab(xvar_name) + ylab(yvar_name) + 
          ggtitle(paste("The relationship between", yvar_name, "and", xvar_name, "in", input$targetLocation_Production))
      PLOT
      }
    PLOT
    })

  
  # Filter data based on selections
  output$table <- renderTable({
    Matrix <- cbind(Catchfish_River[1:8], WaterLevel_No_Human_Mean_Monsoon[32:45,input$WaterLevel])
    names(Matrix)[ncol(Matrix)] <- input$WaterLevel
    Cor <- cor(Matrix, use = "complete")
    TABLE <- data.frame(names(Matrix), Cor[9,])
    TABLE$P_Value <- 0
    for(i in 1:nrow(TABLE)){
      TABLE$P_Value[i] <- cor.test(as.numeric(unlist(Matrix[i])), as.numeric(unlist(Matrix[9])))$p.value
    }
    names(TABLE) <- c("River", "Correlation", "p_value")
    TABLE
  })
  

}
  