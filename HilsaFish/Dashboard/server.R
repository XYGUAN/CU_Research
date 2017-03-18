# Load the ggplot2 package which provides
# the 'mpg' dataset.
library(ggplot2)
library(reshape2)
require(gdata)
library(ggmap)
library(leaflet)
Input_path <- "~/Google Drive/Xunyang_Fall16Spring17/Hilsa fish research/2.Data"
load("~/Google Drive/Xunyang_Fall16Spring17/Hilsa fish research/3.Analysis/RScript/shinyapp/data/WaterLevelTimeSeries_No_Human.RData")
load("~/Google Drive/Xunyang_Fall16Spring17/Hilsa fish research/3.Analysis/RScript/shinyapp/data/WaterLevelTimeSeries_All.RData")
load("~/Google Drive/Xunyang_Fall16Spring17/Hilsa fish research/3.Analysis/RScript/shinyapp/data/Overview_DATA.RData")

Catchfish_River <- read.xls(paste(Input_path, "Tables/catch data.xls", sep = "/"), sheet = 3)
Catchfish_River$Average <- apply(Catchfish_River[1:6], 1, mean)
Catchfish_River$Total <- apply(Catchfish_River[1:6], 1, sum)
Catchfish_River$Year <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014)
raw_data <- read.xls(paste(Input_path, "Research Data.xlsx", sep = "/"), sheet = 1, header = TRUE)
names(raw_data) <- c("Location", "lon", "lat")
Names_NoHuman <- c("Bahadurabad", "Chilmari", "Kanaighat", "Sarighat", "Sherpur")

DATA <- cbind(Catchfish_River, WaterLevel_No_Human_Mean_Monsoon[32:45,2:6])

y_axis_FUN <- function(types){
  switch(types,
         "Year vs. Monsoon" = "Monsoon",
         "Year vs. NonMonsoon" = "NonMonsoon",
         "Year vs. Production" = "Production",
         "Monsoon vs. Production" = "Production",
         "Monsoon vs. NonMonsoon" = "NonMonsoon"
  )
}
x_axis_FUN <- function(types){
  switch(types,
         "Year vs. Monsoon" = "Year",
         "Year vs. NonMonsoon" = "Year",
         "Year vs. Production" = "Year",
         "Monsoon vs. Production" = "Monsoon",
         "Monsoon vs. NonMonsoon" = "Monsoon"
  )
}


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
        Data_Temp <- Data_Temp[,colSums(is.na(Data_Temp))<(nrow(Data_Temp)-5)]
        DATA_COR_MAP <- data.frame(names(Data_Temp))
        names(DATA_COR_MAP) <- "Location"
        DATA_COR_MAP$correlation_abs <- NA
        for(i in 1:length(Data_Temp)){
          DATA_COR_MAP$correlation_abs[i] <- cor(Data_Temp[i], Data_Temp[SelectedName], use = "complete")
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
        DATA_COR_MAP$PorN <- ifelse(DATA_COR_MAP$correlation_abs > 0, 1,-1)
        DATA_COR_MAP$PorN[DATA_COR_MAP$Location == SelectedName] <- 0
        DATA_COR_MAP$correlation_abs <- abs(DATA_COR_MAP$correlation_abs)
        DATA_COR_MAP <- na.omit(DATA_COR_MAP)
        correlation_abs <- DATA_COR_MAP$correlation_abs * 10000
        colorData <- DATA_COR_MAP$PorN
        pal <- colorFactor("Spectral", colorData)
        
        
        leafletProxy("map", data = DATA_COR_MAP) %>%
          clearShapes() %>% 
          addCircles(~lon, ~lat, radius=correlation_abs,
                     stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
          addLegend("bottomleft", pal=pal, values=colorData, title="Types",
                    layerId="colorLegend")
        
      }
      
  }
  
  )
  
  # Show a popup at the given location
  showZipcodePopup <- function(lat, lng) {
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
    
    
    content <- paste("Location:", as.character(lat))
    
    leafletProxy("map") %>% addPopups(lng, lat, content)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showZipcodePopup(event$lat, event$lng)
    })
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
  
  output$plots <- renderPlot({
    xvar_name <- x_axis_FUN(input$analyticsTypes)
    yvar_name <- y_axis_FUN(input$analyticsTypes)
    Target_Waterlevel <- input$targetLocation_WaterLevel
    Target_Production <- input$targetLocation_Production
    
    if(xvar_name != "Year" & yvar_name != "Year"){
      DATA_X <- Overview_DATA[Overview_DATA$Types == xvar_name,]
      names(DATA_X)[3] <- xvar_name
      DATA_X <- DATA_X[DATA_X$variable == Target_Waterlevel,]
      DATA_Y <- Overview_DATA[Overview_DATA$Types == yvar_name,]
      names(DATA_Y)[3] <- yvar_name
      DATA_Y <- DATA_Y[DATA_Y$variable == Target_Production,]
      DATA_PLOT <- DATA_Y
      DATA_PLOT$X <- NA
      for(i in 1:nrow(DATA_PLOT)){
        DATA_PLOT$X[i] <- DATA_X[,3][which(DATA_X$Year %in% DATA_PLOT$Year[i])]
      }
      names(DATA_PLOT)[5] <- xvar_name
      ggplot(DATA_PLOT) + geom_point(aes(x = Monsoon, y = Production)) + 
        xlab(xvar_name) + ylab(yvar_name) + ggtitle(paste(input$analyticsTypes, "in", input$targetLocation_WaterLevel))
      
    }else if(xvar_name == "Year"){
      DATA_PLOT <- Overview_DATA[Overview_DATA$Types == yvar_name,]
      DATA_PLOT <- DATA_PLOT[DATA_PLOT$variable == input$targetLocation_WaterLevel,]
      ggplot(DATA_PLOT) + geom_point(aes(x = Year, y = value)) + 
        ggtitle(paste(input$analyticsTypes, "in", input$targetLocation_WaterLevel)) + 
        ylab(yvar_name)
    }
  })
  
    # Lables for axes
    

    # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
    # but since the inputs are strings, we need to do a little more work.
  
#   output$maps <- renderPlot({
#     SelectedName <- input$Location
#     ID <- which(Names_NoHuman %in% SelectedName)
#     Data_Temp <- WaterLevel_All_Mean_Monsoon[-which(names(WaterLevel_All_Mean_Monsoon) %in% Names_NoHuman)[-ID]]
#     Data_Temp <- Data_Temp[-1]
#     Data_Temp <- Data_Temp[,colSums(is.na(Data_Temp))<(nrow(Data_Temp)-5)]
#     DATA_COR_MAP <- data.frame(names(Data_Temp))
#     names(DATA_COR_MAP) <- "Location"
#     DATA_COR_MAP$correlation_abs <- NA
#     for(i in 1:length(Data_Temp)){
#       DATA_COR_MAP$correlation_abs[i] <- cor(Data_Temp[i], Data_Temp[SelectedName], use = "complete")
#     }
#     DATA_COR_MAP$lon <- NA
#     DATA_COR_MAP$lat <- NA
#     for(i in 1:nrow(DATA_COR_MAP)){
#       Index <- which(raw_data$Location %in% DATA_COR_MAP$Location[i])
#       if(length(Index) != 0){
#         DATA_COR_MAP$lon[i] <- raw_data$lon[Index]
#         DATA_COR_MAP$lat[i] <- raw_data$lat[Index]
#       }
#     }
#     DATA_COR_MAP$PorN <- ifelse(DATA_COR_MAP$correlation_abs > 0, 1,-1)
#     DATA_COR_MAP$correlation_abs <- abs(DATA_COR_MAP$correlation_abs)
#     map <- get_map(location = 'Bangladesh', zoom = 7)
#     mapPoints <- ggmap(map) +
#       geom_point(aes(x = lon, y = lat, size = correlation_abs, color = PorN), data = DATA_COR_MAP) + 
#       scale_size(range = c(0, 10))
#     mapPoints
#     
#   })
#   
}
  
