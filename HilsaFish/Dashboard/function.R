CorrelationMap <- function(DataFrame, SelectedName, GeoInfo){
  DATA_COR_MAP <- data.frame(names(DataFrame))
  names(DATA_COR_MAP) <- "Location"
  DATA_COR_MAP$correlation_abs <- NA
  DATA_COR_MAP$p_value <- NA
  for(i in 1:length(DataFrame)){
    DATA_COR_MAP$correlation_abs[i] <- cor(DataFrame[i], DataFrame[SelectedName], use = "complete")
    if(!is.na(DATA_COR_MAP$correlation_abs[i])){
      DATA_COR_MAP$p_value[i] <- cor.test(as.numeric(unlist(DataFrame[i])), as.numeric(unlist(DataFrame[SelectedName])))$p.value
    }
  }
  DATA_COR_MAP$lon <- NA
  DATA_COR_MAP$lat <- NA
  for(i in 1:nrow(DATA_COR_MAP)){
    Index <- which(GeoInfo$Location %in% DATA_COR_MAP$Location[i])
    if(length(Index) != 0){
      DATA_COR_MAP$lon[i] <- GeoInfo$lon[Index]
      DATA_COR_MAP$lat[i] <- GeoInfo$lat[Index]
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
  return(DATA_COR_MAP)
}



PlotMap <- function(DATA_COR_MAP,input, SelectedName, Non_Human = FALSE){
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

PCA_Creation <- function(Data_Input, Non_Human = FALSE){
  if(Non_Human){
    Names_NoHuman <- c("Bahadurabad", "Chilmari", "Kanaighat", "Sarighat", "Sherpur")
    PCA_Input <- Data_Input[Data_Input$Types == "Monsoon",]
    PCA_Input <- PCA_Input[PCA_Input$variable %in% Names_NoHuman, ]
    PCA_Input <- dcast(PCA_Input, Year ~ variable)
    PCA_Input <- PCA_Input[,colSums(is.na(PCA_Input))<(nrow(PCA_Input)-30)]
    PCA_Input <- na.omit(PCA_Input[15:45,][-1])
    return(PCA_Input)
  }
  else{
    PCA_Input <- Data_Input[Data_Input$Types == "Monsoon",]
    PCA_Input <- dcast(PCA_Input, Year ~ variable)
    PCA_Input <- PCA_Input[,colSums(is.na(PCA_Input))<(nrow(PCA_Input)-30)]
    PCA_Input <- na.omit(PCA_Input[15:45,][-1])
    return(PCA_Input)
  }
}

getMostInfluentialVars <- function(similarity_threshold, svd_out, perc_explained){
  vars <- list()
  require("Hmisc")
  for (i in 1:length(perc_explained)){
    v_abs <- abs(svd_out$v[,i])
    maxContributor <- which.max(v_abs)
    similarSizedContributors <- which(v_abs >= v_abs[maxContributor]*similarity_threshold)
    if (any(similarSizedContributors %nin% maxContributor)){
      maxContributor <- similarSizedContributors[order(v_abs[similarSizedContributors], decreasing=TRUE)]
    }
    vars[[length(vars) + 1]] <- maxContributor
  }
  return(vars)
}


GetName <- function(vars, perc_index, varnames){
  unlist(lapply(vars[1:perc_index], FUN=function(x){
    ret <- paste(varnames[x], collapse="\n")
    return(ret)
  }))
}

Plot_SVD <- function(svd_out, perc_index, threhold, plot_threshold){
  cols_expl <- which(cumsum(perc_explained) <= threhold)
  bar_count <- perc_index
  if (bar_count > length(perc_explained)){
    bar_count <- length(perc_explained)
    plot_percent <- perc_explained
  }else{
    plot_percent <- rep(NA, times=bar_count)
    plot_percent <- perc_explained[perc_explained >= plot_threshold]
    plot_percent[bar_count] <- sum(perc_explained[perc_explained < plot_threshold]) 
  }
  
  # Create transition colors
  selected_colors <- colorRampPalette(c("darkgreen", "#FFFFFF"))(bar_count+2)[cols_expl]
  nonselected_colors <- colorRampPalette(c("darkgrey", "#FFFFFF"))(bar_count+2)[(max(cols_expl)+1):bar_count]
  
  names <- unlist(lapply(vars[1:perc_index], FUN=function(x){
    ret <- paste(varnames[x], collapse="\n")
    return(ret)
  }))
  
  rotation <- 45 + (max(unlist(lapply(vars[1:bar_count], 
                                      function(x) {
                                        min(length(x), 5)
                                      })))-1)*(45/5)
  
  p1 <- barchart(plot_percent * 100 ~ 1:bar_count,
                 horiz=FALSE,
                 ylab="Percentage explained (%D)", 
                 main="SVD - the maximum contributors defined by V column",
                 xlab="Pattern contributing variables",
                 col=c(selected_colors, nonselected_colors),
                 key=list(text=list(c("Selected", "Not selected")), 
                          rectangles=list(col=c("darkgreen", "#777777"))),
                 scales=list(x=list(rot=rotation, labels=names)))
  return(p1)
}


Get_SVD <- function(Matrix_In, threhold, similarity_threshold){
  Result <- list()
  SVD <- svd(scale(Matrix_In))
  b_clr <- c("steelblue", "darkred")
  key <- simpleKey(rectangles = TRUE, space = "top", points=FALSE,
                   text=c("Positive", "Negative"))
  key$rectangles$col <- b_clr
  
  perc_explained <- SVD$d^2/sum(SVD$d^2)
  perc_index <- max(which(cumsum(perc_explained) < threhold))
  
  vars <- getMostInfluentialVars(similarity_threshold, SVD, perc_explained)
  Important_Loc <- GetName(vars, perc_index, names(Matrix_In))
  
  Result[[length(Result)+1]] <- vars
  Result[[length(Result)+1]] <- unlist(strsplit(Important_Loc, "\n"))
  #Result[[length(Result)+1]] <- Plot_SVD(SVD, perc_index, threhold, plot_threshold = 0.05)
  
  return(Result)
}


Plot_Exploration <- function(x_variables = "Year", y_variables, DATA){
  Data_Temp <- DATA[DATA$variable %in% y_variables,]
  ggplot(Data_Temp, aes(x = Year, y = value, color = variable)) + geom_point() + 
    ggtitle("Time Series") + geom_smooth(method = "lm") + theme(axis.text.x=element_text(angle=90)) 
  
}



