library(ggplot2)
library(reshape2)
require(gdata)
library(ggmap)
library(ggvis)
library(leaflet)
library(lattice)
source("function.R")

load("data/WaterLevelTimeSeries_No_Human.RData")
load("data/WaterLevelTimeSeries_All.RData")
load("data/Overview_DATA.RData")
load("data/GeoInfo.RData")
load("data/Overview_DATA.RData")


Catchfish_River <- read.xls("data/catch data.xls", sheet = 3)
Catchfish_River$Average <- apply(Catchfish_River[1:6], 1, mean)
Catchfish_River$Total <- apply(Catchfish_River[1:6], 1, sum)
Catchfish_River$Year <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014)


Names_NoHuman <- c("Bahadurabad", "Chilmari", "Kanaighat", "Sarighat", "Sherpur")

CorrelationType <- c("WaterLevel: No_human vs. others",
                     "Bangeladesh_Inland(FAO) & Water_Level locations",
                     "Bangeladesh_Ocean(FAO) & Water_Level locations")

Plot_Explorer_Input <- c("Year", "Monsoon", "Non_Monsoon", "Production")

Locations <- names(WaterLevel_All_Mean_Monsoon)[-1]

Production_Locations <- unique(Overview_DATA[Overview_DATA$Types == "Production",]$variable)

Names <- c("Sherpur")

Names_AllWaterLevel <- names(table(Overview_DATA$variable[Overview_DATA$Types == "Monsoon"]))

PCA_Input <- PCA_Creation(Overview_DATA)
PCA_Locations <- Get_SVD(PCA_Input, 0.8, 0.99)[[2]]
PCA_Input_NoHuman <- PCA_Creation(Overview_DATA, Non_Human = TRUE)
PCA_Locations_NoHuman <- Get_SVD(PCA_Input_NoHuman, 0.95, 0.99)[[2]]

Names_list <- list()
Names_list[[length(Names_list) + 1]] <- c(Names_NoHuman)
Names_list[[length(Names_list) + 1]] <- c("Bangladesh_Inland", "Bangladesh_Ocean")
Names_list[[length(Names_list) + 1]] <- c(PCA_Locations, "Non_Human")
Names_list[[length(Names_list) + 1]] <- c("Bangladesh_Inland", "Bangladesh_Ocean")
names(Names_list)[[1]] <- "WL vs. WL"
names(Names_list)[[2]] <- "Production vs. WL"
names(Names_list)[[3]] <- "WaterLevel vs. PCs"
names(Names_list)[[4]] <- "Production vs. PCs"


# Fish_Production <- read.xls("data/hilsa_FAOdata.xlsx", sheet = 3, header = FALSE)
# names(Fish_Production) <- c("Year", "Production")

# a <- Overview_DATA
# a[(nrow(a)+1):(nrow(a)+31),]$Year <- c(1984:2014)
# a[(nrow(a)-30):nrow(a),]$Types <- rep("Production",31)
# a[(nrow(a)-30):nrow(a),]$variable <- rep("Bangladesh_Ocean",31)
# a[(nrow(a)-30):nrow(a),]$value <- na.omit(as.numeric(as.character(unlist(Fish_Production[6]))))
# Overview_DATA <- a

# Define the variables
# Names_NoHuman <- c("Bahadurabad", "Chilmari", "Kanaighat", "Sarighat", "Sherpur")
# WaterLevel_All_Mean_Monsoon <- dcast(Overview_DATA[Overview_DATA$Types == "Monsoon",], Year ~ variable)
