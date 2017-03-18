########################
# Hilsa Fish Project   #
# Main Code            #
# Author: Xiuyang Guan #
########################

###
# 1. SETUP 
###
require(gdata)
library(ggmap)
library(ggplot2)
library(reshape2)
library(plyr)
setwd("~/Google Drive/Xunyang_Fall16Spring17/Hilsa fish research/3.Analysis/RScript")
Input_path <- "~/Google Drive/Xunyang_Fall16Spring17/Hilsa fish research/2.Data"
source("Functions.R")

# Define the important parameters
Names_NoHuman <- c("Bahadurabad", "Chilmari", "Kanaighat", "Sarighat", "Sherpur")

###
# 2. LOAD THE DATA 
###

### Water Level
## Non-Human Impacted
  #################### These archived things are the calculation process ####################

#     a <- read.xls(paste(Input_path, "Tables/1. WL 4-15-1970-10-3-2004, 0_00 hr (whole dataset, 90 station).xls", sep = "/"), sheet = 1, header = TRUE)
#     b <- read.xls(paste(Input_path, "Tables/2. WL10-3-2004 to 10-31-2016, 6 hr (whole dataset, 90 station).xls", sep = "/"), sheet = 1, header = TRUE)
#     b <- b[-1,]
#     WaterLevel_All <- rbind(a,b)
#     remove(a,b)
#     No_Human <- c("Chilmari", "Bahadhurabad", "Sariakandi", "Kaniaghat", "Sherpur-Sylhet") 
#     Names_Temp <- as.character(unlist(WaterLevel_All[1,]))
#     Names_Temp <- sapply(Names_Temp, function(x) strsplit(x, split = ":"))
#     Names_Temp <- lapply(Names_Temp, function(x) x[[1]])
#     Names_Temp <- as.vector(unlist(Names_Temp))
#     Names_Temp[1] <- "Time"
#     names(WaterLevel_All) <- Names_Temp
#     WaterLevel_No_Human <- WaterLevel_All[c(1,7,21,60,94,98)]
#     WaterLevel_No_Human <- WaterLevel_No_Human[-1,]
  # WaterLevel_No_Human <- ToDaily(WaterLevel_No_Human)

load("~/Google Drive/Xunyang_Fall16Spring17/Hilsa fish research/2.Data/CleanData/WaterLevel_No_Human.RData")

# Get the monsoon time series data
Temp <- WaterLevel_No_Human[WaterLevel_No_Human$Moonsoon == TRUE,]
Temp <- Temp[c(2,3,4,5,6,8)]
Temp <- melt(Temp, id = "Year")
a <- ddply(Temp, c("Year", "variable"), summarize, value=mean(value, na.rm = TRUE))
a <- dcast(a, Year ~ variable, value.var = "value")
WaterLevel_No_Human_Mean_Monsoon <- a
# Get the non-monsoon time series data
Temp <- WaterLevel_No_Human[WaterLevel_No_Human$Moonsoon == FALSE,]
Temp <- Temp[c(2,3,4,5,6,8)]
Temp <- melt(Temp, id = "Year")
a <- ddply(Temp, c("Year", "variable"), summarize, value=mean(value, na.rm = TRUE))
a <- dcast(a, Year ~ variable, value.var = "value")
WaterLevel_No_Human_Mean_NonMonsoon <- a


## Human Impacted

# WaterLevel_All <- ToDaily(WaterLevel_All)
load("~/Google Drive/Xunyang_Fall16Spring17/Hilsa fish research/2.Data/CleanData/WaterLevel_All.RData")

# Get the monsoon time series data
Temp <- WaterLevel_All[WaterLevel_All$Moonsoon == TRUE,]
Temp <- Temp[-111]
Temp <- Temp[-1]
Temp <- melt(Temp, id = "Year")
a <- ddply(Temp, c("Year", "variable"), summarize, value=mean(value, na.rm = TRUE))
a <- dcast(a, Year ~ variable, value.var = "value")
WaterLevel_All_Mean_Monsoon <- a
# Get the non-monsoon time series data
Temp <- WaterLevel_All[WaterLevel_All$Moonsoon == FALSE,]
Temp <- Temp[-111]
Temp <- Temp[-1]
Temp <- melt(Temp, id = "Year")
a <- ddply(Temp, c("Year", "variable"), summarize, value=mean(value, na.rm = TRUE))
a <- dcast(a, Year ~ variable, value.var = "value")
WaterLevel_All_Mean_NonMonsoon <- a


### Production
## FAO
FAO <- read.xls(paste(Input_path, "Tables/hilsa_FAOdata.xlsx", sep = "/"), sheet = 3)
FAO$Bangladesh.Inland <- as.numeric(as.character(unlist(FAO$Bangladesh.Inland)))
FAO$Bangladesh.Ocean.Eastern <- as.numeric(as.character(unlist(FAO$Bangladesh.Ocean.Eastern)))
FAO <- FAO[c(1,5,6)]
names(FAO) <- c("Year", "Bangladesh.Inland", "Bangladesh.Ocean.Eastern")

## Catch Data 
# Rivers
Catchfish_River <- read.xls(paste(Input_path, "Tables/catch data.xls", sep = "/"), sheet = 3)
Catchfish_River$Average <- apply(Catchfish_River[1:6], 1, mean)
Catchfish_River$Total <- apply(Catchfish_River[1:6], 1, sum)
Catchfish_River$Year <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014)

# Districts
Catchfish_Districts <- read.xls(paste(Input_path, "Tables/catch data.xls", sep = "/"), sheet = 4)
Catchfish_Districts$Average <- apply(Catchfish_Districts[1:36], 1, mean)
Catchfish_Districts$Total <- apply(Catchfish_Districts[1:36], 1, sum)
Catchfish_Districts$Year <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014)



###
# 3. CORRELATION 
###

### Water Level

## Non-Human Impacted
# Moonsoon
WaterLevel_No_Human_Temp <- WaterLevel_No_Human[WaterLevel_No_Human$Moonsoon == TRUE,]
Matrix_Temp <- as.matrix(WaterLevel_No_Human_Temp[2:6])
COR_Matrix <- cor(Matrix_Temp, use = "complete")
print(COR_Matrix)
# Non-Moonsoon
WaterLevel_No_Human_Temp <- WaterLevel_No_Human[WaterLevel_No_Human$Moonsoon == FALSE,]
Matrix_Temp <- as.matrix(WaterLevel_No_Human_Temp[2:6])
COR_Matrix <- cor(Matrix_Temp, use = "complete")
print(COR_Matrix)

### Production
Catch_River_Matrix <- as.matrix(Catchfish_River[1:10])
Catch_River_COR_Matrix <- cor(Catch_River_Matrix, use = "complete")
write.csv(Catch_River_COR_Matrix, file = "~/Google Drive/Xunyang_Fall16Spring17/Hilsa fish research/3.Analysis/Correlation Matrix/Cor_Matrix_River&FAO.csv")

Catch_District_Matrix <- as.matrix(Catchfish_Districts[1:(ncol(Catchfish_Districts)-1)])
Catch_District_COR_Matrix <- cor(Catch_District_Matrix, use = "complete")
write.csv(Catch_District_COR_Matrix, file = "~/Google Drive/Xunyang_Fall16Spring17/Hilsa fish research/3.Analysis/Correlation Matrix/Cor_Matrix_District&FAO.csv")


###
# 4. PLOT 
###
Catch <- melt(catchfish_Sub, id = "Year")
names(Catch) <- c("Year", "Variables", "Value")
ggplot(Catch) + geom_point(aes(x = Year, y = Value, colour = Variables)) + ylab("Production") + ggtitle("Production data from Catch Data")

Catch_FAOandCatch <- melt(catchfish_Sub[c(7,8,9)], id = "Year")
names(Catch_FAOandCatch) <- c("Year", "Variables", "Value")
ggplot(Catch_FAOandCatch) + geom_point(aes(x = Year, y = Value, colour = Variables)) + ylab("Production") + ggtitle("Production data between FAO and Catch data")

FAO <- melt(FAO, id = "X")
names(FAO) <- c("Year", "Variables", "Value")
ggplot(FAO) + geom_point(aes(x = Year, y = Value, colour = Variables)) + ylab("Production") + ggtitle("Production data from FAO")


##### This code is for the whole data of the water level #####


# Fish Production
Fish_Production <- read.xls("hilsa_FAOdata.xlsx", sheet = 3, header = FALSE)
names(Fish_Production) <- c("Year", "Production")


###### Correlation Matrix ######
Data_Correlation <- Dataframe_After[c(2:6)]
Correlation <- Correlation_Matrix(Data_Correlation)
cor.test(as.numeric(unlist(Data_Correlation[4])), as.numeric(unlist(Data_Correlation[3])))$p.value

##################### Plots #####################
Moonsoon <- ggplot(Water_level_Goalondo_Daily, aes(x = Year, y = WaterLevel, colour = Moonsoon)) + 
  stat_summary(fun.y = "mean", size = 3, geom = "point") + 
  stat_summary(fun.y = "max", size = 3, geom = "point", shape = 1) +
  stat_summary(fun.y = "min", size = 3, geom = "point", shape = 1) +
  ylab("WaterLevel (meter)") + 
  ggtitle("The comparison of water level in Moonsoon")

Water_level_Goalondo_Daily$FourthDay <- FALSE
for(i in 1:nrow(Water_level_Goalondo_Daily)){
  if(i%%4 == 0){
    Water_level_Goalondo_Daily$FourthDay[i] <- TRUE
  }
  print(i)
}

Water_level_Goalondo_Daily_4 <- Water_level_Goalondo_Daily[Water_level_Goalondo_Daily$FourthDay,]
Moonsoon_Week <- ggplot(Water_level_Goalondo_Daily_4, aes(x = Year, y = WaterLevel, colour = Moonsoon)) + 
  stat_summary(fun.y = "mean", size = 3, geom = "point") + 
  stat_summary(fun.y = "max", size = 3, geom = "point", shape = 1) +
  stat_summary(fun.y = "min", size = 3, geom = "point", shape = 1) +
  ylab("WaterLevel (meter)") +
  ggtitle("The comparison of water level in Moonsoon_Week")

Moonsoon_Com <- ggplot(Water_level_Goalondo_Daily, aes(x = Year, y = WaterLevel, colour = Moonsoon)) + 
  stat_summary(fun.y = "mean", size = 3, geom = "point") + 
  stat_summary(fun.y = "max", size = 3, geom = "point", shape = 1) +
  stat_summary(fun.y = "min", size = 3, geom = "point", shape = 1) +
  facet_grid(. ~ FourthDay) + 
  ggtitle("The comparison of water level in Moonsoon_Week")

c$V1 <- as.character(c$V1)
for(i in 1:nrow(c)){
  c$V1[i] <- substr(c$V1[i],1,4)
  print(i)
}
Water_level_Goalondo_Daily$Production <- NA
c$V1 <- as.numeric(c$V1)

for(i in 1:nrow(Water_level_Goalondo_Daily)){
  index <- which(Water_level_Goalondo_Daily$Year[i] == Fish_Production$Year)
  Water_level_Goalondo_Daily$Production[i] <- Fish_Production$Production[index]
  print(i)
}


GGPLOT_WaterLevel <- data.frame()


Water_level_Goalondo_Daily$WaterLevelMean <- 0
Water_level_Goalondo_Daily$WaterLevelMax <- 0
Water_level_Goalondo_Daily$WaterLevelMin <- 0
Water_level_Goalondo_Daily$WaterLevelMean_JJAS <- 0
Water_level_Goalondo_Daily$WaterLevelMax_JJAS <- 0
Water_level_Goalondo_Daily$WaterLevelMin_JJAS <- 0
Water_level_Goalondo_Daily$WaterLevelMean_NonJJAS <- 0
Water_level_Goalondo_Daily$WaterLevelMax_NonJJAS <- 0
Water_level_Goalondo_Daily$WaterLevelMin_NonJJAS <- 0

for(i in 1:nrow(Water_level_Goalondo_Daily)){
  Year_Temp <- Water_level_Goalondo_Daily$Year[i]
  index_temp <- which(Water_level_Goalondo_Daily$Year %in% Year_Temp)
  Data_Temp <- Water_level_Goalondo_Daily[index_temp,]
  WaterLevel_Temp <- as.numeric(as.character(na.omit(unlist(Data_Temp$WaterLevel))))
  Water_level_Goalondo_Daily$WaterLevelMean[index_temp] <- mean(WaterLevel_Temp)
  Water_level_Goalondo_Daily$WaterLevelMax[index_temp] <- max(WaterLevel_Temp)
  Water_level_Goalondo_Daily$WaterLevelMin[index_temp] <- min(WaterLevel_Temp)
  
  Data_Temp <- Water_level_Goalondo_Daily[index_temp,]
  Data_Temp <- Data_Temp[Data_Temp$Moonsoon,]
  WaterLevel_Temp <- as.numeric(as.character(na.omit(unlist(Data_Temp$WaterLevel))))
  Water_level_Goalondo_Daily$WaterLevelMean_JJAS[index_temp] <- mean(WaterLevel_Temp)
  Water_level_Goalondo_Daily$WaterLevelMax_JJAS[index_temp] <- max(WaterLevel_Temp)
  Water_level_Goalondo_Daily$WaterLevelMin_JJAS[index_temp] <- min(WaterLevel_Temp)
  
  Data_Temp <- Water_level_Goalondo_Daily[index_temp,]
  Data_Temp <- Data_Temp[!Data_Temp$Moonsoon,]
  WaterLevel_Temp <- as.numeric(as.character(na.omit(unlist(Data_Temp$WaterLevel))))
  Water_level_Goalondo_Daily$WaterLevelMean_NonJJAS[index_temp] <- mean(WaterLevel_Temp)
  Water_level_Goalondo_Daily$WaterLevelMax_NonJJAS[index_temp] <- max(WaterLevel_Temp)
  Water_level_Goalondo_Daily$WaterLevelMin_NonJJAS[index_temp] <- min(WaterLevel_Temp)
}

GGPLOT_WaterLevel <- reshape(Water_level_Goalondo_Daily, 
             varying = c("WaterLevelMean", "WaterLevelMax", "WaterLevelMin", 
                         "WaterLevelMean_JJAS", "WaterLevelMax_JJAS", "WaterLevelMin_JJAS", 
                         "WaterLevelMean_NonJJAS", "WaterLevelMax_NonJJAS", "WaterLevelMin_NonJJAS"), 
             v.names = "Value",
             timevar = "Items", 
             times = c("WaterLevelMean", "WaterLevelMax", "WaterLevelMin", 
                       "WaterLevelMean_JJAS", "WaterLevelMax_JJAS", "WaterLevelMin_JJAS", 
                       "WaterLevelMean_NonJJAS", "WaterLevelMax_NonJJAS", "WaterLevelMin_NonJJAS"),
             direction = "long")

# WaterLevelMean

LR <- coef(lm(Production ~ WaterLevelMean, data = Water_level_Goalondo_Daily))

# Max
Data_Temp <- GGPLOT_WaterLevel[GGPLOT_WaterLevel$Items == "WaterLevelMax" | GGPLOT_WaterLevel$Items == "WaterLevelMax_JJAS" | GGPLOT_WaterLevel$Items == "WaterLevelMax_NonJJAS",]
jpeg(file = "/Users/qinjingya/Google Drive/Xunyang_Fall16Spring17/Hilsa fish research/3.Analysis/Plots/WaterLevelMax.jpeg", width = 760)
ggplot(Data_Temp) + 
  geom_point(aes(x = Value, y = Production, colour = Items)) + 
  xlab("WaterLevel") + ggtitle("WaterLevelMax vs Production")
dev.off()
# Min
Data_Temp <- GGPLOT_WaterLevel[GGPLOT_WaterLevel$Items == "WaterLevelMin" | GGPLOT_WaterLevel$Items == "WaterLevelMin_JJAS" | GGPLOT_WaterLevel$Items == "WaterLevelMin_NonJJAS",]
jpeg(file = "/Users/qinjingya/Google Drive/Xunyang_Fall16Spring17/Hilsa fish research/3.Analysis/Plots/WaterLevelMin.jpeg", width = 760)
ggplot(Data_Temp) + 
  geom_point(aes(x = Value, y = Production, colour = Items)) + 
  xlab("WaterLevel") + ggtitle("WaterLevelMin vs Production")
dev.off()
# Mean
Data_Temp <- GGPLOT_WaterLevel[GGPLOT_WaterLevel$Items == "WaterLevelMean" | GGPLOT_WaterLevel$Items == "WaterLevelMean_JJAS" | GGPLOT_WaterLevel$Items == "WaterLevelMean_NonJJAS",]
jpeg(file = "/Users/qinjingya/Google Drive/Xunyang_Fall16Spring17/Hilsa fish research/3.Analysis/Plots/WaterLevelMean.jpeg", width = 760)
ggplot(Data_Temp) + 
  geom_point(aes(x = Value, y = Production, colour = Items)) + 
  xlab("WaterLevel") + ggtitle("WaterLevelMean vs Production")
dev.off()

+ 
  geom_smooth(aes(x = WaterLevelMean, y = Production),method = "lm", se = FALSE, color ="black") + 
  geom_point(aes(x = WaterLevelMean, y = Production),aes(color = "black")) + 
  geom_smooth(aes(x = WaterLevelMax, y = Production), method = "lm", se = FALSE, color ="red") + 
  geom_point(aes(x = WaterLevelMax, y = Production), aes(color = "red")) + 
  xlab("WaterLevel Mean (Meters)") + ylab("Production (MT)") + 
  ggtitle("The relationship between WaterLevel and Production")



ggplot(c, aes(x = Production, y = V5)) + 
  stat_summary(fun.y = "sd", size = 1, geom = "point") +
  xlab("Years") + ylab("Water level sd")

# Catch Fish
catchfish <- read.xls("catch data.xls", sheet = 2)
catchfish_temp <- data.frame(t(catchfish), check.rows = TRUE, check.names = FALSE)
names(catchfish_temp) <- as.character(unlist(catchfish_temp[1,]))
catchfish_temp <- catchfish_temp[-1,]
for(i in 1:length(catchfish_temp)){
  catchfish_temp[i] <- as.numeric(as.character(unlist(catchfish_temp[i])))
}
catchfish_temp$Year <- c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014)
ggplot(catchfish_temp) + geom_point(aes(x = Year, y = Rajbari_Up, colour = "red")) + geom_point(aes(x = Year, y = Rajbari_Low))

GGPLOT_DataFrame <- data.frame(catchfish_temp$Year)
names(GGPLOT_DataFrame) <- "Year"
GGPLOT_DataFrame$Up <- catchfish_temp$Rajbari_Up
GGPLOT_DataFrame$Low <- catchfish_temp$Rajbari_Low
GGPLOT_DataFrame$WaterLevelMean <- NA
GGPLOT_DataFrame$WaterLevelMax <- NA
GGPLOT_DataFrame$WaterLevelMin <- NA

for(i in 1:nrow(Water_level_Goalondo_Daily)){
  Year_Temp <- Water_level_Goalondo_Daily$Year[i]
  if(Year_Temp %in% GGPLOT_DataFrame$Year){
    index_temp <- which(Water_level_Goalondo_Daily$Year %in% Year_Temp)
    Data_Temp <- Water_level_Goalondo_Daily[index_temp,]
    WaterLevel_Temp <- as.numeric(as.character(na.omit(unlist(Data_Temp$WaterLevel))))
    GGPLOT_DataFrame[GGPLOT_DataFrame$Year == Year_Temp,]$WaterLevelMean <- mean(WaterLevel_Temp)
    GGPLOT_DataFrame[GGPLOT_DataFrame$Year == Year_Temp,]$WaterLevelMax <- max(WaterLevel_Temp)
    GGPLOT_DataFrame[GGPLOT_DataFrame$Year == Year_Temp,]$WaterLevelMin <- min(WaterLevel_Temp)
    }
}

ggplot(GGPLOT_DataFrame) + geom_point(aes(x = WaterLevelMean, y = Up, colour = "red")) + geom_point(aes(x = WaterLevelMean, y = Low))
ggplot(GGPLOT_DataFrame) + geom_point(aes(x = WaterLevelMean, y = Up, colour = "red")) + geom_point(aes(x = WaterLevelMax, y = Low))
ggplot(GGPLOT_DataFrame) + geom_point(aes(x = WaterLevelMean, y = Up, colour = "red")) + geom_point(aes(x = WaterLevelMin, y = Low))

GGPLOT_DataFrame$WaterLevelMean_JJAG <- NA
GGPLOT_DataFrame$WaterLevelMax_JJAG <- NA
GGPLOT_DataFrame$WaterLevelMin_JJAG <- NA

for(i in 1:nrow(Water_level_Goalondo_Daily)){
  Year_Temp <- Water_level_Goalondo_Daily$Year[i]
  if(Year_Temp %in% GGPLOT_DataFrame$Year){
    index_temp <- which(Water_level_Goalondo_Daily$Year %in% Year_Temp)
    Data_Temp <- Water_level_Goalondo_Daily[index_temp,]
    Data_Temp <- Data_Temp[Data_Temp$Moonsoon,]
    WaterLevel_Temp <- as.numeric(as.character(na.omit(unlist(Data_Temp$WaterLevel))))
    GGPLOT_DataFrame[GGPLOT_DataFrame$Year == Year_Temp,]$WaterLevelMean_JJAG <- mean(WaterLevel_Temp)
    GGPLOT_DataFrame[GGPLOT_DataFrame$Year == Year_Temp,]$WaterLevelMax_JJAG <- max(WaterLevel_Temp)
    GGPLOT_DataFrame[GGPLOT_DataFrame$Year == Year_Temp,]$WaterLevelMin_JJAG <- min(WaterLevel_Temp)
  }
}


m <- as.matrix(catchfish_temp)
Cor_M <- cor(m)
Names <- names(catchfish_temp)
for(i in 1:nrow(Cor_M)){
  for(j in 1:ncol(Cor_M)){
    if(Cor_M[i,][j] > 0.9 & Cor_M[i,][j] < 1){
      print(paste(Names[i], "and", Names[j]))
    }
  }
}

for(i in 1:nrow(Cor_M)){
  for(j in 1:ncol(Cor_M)){
    if(Cor_M[i,][j] < -0.6){
      print(paste(Names[i], "and", Names[j]))
    }
  }
}


##### Map #####
raw_data <- read.xls(paste(Input_path, "Research Data.xlsx", sep = "/"), sheet = 1, header = TRUE)
names(raw_data) <- c("Location", "lon", "lat")
map <- get_map(location = 'Bangladesh', zoom = 7)
mapPoints <- ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = 1.5, color = "red"), data = raw_data)
mapPoints


##### For the Shiny #####
SelectedName <- "Sherpur"
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
DATA_COR_MAP$correlation_abs <- abs(DATA_COR_MAP$correlation_abs)

mapPoints <- ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = correlation_abs, color = PorN), data = DATA_COR_MAP) + 
  scale_size(range = c(0, 10))
mapPoints
