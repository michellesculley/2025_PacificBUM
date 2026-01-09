library(ggplot2)
library(mgcv)
library(maps)
library(reshape2)
library(plyr)
library(gridExtra)
library(lme4)
library(labeling)
library(emmeans)
library(statmod)
library(lattice)
library(arm)
library(car)
library(pdp)
library(tidyverse)
library(gt)
library(viridis)



## Code to pull catch data for billfish assessments from LLDS summary tables
source("01_LoadData.R")
 
 logbook <- read.csv("C:/Users/Michelle.Sculley/Documents/2025_PacificBUM/HI CPUE/BUM_CPUEnoSST.csv")
 df <- logbook
BUMCPUE <- data.frame(
    "Year" = df[, "HAUL_YEAR"], "Month" = df[, "HAUL_MONTH"],
    "Day" = df[, "HAUL_DAY"], "Bait" = df[, "BAIT_CODE"], "BeginSetTime" = df[, "BEGIN_SET_TIME"],
    "HPF" = df[, "HOOKS_PER_FLOAT"], "Lat" = df[, "LATITUDE"], "Set" = df[, "SET_TYPE"],
    "CPUE" = df[, "BUM_CPUE"], "Vessel" = df[, "PERMIT_NUMBER"], "Lon" = df[, "LONGITUDE"] * -1,
    "Target" = df[, "TARGET_SPECIES_CODE"], "Hooks" = df[, "NUMBER_OF_HOOKS_SET"], Catch = df[, "BLUE_MARLIN_NUMBER_OF_FISH_KEPT"]
)

## remove missing data
BUMCPUE <- subset(BUMCPUE, !is.na(HPF) & !is.na(Bait))

## set up 1x1 and 5x5 block assignments
BUMCPUE$Lat1 <- ceiling(BUMCPUE$Lat)
BUMCPUE$Lon1 <- ceiling(BUMCPUE$Lon)
BUMCPUE$Lat5 <- (ceiling(BUMCPUE$Lat / 5) * 5) - 2.5
BUMCPUE$Lon5 <- (ceiling(BUMCPUE$Lon / 5) * 5) - 2.5

## set months as quarters
Q1 <- which(BUMCPUE$Month >= 1 & BUMCPUE$Month <= 3)
Q2 <- which(BUMCPUE$Month >= 4 & BUMCPUE$Month <= 6)
Q3 <- which(BUMCPUE$Month >= 7 & BUMCPUE$Month <= 9)
BUMCPUE$Quarter <- 4
BUMCPUE[Q1, "Quarter"] <- 1
BUMCPUE[Q2, "Quarter"] <- 2
BUMCPUE[Q3, "Quarter"] <- 3

## set Begin Set Time as quarter of day
BUMCPUE$Begin <- ifelse(BUMCPUE$BeginSetTime >= 0 & BUMCPUE$BeginSetTime <= 600, 1,
    ifelse(BUMCPUE$BeginSetTime > 600 & BUMCPUE$BeginSetTime <= 1200, 2,
        ifelse(BUMCPUE$BeginSetTime > 1200 & BUMCPUE$BeginSetTime <= 1800, 3, 4)
    )
)

## define deep and shallow sets based on HPF
BUMCPUE$SetType <- ifelse(BUMCPUE$Year < 2004 & BUMCPUE$HPF <= 10, "S",
    ifelse(BUMCPUE$Year >= 2004 & BUMCPUE$HPF <= 14, "S", "D")
)

# ## set fisher defined target as Tuna or Billfish sets
BUMCPUE <- BUMCPUE[which(BUMCPUE$CPUE <= 60), ]
BUMCPUE$Target <- trimws(BUMCPUE$Target)
BUMCPUE$Target <- ifelse(BUMCPUE$Target == "B", "B", ifelse(BUMCPUE$Target == "T", "T", "M"))
BUMCPUE$Target <- as.factor(BUMCPUE$Target)
 BUMCPUE <- subset(BUMCPUE, Hooks >= 100)
 BUMCPUE <- BUMCPUE[complete.cases(BUMCPUE[, c("Year", "Quarter", "HPF", "Bait", "Begin", "Lat", "Lon", "Vessel")]), ]


 ## set up positive catches dataframe
 BUMCPUE$PropPos <- ifelse(BUMCPUE$CPUE > 0, 1, 0)
 BUMCPUE <- subset(BUMCPUE, CPUE < 20)
 BUMPos <- subset(BUMCPUE, PropPos == 1)

 # Identify the confidential Information
 # for plotting annual catch
 BUMUnique5 <- unique(BUMCPUE[, c("Year", "Lat5", "Lon5", "Vessel")])
 UniqueCount5 <- plyr::count(BUMUnique5, vars = c("Year", "Lat5", "Lon5"))
 BUMCPUE_Map <- merge(BUMCPUE, UniqueCount5, by = c("Year", "Lat5", "Lon5"))
 BUMCPUE_Map$Include5 <- ifelse(BUMCPUE_Map$freq < 3, 0, 1)
 BUMMapping5 <- subset(BUMCPUE_Map, Include5 == 1 & Catch > 0)



 BUMUnique5Q <- unique(BUMCPUE[, c("Quarter", "Lat5", "Lon5", "Vessel")])
 UniqueCount5 <- plyr::count(BUMUnique5Q, vars = c("Quarter", "Lat5", "Lon5"))
 BUMCPUE_MapQ <- merge(BUMCPUE, UniqueCount5, by = c("Quarter", "Lat5", "Lon5"))
 BUMCPUE_MapQ$Include5 <- ifelse(BUMCPUE_MapQ$freq < 3, 0, 1)
 BUMMapping5Q <- subset(BUMCPUE_MapQ, Include5 == 1 & Catch > 0)



## BUM
# HI Longline quarterly catch in numbers
HI_QCatch <- HI_stats %>%
            filter(REPT_PERIOD == "Q") %>%
            group_by(YEAR, PERIOD_NUM) %>%
            summarise(
                C_BLUE_MARLIN = sum(C_BLUE_MARLIN, na.rm = TRUE)/1000
                ) %>%
                ungroup() %>%
            select(YEAR, PERIOD_NUM, C_BLUE_MARLIN)

#American Samoa longline quarterly catch in numbers
AS_QCatch <- AS_stats %>%
    filter(REPT_PERIOD == "Q") %>%
    group_by(YEAR, PERIOD_NUM) %>%
    summarise(
        C_BLUE_MARLIN = sum(C_BLUE_MARLIN, na.rm = TRUE)/1000
    ) %>%
    ungroup() %>%
    select(YEAR, PERIOD_NUM, C_BLUE_MARLIN)

## Small boat/other gears, catch in biomass
HI_Other<-HI_all_gears %>%
        filter(FAO_CODE=="BUM") %>% 
        mutate( C_BLUE_MARLIN = (PEL_LBS_KEPT - LL_ALL_LBS_KEPT)*0.000453592) %>%
        select(YEAR, C_BLUE_MARLIN)


## Annual catch by gear

#sum by year
HI_CatchYr<-aggregate(HI_QCatch$C_BLUE_MARLIN, by=list(HI_QCatch$YEAR), sum)
names(HI_CatchYr)=c("YEAR","C_BLUE_MARLIN")
HI_CatchYr<-subset(HI_CatchYr, YEAR<2025)
AS_CatchYr <- aggregate(AS_QCatch$C_BLUE_MARLIN, by = list(AS_QCatch$YEAR), sum)
names(AS_CatchYr) <- c("YEAR", "C_BLUE_MARLIN")
AS_CatchYr <- subset(AS_CatchYr, YEAR < 2025)


AnnualCatch<-bind_rows(
    "US HI Longline" = HI_CatchYr,
    "US AS Longline" = AS_CatchYr,
    "US Other Gears" = HI_Other,
    .id="Source"
)

## plot

## LL catch in numbers
AnnualLLCatch<-ggplot() +
    geom_col(aes(x=YEAR, y=C_BLUE_MARLIN, fill=Source), data = subset(AnnualCatch, Source!="US Other Gears")) +
    theme_bw() +
    scale_fill_manual(values=c("US AS Longline" = "mediumblue", "US HI Longline" = "grey70")) +
    scale_x_continuous(limits=c(1992, 2025), breaks=seq(1995,2025,5)) +
    scale_y_continuous(name="Thousands of Fish Caught")





## Mapping code



HI_CatchMapYR <- aggregate(BUMMapping5$Catch,by=list(BUMMapping5$Year, BUMMapping5$Lon5, BUMMapping5$Lat5), sum)
names(HI_CatchMapYR)<-c("Year","Lon5","Lat5","Catch")
HI_CatchMapQTR <- aggregate(BUMMapping5Q$Catch, by = list(BUMMapping5Q$Quarter, BUMMapping5Q$Lon5, BUMMapping5Q$Lat5), sum)
names(HI_CatchMapQTR) <- c("Quarter", "Lon5", "Lat5", "Catch")


HICatch_5yr<-HI_CatchMapYR

HICatch_5yr$YrGroup<-1
Group2 <- which(HICatch_5yr$Year >= 1995 & HICatch_5yr$Year < 2000)
Group3 <- which(HICatch_5yr$Year >= 2000 & HICatch_5yr$Year < 2005)
Group4 <- which(HICatch_5yr$Year >= 2005 & HICatch_5yr$Year < 2010)
Group5 <- which(HICatch_5yr$Year >= 2010 & HICatch_5yr$Year < 2015)
Group6 <- which(HICatch_5yr$Year >= 2015 & HICatch_5yr$Year < 2020)


HICatch_5yr$YrGroup[Group2]<-2
HICatch_5yr$YrGroup[Group3] <- 3
HICatch_5yr$YrGroup[Group4] <- 4
HICatch_5yr$YrGroup[Group5] <- 5
HICatch_5yr$YrGroup[Group6] <- 6


HICatch_5yr<-aggregate(Catch ~ YrGroup + Lat5 + Lon5, HICatch_5yr, sum)


YearLabels<-c("1" = "1995-1999", "2" = "2000 - 2004","3"="2005 - 2009","4"="2010 - 2014","5"="2015 - 2019","6"="2020 - 2024")
world_df <- map_data("world")
CatchMapYear<-ggplot() +
    # World map polygons
    geom_polygon(
        data = world_df,
        aes(x = long, y = lat, group = group),
        fill = "grey80", color = "gray50"
    ) +

    # Plotting the effort data as points with binned sizes
    geom_point(
        data = HICatch_5yr,
        aes(x = Lon5, y = Lat5, size = Catch/1000),
        color = "red", alpha = 0.7
    ) +

    # Using discrete size scale for binned data
 #   scale_size_manual(
 #       values = seq(2, 6, length.out = n_bins), # Size range from 2 to 6
 #       labels = bin_labels,
 #       name = "Effort Range"
 #   ) +

    # Formatting X and Y axis labels with degrees
    scale_x_continuous(labels = function(x) paste0(abs(x), "°W")) +
    scale_y_continuous(labels = function(y) paste0(y, "°N")) +

    # Coordinate adjustments for map area
    coord_fixed(
        ratio = 1.3,
        xlim = c(-180, -100), # Adjust longitude range for Hawaii
        ylim = c(0, 40) # Adjust latitude range for Hawaii
    ) +

    # Customizing the legend
    guides(size = guide_legend(
        title = "Thousands of Fish Caught",
        override.aes = list(color = "red") # Ensure legend symbols are red
    )) +

    # Adding plot title and theme
    theme_minimal() +

    # Customizing the overall plot theme
    theme(
        legend.position = "bottom",
        legend.background = element_rect(fill = alpha("white", 0.7), color = "black", linewidth = 0.5),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin = margin(20, 20, 20, 20),
        panel.grid.major = element_line(color = "gray70"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.background = element_rect(fill = "white", color = NA) # Set white background
    ) + 
    facet_wrap(~YrGroup, ncol=2, labeller = labeller(YrGroup=YearLabels)) 


CatchMapQTR <- ggplot() +
    # World map polygons
    geom_polygon(
        data = world_df,
        aes(x = long, y = lat, group = group),
        fill = "grey80", color = "gray50"
    ) +

    # Plotting the effort data as points with binned sizes
    geom_point(
        data = HI_CatchMapQTR,
        aes(x = Lon5, y = Lat5, size = Catch/1000),
        color = "red", alpha = 0.7
    ) +

    # Using discrete size scale for binned data
    #   scale_size_manual(
    #       values = seq(2, 6, length.out = n_bins), # Size range from 2 to 6
    #       labels = bin_labels,
    #       name = "Effort Range"
    #   ) +

    # Formatting X and Y axis labels with degrees
    scale_x_continuous(labels = function(x) paste0(abs(x), "°W")) +
    scale_y_continuous(labels = function(y) paste0(y, "°N")) +

    # Coordinate adjustments for map area
    coord_fixed(
        ratio = 1.3,
        xlim = c(-180, -100), # Adjust longitude range for Hawaii
        ylim = c(0, 40) # Adjust latitude range for Hawaii
    ) +

    # Customizing the legend
    guides(size = guide_legend(
        title = "Thousands of Fish Caught",
        override.aes = list(color = "red") # Ensure legend symbols are red
    )) +

    # Adding plot title and theme
    theme_minimal() +

    # Customizing the overall plot theme
    theme(
         legend.position = "bottom",
        legend.background = element_rect(fill = alpha("white", 0.7), color = "black", linewidth = 0.5),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        plot.title = element_text(size = 14, face = "bold"),
        plot.margin = margin(20, 20, 20, 20),
        panel.grid.major = element_line(color = "gray70"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.background = element_rect(fill = "white", color = NA) # Set white background
    ) +
    facet_wrap(~Quarter)




