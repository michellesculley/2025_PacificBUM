## Data exploration for BUM 2026 assessment


## load data
logbook<-read.csv('BUM_CPUEwSST.csv')
df<-df_sst_pdoi_soi_oni


library(ggplot2)
library(mgcv)
library(maps)
library(maptools)
library(reshape2)
library(plyr)
library(gridExtra)
library(lme4)
library(labeling)
library(emmeans)
library(statmod)
library(lattice)
library(arm)





BUMCPUE <- data.frame("Year" = df[, "HAUL_YEAR"], "Month" = df[, "HAUL_MONTH"], "Day" = df[, "HAUL_DAY"], "Bait" = df[, "BAIT_CODE"], "BeginSetTime" = df[, "BEGIN_SET_TIME"], "HPF" = df[, "HOOKS_PER_FLOAT"], "Lat" = df[, "LAT"], "Set" = df[, "SET_TYPE"], "CPUE" = df[, "BUM_CPUE"], "Vessel" = df[, "PERMIT_NUMBER"], "Lon" = df[, "LON"] * -1, "SST" = df[, "SSTDEGC"], "PDO" = df[, "PDO_INDEX"], "SOI" = df[, "Value"],"ONI"=df[,"ANOM"], "Target" = df[, "TARGET_SPECIES_CODE"], "Hooks" = df[, "NUMBER_OF_HOOKS_SET"], Catch = df[, "BLUE_MARLIN_NUMBER_OF_FISH_KEPT"])
BUMCPUE <- subset(BUMCPUE, !is.na(HPF) & !is.na(Bait))
BUMCPUE$Lat1 <- ceiling(BUMCPUE$Lat)
BUMCPUE$Lon1 <- ceiling(BUMCPUE$Lon)
BUMCPUE$Lat5 <- (ceiling(BUMCPUE$Lat / 5) * 5) - 2.5
BUMCPUE$Lon5 <- (ceiling(BUMCPUE$Lon / 5) * 5) - 2.5

Q1 <- which(BUMCPUE$Month >= 1 & BUMCPUE$Month <= 3)
Q2 <- which(BUMCPUE$Month >= 4 & BUMCPUE$Month <= 6)
Q3 <- which(BUMCPUE$Month >= 7 & BUMCPUE$Month <= 9)
BUMCPUE$Quarter <- 4
BUMCPUE[Q1, "Quarter"] <- 1
BUMCPUE[Q2, "Quarter"] <- 2
BUMCPUE[Q3, "Quarter"] <- 3

BUMCPUE$Begin <- ifelse(BUMCPUE$BeginSetTime >= 0 & BUMCPUE$BeginSetTime <= 600, 1,
    ifelse(BUMCPUE$BeginSetTime > 600 & BUMCPUE$BeginSetTime <= 1200, 2,
        ifelse(BUMCPUE$BeginSetTime > 1200 & BUMCPUE$BeginSetTime <= 1800, 3, 4)
    )
)

BUMCPUE$SetType <- ifelse(BUMCPUE$Year < 2004 & BUMCPUE$HPF <= 10, "S",
    ifelse(BUMCPUE$Year >= 2004 & BUMCPUE$HPF <= 14, "S", "D")
)

BUMCPUE <- BUMCPUE[which(BUMCPUE$CPUE <= 60), ]
BUMCPUE$Target <- trimws(BUMCPUE$Target)
BUMCPUE$Target <- ifelse(BUMCPUE$Target == "B", "B", ifelse(BUMCPUE$Target == "T", "T", "M"))
BUMCPUE$Target <- as.factor(BUMCPUE$Target)


BUMCPUE <- BUMCPUE[complete.cases(BUMCPUE[, c("Year", "Quarter", "HPF", "Bait", "Begin", "SST", "Lat", "Lon", "PDO", "SOI", "Vessel")]), ]


BUMCPUE$PropPos <- ifelse(BUMCPUE$CPUE > 0, 1, 0)


BUMPos <- subset(BUMCPUE, PropPos == 1)



RawCPUESet <- aggregate(BUMCPUE$CPUE, by = list(BUMCPUE$Year, BUMCPUE$SetType), mean)
RawCPUE <- aggregate(BUMCPUE$CPUE, by = list(BUMCPUE$Year), mean)
names(RawCPUESet) <- c("Year", "Set", "MeanCPUE")
names(RawCPUE) <- c("Year", "MeanCPUE")


