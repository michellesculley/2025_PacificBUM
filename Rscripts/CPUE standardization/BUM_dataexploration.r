## Data exploration for BUM 2026 assessment


# load data
 logbook<-read.csv('C:/Users/Michelle.Sculley/Documents/2025_PacificBUM/HI CPUE/BUM_CPUEnoSST.csv')
 df<-logbook

logbook <- read.csv("C:/Users/Michelle.Sculley/Documents/2025_PacificBUM/HI CPUE/BUM_CPUEwSST.csv")

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

## load completed results to save time when rendering pdf
load("C:/Users/Michelle.Sculley/Documents/2025_PacificBUM/HICPUEresults.RData")

#including enviro
BUMCPUE2 <- data.frame("Year" = logbook[, "HAUL_YEAR"], "Month" = logbook[, "HAUL_MONTH"], 
"Day" = logbook[, "HAUL_DAY"], "Bait" = logbook[, "BAIT_CODE"], "BeginSetTime" = logbook[, "BEGIN_SET_TIME"], 
"HPF" = logbook[, "HOOKS_PER_FLOAT"], "Lat" = logbook[, "LAT"], "Set" = logbook[, "SET_TYPE"], 
"CPUE" = logbook[, "BUM_CPUE"], "Vessel" = logbook[, "PERMIT_NUMBER"], "Lon" = logbook[, "LON"] * -1, 
"SST" = logbook[, "SSTDEGC"], "PDO" = logbook[, "PDO_INDEX"], "SOI" = logbook[, "Value"],"ONI"=logbook[,"ANOM"], 
"Target" = logbook[, "TARGET_SPECIES_CODE"], "Hooks" = logbook[, "NUMBER_OF_HOOKS_SET"], Catch = logbook[, "BLUE_MARLIN_NUMBER_OF_FISH_KEPT"])


# # without enviro
BUMCPUE <- data.frame("Year" = df[, "HAUL_YEAR"], "Month" = df[, "HAUL_MONTH"], 
"Day" = df[, "HAUL_DAY"], "Bait" = df[, "BAIT_CODE"], "BeginSetTime" = df[, "BEGIN_SET_TIME"], 
"HPF" = df[, "HOOKS_PER_FLOAT"], "Lat" = df[, "LATITUDE"], "Set" = df[, "SET_TYPE"], 
"CPUE" = df[, "BUM_CPUE"], "Vessel" = df[, "PERMIT_NUMBER"], "Lon" = df[, "LONGITUDE"] * -1, 
"Target" = df[, "TARGET_SPECIES_CODE"], "Hooks" = df[, "NUMBER_OF_HOOKS_SET"], Catch = df[, "BLUE_MARLIN_NUMBER_OF_FISH_KEPT"])

##remove missing data
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


## filter out sets with <100 hooks set
BUMCPUE2<-subset(BUMCPUE2, Hooks>=100)
BUMCPUE2<-subset(BUMCPUE2, CPUE<10)
 BUMCPUE<-subset(BUMCPUE, Hooks>=100)

# ## remove missing data
# #with enviro
# # BUMCPUE <- BUMCPUE[complete.cases(BUMCPUE[, c("Year", "Quarter", "HPF", "Bait", "Begin", "SST", "Lat", "Lon", "PDO", "SOI", "Vessel")]), ]
# # w/0 enviro
 BUMCPUE <- BUMCPUE[complete.cases(BUMCPUE[, c("Year", "Quarter", "HPF", "Bait", "Begin", "Lat", "Lon", "Vessel")]), ]


## set up positive catches dataframe
 BUMCPUE$PropPos <- ifelse(BUMCPUE$CPUE > 0, 1, 0)
BUMCPUE<-subset(BUMCPUE, CPUE<20)
  BUMPos <- subset(BUMCPUE, PropPos == 1)

# Identify the confidential Information
# for plotting annual catch
BUMUnique5 <- unique(BUMCPUE[, c("Year", "Lat5", "Lon5", "Vessel")])
UniqueCount5 <- plyr::count(BUMUnique5, vars = c("Year", "Lat5", "Lon5"))
BUMCPUE_Map <- merge(BUMCPUE, UniqueCount5, by = c("Year", "Lat5", "Lon5"))
BUMCPUE_Map$Include5 <- ifelse(BUMCPUE_Map$freq < 3, 0, 1)
BUMMapping5 <- subset(BUMCPUE_Map, Include5 == 1)


BUMUnique5Q <- unique(BUMCPUE[, c("Quarter", "Lat5", "Lon5", "Vessel")])
UniqueCount5 <- plyr::count(BUMUnique5Q, vars = c("Quarter", "Lat5", "Lon5"))
BUMCPUE_MapQ <- merge(BUMCPUE, UniqueCount5, by = c("Quarter", "Lat5", "Lon5"))
BUMCPUE_MapQ$Include5 <- ifelse(BUMCPUE_MapQ$freq < 3, 0, 1)
BUMMapping5Q <- subset(BUMCPUE_MapQ, Include5 == 1)



# ## Final CPUE models


#  BUM_PropPos<-glm(PropPos ~ Lat + as.factor(Year) + as.factor(Month) + Lon + as.factor(Bait) + as.factor(HPF) + as.factor(Begin), data = BUMCPUE, family = binomial(link = "logit"))

#  BUM_Pos <- lmer(log(CPUE) ~ as.factor(Bait) + as.factor(Begin) + as.factor(Year) + as.factor(HPF) + (1 | Vessel), data = BUMPos, REML = FALSE)

# ## deepset only doesn't improve residuals
# # DeepOnly<-subset(BUMCPUE,SetType=="D")
# # DeepPos<-subset(DeepOnly, PropPos==1)
# # Deep_PropPos <- glm(PropPos ~ Lat + as.factor(Year) + as.factor(Month) + Lon + as.factor(Bait) + as.factor(HPF) + as.factor(Begin), data = DeepOnly, family = binomial(link = "logit"))

# # Deep_Pos <- lmer(log(CPUE) ~ as.factor(Bait) + as.factor(Begin) + as.factor(Year) + as.factor(HPF) + (1 | Vessel), data = DeepPos, REML = FALSE)


# ## calculate standardized CPUE value: 
# emm_options(rg.limit=300000)
# lsmean.CPUEPos <- emmeans(BUM_Pos, ~Year)
# Final.Pos<-regrid(lsmean.CPUEPos, transform="response")

# emm_options(rg.limit=4500000)
# lsmean.CPUEprop<-summary(ref_grid(BUM_PropPos))
# lsmean.CPUEprop$TranCPUE<-1/(1+exp(-lsmean.CPUEprop$prediction))
# predicted.proppos <- aggregate(lsmean.CPUEprop$TranCPUE, by = list(lsmean.CPUEprop$Year), mean)
# variance.proppos <- aggregate(lsmean.CPUEprop$TranCPUE, by = list(lsmean.CPUEprop$Year), var)

# StandCPUE <- data.frame("Year" = predicted.proppos$Group.1, "CPUE" = Final.Pos@bhat * predicted.proppos$x)

# goodman.se <- function(p, var_p, c, var_c) {
#     (var_p * c^2 + var_c * p^2 - var_p * var_c)^.5
# }
# StandCPUE$SE <- goodman.se(Final.Pos@bhat, (sqrt(diag(Final.Pos@V))^2), predicted.proppos$x, variance.proppos$x)

# StandCPUE$UL<-StandCPUE$CPUE+1.96*StandCPUE$SE
# StandCPUE$LL <- StandCPUE$CPUE - 1.96 * StandCPUE$SE


# ## save to Rdata file so I don't have to run it again
# save(BUM_PropPos, BUM_Pos, Final.Pos,predicted.proppos, variance.proppos, StandCPUE, BUMCPUE, BUMPos, file="HICPUEresults.RData")




## nominal CPUE calculation
RawCPUESet <- aggregate(BUMCPUE$CPUE, by = list(BUMCPUE$Year, BUMCPUE$SetType), mean)
RawCPUE <- aggregate(BUMCPUE$CPUE, by = list(BUMCPUE$Year), mean)
names(RawCPUESet) <- c("Year", "Set", "MeanCPUE")
names(RawCPUE) <- c("Year", "MeanCPUE")

#plot nominal by set
NominalBySet<-ggplot()+
geom_point(aes(x=Year, y=MeanCPUE, color=Set), data=RawCPUESet)+
geom_line(aes(x = Year, y = MeanCPUE, color = Set), data = RawCPUESet) +
theme_bw()

# mean(BUMCPUE$PropPos)
# nrow(BUMCPUE)
# Final.Pos
# sum(BUMCPUE$Catch)

## Plot the catch on a map



# ## plot standardized vs nominal

# # plot nominal by set

# ggplot() +
#     geom_point(aes(x = Year, y = MeanCPUE), data = RawCPUE, color="blue") +
#     geom_line(aes(x = Year, y = MeanCPUE), data = RawCPUE, color="blue") +
    
#     theme_bw()


# # plot nominal vs standardized with uncertainty

CPUEPlot<- ggplot() +
     geom_point(aes(x = Year, y = MeanCPUE), data = RawCPUE, color = "blue") +
     geom_line(aes(x = Year, y = MeanCPUE), data = RawCPUE, color = "blue") +
         geom_point(aes(x = Year, y = CPUE), data = StandCPUE, color = "black") +
     geom_line(aes(x = Year, y = CPUE), data = StandCPUE, color = "black") +
 geom_ribbon(aes(x = Year, ymin = ifelse(LL<0,0,LL), ymax=CPUE), data = StandCPUE, color = "black", alpha=0.05) +
 geom_ribbon(aes(x = Year, ymax = UL, ymin=CPUE), data = StandCPUE, color = "black", alpha=0.05) +
     theme_bw()


# ### nominal CPUE vs covariate
## filter out 
# ## all Data
SSTPlot<-ggplot()+
     geom_point(aes(y=CPUE,x=SST),data=BUMCPUE2)+
     geom_smooth(aes(y=CPUE,x=SST),data=BUMCPUE2, method="gam") + theme_bw()

SOIPlot<-ggplot()+
     geom_point(aes(y=CPUE,x=SOI),data=BUMCPUE2)+
     geom_smooth(aes(y=CPUE,x=SOI),data=BUMCPUE2, method="gam") + theme_bw()

PDOPlot<-ggplot()+
     geom_point(aes(y=CPUE,x=PDO),data=BUMCPUE2)+
     geom_smooth(aes(y=CPUE,x=PDO),data=BUMCPUE2, method="gam") + theme_bw()

ONIPlot<-ggplot()+
     geom_point(aes(y=CPUE,x=ONI),data=BUMCPUE2)+
     geom_smooth(aes(y=CPUE,x=ONI),data=BUMCPUE2, method="gam") + theme_bw()


LatPlot<-ggplot()+
     geom_point(aes(y=CPUE,x=Lat),data=subset(BUMCPUE, CPUE<10))+
     geom_smooth(aes(y=CPUE,x=Lat),data=BUMCPUE, method="gam") + theme_bw()

LonPlot<-ggplot()+
     geom_point(aes(y=CPUE,x=Lon),data=subset(BUMCPUE, CPUE<10))+
     geom_smooth(aes(y=CPUE,x=Lon),data=BUMCPUE, method="gam") + theme_bw()


BeginSetTimePlot<-ggplot()+
     geom_point(aes(y=CPUE,x=BeginSetTime),data=BUMCPUE)+
     geom_smooth(aes(y=CPUE,x=BeginSetTime),data=BUMCPUE, method="gam") +
     theme_bw()

BeginPlot<-ggplot()+
     geom_boxplot(aes(y=CPUE,group=Begin, x=Begin),data=subset(BUMCPUE, CPUE<5))+
     theme_bw()

BaitPlot<-ggplot()+
     geom_boxplot(aes(y=CPUE,group=as.factor(Bait), x=as.factor(Bait)),data=subset(BUMCPUE, CPUE<5))+
     xlab("Bait Type")  +
     theme_bw() + theme(axis.text.x=element_text(size=6, angle = 45, vjust = 1, hjust = 1)
  )

HPFPlot<-ggplot()+
     geom_boxplot(aes(y=CPUE,group=as.factor(HPF), x=as.factor(HPF)),data=subset(BUMCPUE, CPUE<5))+
     xlab("Hooks Per Float")  +
     theme_bw() + theme(axis.text.x = element_text(size = 6, angle = 45, vjust = 1, hjust = 1))



# ## diagnostic plots
# par(mfrow=c(2,2))
# plot(BUM_PropPos)
# hist(residuals(BUM_PropPos,type="pearson"))
# qqnorm(residuals(BUM_PropPos))
# qqline(residuals(BUM_PropPos))

# plot(BUM_Pos)
# hist(residuals(BUM_Pos, type = "pearson"))
# qqnorm(residuals(BUM_Pos))
# qqline(residuals(BUM_Pos))




# ## partial dependence plots for Postives model
pdp_year <- pdp::partial(
    object = BUM_Pos,
    pred.var = "Year",
    train = BUMPos
)

pdp_pos_year<-autoplot(
    object = pdp_year,
    # Customizations for the plot
     rug = TRUE, # Show rug marks at the bottom to indicate data density
    ylab = "Partial Dependence", 
    train = BUMPos
) + 
theme_bw()
BUMPos$Bait<-as.factor(BUMPos$Bait)
bait_levels<-unique(BUMPos$Bait)
pdp_bait <- pdp::partial(
    object = BUM_Pos,
    pred.var = "Bait",
    train = BUMPos,
    type = "regression"
)

pdp_pos_bait<-autoplot(
    object = pdp_bait,
    # Customizations for the plot
    rug = TRUE, # Show rug marks at the bottom to indicate data density
    ylab = "Partial Dependence",
    train = BUMPos
) +
    theme_bw()
pdp_begin <- pdp::partial(
    object = BUM_Pos,
    pred.var = "Begin",
    train = BUMPos,
    type = "regression"
)

pdp_pos_begin<-autoplot(
    object = pdp_begin,
    # Customizations for the plot
     rug = TRUE, # Show rug marks at the bottom to indicate data density
    ylab = "Partial Dependence", 
    train = BUMPos
) + 
theme_bw() +
theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

BUMPos$HPF<-as.factor(BUMPos$HPF)
pdp_hpf <- pdp::partial(
    object = BUM_Pos,
    pred.var = "HPF",
    train = BUMPos,
    type = "regression"
)

pdp_pos_hpf<-autoplot(
    object = pdp_hpf,
    # Customizations for the plot
    rug = TRUE, # Show rug marks at the bottom to indicate data density
    ylab = "Partial Dependence",
    train = BUMPos
) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


# ## partial dependence plots for Proportion positives model
BUMCPUE$Year<-as.factor(BUMCPUE$Year)
pdp_year <- pdp::partial(
    object = BUM_PropPos,
    pred.var = "Year",
    train = BUMCPUE
)

pdp_prop_year<-autoplot(
    object = pdp_year,
    # Customizations for the plot
    rug = TRUE, # Show rug marks at the bottom to indicate data density
    ylab = "Partial Dependence",
    train = BUMCPUE
) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

BUMCPUE$Bait <- as.factor(BUMCPUE$Bait)
bait_levels <- unique(BUMCPUE$Bait)
pdp_bait <- pdp::partial(
    object = BUM_PropPos,
    pred.var = "Bait",
    train = BUMCPUE,
    type = "regression"
)

pdp_prop_bait<-autoplot(
    object = pdp_bait,
    # Customizations for the plot
    rug = TRUE, # Show rug marks at the bottom to indicate data density
    ylab = "Partial Dependence",
    train = BUMCPUE
) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


pdp_begin <- pdp::partial(
    object = BUM_PropPos,
    pred.var = "Begin",
    train = BUMCPUE,
    type = "regression"
)

pdp_prop_begin<-autoplot(
    object = pdp_begin,
    # Customizations for the plot
    rug = TRUE, # Show rug marks at the bottom to indicate data density
    ylab = "Partial Dependence",
    train = BUMCPUE
) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

BUMCPUE$HPF <- as.factor(BUMCPUE$HPF)
pdp_hpf <- pdp::partial(
    object = BUM_PropPos,
    pred.var = "HPF",
    train = BUMCPUE,
    type = "regression"
)

pdp_prop_hpf<-autoplot(
    object = pdp_hpf,
    # Customizations for the plot
    rug = TRUE, # Show rug marks at the bottom to indicate data density
    ylab = "Partial Dependence",
    train = BUMCPUE
) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

pdp_mon <- pdp::partial(
    object = BUM_PropPos,
    pred.var = "Month",
    train = BUMCPUE,
    type = "regression"
)

pdp_prop_mon<-autoplot(
    object = pdp_mon,
    # Customizations for the plot
    rug = TRUE, # Show rug marks at the bottom to indicate data density
    ylab = "Partial Dependence",
    train = BUMCPUE
) +
    theme_bw()

    pdp_lon <- pdp::partial(
        object = BUM_PropPos,
        pred.var = "Lon",
        train = BUMCPUE,
        type = "regression"
    )
pdp_prop_lon<-    autoplot(
        object = pdp_lon,
        # Customizations for the plot
        rug = TRUE, # Show rug marks at the bottom to indicate data density
        ylab = "Partial Dependence",
        train = BUMCPUE
    ) +
        theme_bw()

pdp_lat <- pdp::partial(
    object = BUM_PropPos,
    pred.var = "Lat",
    train = BUMCPUE,
    type = "regression"
)
pdp_prop_lat<-autoplot(
    object = pdp_lat,
    # Customizations for the plot
    rug = TRUE, # Show rug marks at the bottom to indicate data density
    ylab = "Partial Dependence",
    train = BUMCPUE
) +
    theme_bw()




## positive model residual plots
#residuals vs Bait

PosResid_Bait<-ggplot(data.frame(x1=BUM_Pos@frame$`as.factor(Bait)`,pearson=residuals(BUM_Pos,type="pearson")),
          aes(x=x1,y=pearson)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Reference line
    theme_bw() +    
    labs(
        x = "Bait Type",
        y = "Pearson Residuals"
    ) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))



#residuals vs Year
PosResid_Year<-ggplot(data.frame(x1=BUM_Pos@frame$`as.factor(Year)`,pearson=residuals(BUM_Pos,type="pearson")),
          aes(x=x1,y=pearson)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Reference line
    theme_bw() +
    theme(axis.text.x=element_text(size=6))+
    labs(
        x = "Year",
        y = "Pearson Residuals"
    ) +
    theme(axis.text.x = element_text(size=4, angle = 45, vjust = 1, hjust = 1))

## residuals vs Begin Set time
PosResid_Begin<- ggplot(
    data.frame(x1 = BUM_Pos@frame$`as.factor(Begin)`, pearson = residuals(BUM_Pos, type = "pearson")),
    aes(x = x1, y = pearson)
) +
    geom_boxplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Reference line
    theme_bw() +
    labs(
        x = "Begin Set Time (Quarter of Day)",
        y = "Pearson Residuals"
    ) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

## residuals vs HPF
PosResid_HPF <- ggplot(
    data.frame(x1 = BUM_Pos@frame$`as.factor(HPF)`, pearson = residuals(BUM_Pos, type = "pearson")),
    aes(x = x1, y = pearson)
) +
    geom_boxplot() +
   geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Reference line
    theme_bw() +
    labs(
        x = "HPF",
        y = "Pearson Residuals"
    ) +
    theme(axis.text.x = element_text(size=4, angle = 45, vjust = 1, hjust = 1))






## Binomial model
BUM_PropPos1<-model.frame(BUM_PropPos)
Pearson_Residuals = residuals(BUM_PropPos, type = "pearson")
boxplot_data <- data.frame(
    "Year" = BUM_PropPos1$`as.factor(Year)`,
    "Bait" = BUM_PropPos1$`as.factor(Bait)`,
    "Month" = BUM_PropPos1$`as.factor(Month)`,
    "HPF" = BUM_PropPos1$`as.factor(HPF)`,
    "Begin" = BUM_PropPos1$`as.factor(Begin)`,
    "Lon" = BUM_PropPos1$Lon,
    "Lat" = BUM_PropPos1$Lat,
    "Pearson_Residuals" = Pearson_Residuals)

# 2. Plotting the boxplots
PPResid_Year<-ggplot(boxplot_data, aes(x = Year, y = Pearson_Residuals)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Reference line
    labs(
        x = "Year",
        y = "Pearson Residuals"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(size=4,angle = 45, hjust = 1))


PPResid_Month<-ggplot(boxplot_data, aes(x = Month, y = Pearson_Residuals)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Reference line
    labs(
        x = "Month",
        y = "Pearson Residuals"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

PPResid_Lat<-ggplot(boxplot_data, aes(x = Lat, y = Pearson_Residuals)) +
    geom_point(alpha=0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Reference line
#      geom_smooth(
#     method = "loess", 
#     se = FALSE,       # Don't show confidence interval band for clarity
#     color = "red", 
#     linewidth = 1.2
#   ) +
    labs(
        x = "Latitude",
        y = "Pearson Residuals"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


PPResid_Lon <- ggplot(boxplot_data, aes(x = Lon, y = Pearson_Residuals)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Reference line
    # geom_smooth(
    #     method = "loess",
    #     se = FALSE, # Don't show confidence interval band for clarity
    #     color = "red",
    #     linewidth = 1.2
    # ) +
    labs(
        x = "Longitude",
        y = "Pearson Residuals"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

PPResid_Bait<-ggplot(boxplot_data, aes(x = Bait, y = Pearson_Residuals)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Reference line
    labs(
        x = "Bait Type",
        y = "Pearson Residuals"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(size=4, angle = 45, hjust = 1))

PPResid_HPF<-ggplot(boxplot_data, aes(x = HPF, y = Pearson_Residuals)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Reference line
    labs(
        x = "Hooks Per Float",
        y = "Pearson Residuals"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(size=4, angle = 45, hjust = 1))

PPResid_Begin<-ggplot(boxplot_data, aes(x = Begin, y = Pearson_Residuals)) +
    geom_boxplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Reference line
    labs(
        x = "Begin Set Time (Quarter of Day)",
        y = "Pearson Residuals"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
