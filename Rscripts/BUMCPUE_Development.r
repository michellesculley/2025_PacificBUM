## Stepwise GLM development of BUM
## note with SST file starts in 1995, without SST starts in 1990

## start with positivies model
Final.Positives<-lmer(log(CPUE)~as.factor(Year)+as.factor(HPF)+as.factor(Month)+as.factor(Bait)+Lat+Lon+MLD+(1|Vessel),data=MLSCPUE2, REML=FALSE)
Model0 <- lmer(CPUE ~ 1 + (1|Vessel), data = BUMPos, REML=FALSE)


Model1.1 <- lmer(CPUE ~ as.factor(Year) + (1|Vessel), data = BUMPos, REML=FALSE)
Model1.2 <- lmer(CPUE ~ as.factor(Month) + (1 | Vessel), data = BUMPos, REML = FALSE)
Model1.3 <- lmer(CPUE ~ as.factor(Bait) + (1 | Vessel), data = BUMPos, REML = FALSE)
Model1.10 <- lmer(CPUE ~ as.factor(Begin) + (1|Vessel), data = BUMPos, REML=FALSE)
Model1.4 <- lmer(CPUE ~ BeginSetTime + (1 | Vessel), data = BUMPos, REML = FALSE)
Model1.5 <- lmer(CPUE ~ as.factor(HPF) + (1|Vessel), data = BUMPos, REML=FALSE)
Model1.6 <- lmer(CPUE ~ Lat + (1 | Vessel), data = BUMPos, REML = FALSE)
Model1.7 <- lmer(CPUE ~ Lon + (1 | Vessel), data = BUMPos, REML = FALSE)
Model1.8 <- lmer(CPUE ~ SOI + (1 | Vessel), data = BUMPos, REML = FALSE)
Model1.9 <- lmer(CPUE ~ PDO + (1 | Vessel), data = BUMPos, REML = FALSE)

Model1.11 <- lmer(CPUE ~ as.factor(Quarter) + (1 | Vessel), data = BUMPos, REML = FALSE)
#Model1.12 <- lmer(CPUE ~ SST + (1 | Vessel), data = BUMPos, REML = FALSE) onlyfor SST model, which isn't significant so eliminated and using 1990 start without SST (BUM_CPUEnoSST.csv)
Model1.13 <- lmer(CPUE ~ as.factor(SetType) + (1|Vessel), data = BUMPos, REML=FALSE)

anova(Model0,Model1.1, Model1.2, Model1.3, Model1.4, Model1.5, Model1.6, Model1.7, Model1.8, Model1.9, Model1.10,Model1.11,Model1.13)
AIC(Model0,Model1.1, Model1.2, Model1.3, Model1.4, Model1.5, Model1.6, Model1.7, Model1.8, Model1.9, Model1.10,Model1.11,Model1.13)
BIC(Model0,Model1.1, Model1.2, Model1.3, Model1.4, Model1.5, Model1.6, Model1.7, Model1.8, Model1.9, Model1.10,Model1.11,Model1.13)
models<-list(Model0,Model1.1, Model1.2, Model1.3, Model1.4, Model1.5, Model1.6, Model1.7, Model1.8, Model1.9, Model1.10,Model1.11,Model1.13)

deviance(Model0)
deviance(Model1.1)
deviance(Model1.2)
deviance(Model1.3)
deviance(Model1.4)
deviance(Model1.5)
deviance(Model1.6)
deviance(Model1.7)
deviance(Model1.8)
deviance(Model1.9)
deviance(Model1.10)
deviance(Model1.11)
deviance(Model1.13)


Model1.3 <- lmer(CPUE ~ as.factor(Bait) + (1 | Vessel), data = BUMPos, REML = FALSE)

Model2.1 <- lmer(CPUE ~ as.factor(Bait) + as.factor(Year) + (1|Vessel), data = BUMPos, REML=FALSE)
Model2.2 <- lmer(CPUE ~ as.factor(Bait) + as.factor(Month) + (1 | Vessel), data = BUMPos, REML = FALSE)
Model2.3 <- lmer(CPUE ~ as.factor(Bait) + as.factor(Begin) + (1|Vessel), data = BUMPos, REML=FALSE)
Model2.4 <- lmer(CPUE ~ as.factor(Bait) + as.factor(HPF) + (1|Vessel), data = BUMPos, REML=FALSE)
Model2.5 <- lmer(CPUE ~ as.factor(Bait) + Lat + (1 | Vessel), data = BUMPos, REML = FALSE)
Model2.6 <- lmer(CPUE ~ as.factor(Bait) + PDO + (1 | Vessel), data = BUMPos, REML = FALSE)
Model2.7 <- lmer(CPUE ~ as.factor(Bait) + as.factor(SetType) + (1|Vessel), data = BUMPos, REML=FALSE)

AIC(Model2.1, Model2.2, Model2.3, Model2.4, Model2.5, Model2.6, Model2.7)
BIC(Model2.1, Model2.2, Model2.3, Model2.4, Model2.5, Model2.6, Model2.7)
deviance(Model2.1)
deviance( Model2.2)
deviance( Model2.3)
deviance( Model2.4)
deviance( Model2.5)
deviance( Model2.6) 
deviance(Model2.7)

Model2.3 <- lmer(CPUE ~ as.factor(Bait) + as.factor(Begin) + (1|Vessel), data = BUMPos, REML=FALSE)


Model3.1 <- lmer(CPUE ~ as.factor(Bait) + as.factor(Begin) + as.factor(Year) + (1|Vessel), data = BUMPos, REML=FALSE)
Model3.2 <- lmer(CPUE ~ as.factor(Bait) + as.factor(Begin) + as.factor(Month) + (1 | Vessel), data = BUMPos, REML = FALSE)
Model3.3 <- lmer(CPUE ~ as.factor(Bait) + as.factor(Begin) + as.factor(HPF) + (1|Vessel), data = BUMPos, REML=FALSE)
Model3.4 <- lmer(CPUE ~ as.factor(Bait) + as.factor(Begin) + Lat + (1 | Vessel), data = BUMPos, REML = FALSE)
Model3.5 <- lmer(CPUE ~ as.factor(Bait) + as.factor(Begin) + PDO + (1 | Vessel), data = BUMPos, REML = FALSE)

AIC(Model3.1, Model3.2, Model3.3, Model3.4, Model3.5)
BIC(Model3.1, Model3.2, Model3.3, Model3.4, Model3.5)
deviance(Model3.1)
deviance( Model3.2)
deviance( Model3.3)
deviance( Model3.4)
deviance( Model3.5)

Model3.1 <- lmer(CPUE ~ as.factor(Bait) + as.factor(Begin) + as.factor(Year) + (1|Vessel), data = BUMPos, REML=FALSE)

Model4.1 <- lmer(CPUE ~ as.factor(Bait) + as.factor(Begin) + as.factor(Year) + as.factor(HPF) + (1|Vessel), data = BUMPos, REML=FALSE)

AIC(Model4.1)
BIC(Model4.1)
deviance(Model4.1)
