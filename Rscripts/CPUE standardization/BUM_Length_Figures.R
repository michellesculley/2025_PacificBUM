library(ggplot2)
library(mgcv)

library(reshape2)
library(lubridate)



BUMLength<-read.csv("C:\\Users\\Michelle.Sculley\\Documents\\2025_PacificBUM\\HI CPUE\\BUM_length_94_24.csv", header=TRUE)


BUMLength <- BUMLength %>%
    mutate(
        # Convert text to a real date-time object first
        dt_obj = mdy_hm(HAUL_BEGIN_DATETIME),
        # Extract the pieces
        HAULBEGIN_MON = month(dt_obj),
        HAULBEGIN_DAY = day(dt_obj),
        HAULBEGIN_YR = year(dt_obj),
        HAULBEGIN_TIME = format(dt_obj, "%H:%M")
    )

BUMLength$LATITUDE<-ifelse(is.na(BUMLength$HAUL_BEGIN_LAT),BUMLength$HAUL_END_LAT, BUMLength$HAUL_BEGIN_LAT)
BUMLength$LONG <- ifelse(is.na(BUMLength$HAUL_BEGIN_LON), BUMLength$HAUL_END_LON, BUMLength$HAUL_BEGIN_LON)
BUMLength <- subset(BUMLength, MEAS1_TYPE_ID==1350| MEAS1_TYPE_ID==7852)
BUMLength<-subset(BUMLength, !is.na(MEAS1))
BUMLength$LATITUDE<-as.numeric(BUMLength$LATITUDE)
BUMLength$LONG<-as.numeric(BUMLength$LONG)

BUMLength$Fleet<-substr(BUMLength$T_TRIP_NUM,1,2)


BUM_Length<-data.frame("Year"=as.numeric(BUMLength[,"HAULBEGIN_YR"]),
                       "Month"=as.numeric(BUMLength[,"HAULBEGIN_MON"]),
                       "Day"=as.numeric(BUMLength[,"HAULBEGIN_DAY"]),
                       "Lat"=BUMLength[,"LATITUDE"],
                       "Length"=as.numeric(BUMLength[,"MEAS1"]),
                       "Lon"=BUMLength[,"LONG"],
                       "Fleet"=BUMLength[,"Fleet"])
BUM_Length<-subset(BUM_Length,!is.na(Lat)|!is.na(Lon))
BUM_Length<-subset(BUM_Length, !is.na(Length))
BUM_Length<-subset(BUM_Length,Year!=2025)

Q1<-which(BUM_Length$Month>=1&BUM_Length$Month<=3)
Q2<-which(BUM_Length$Month>=4&BUM_Length$Month<=6)
Q3<-which(BUM_Length$Month>=7&BUM_Length$Month<=9)
BUM_Length$Quarter<-4
BUM_Length[Q1,"Quarter"]<-1
BUM_Length[Q2,"Quarter"]<-2
BUM_Length[Q3,"Quarter"]<-3   

BUM_Length$Lon<-ifelse(BUM_Length$Lon<0,BUM_Length$Lon,BUM_Length$Lon-360)
BUM_Length$Lat1<-ceiling(BUM_Length$Lat)
BUM_Length$Lon1<-ceiling(BUM_Length$Lon)
BUM_Length$Lat5<-(ceiling(BUM_Length$Lat/5)*5)-2.5
BUM_Length$Lon5<-(ceiling(BUM_Length$Lon /5)*5)-2.5

BUM_HIMean<-mean(subset(BUM_Length, Fleet=="LL")$Length)
BUM_ASMean <- mean(subset(BUM_Length, Fleet == "AS")$Length)
# library(plyr)
# BUMUnique<-unique(BUM_Length[,c("Year","Lat1","Lon1","Vessel")])
# UniqueCount<-plyr::count(BUMUnique,c("Year","Lat1","Lon1"))
# BUM_Length<-merge(BUM_Length,UniqueCount,by=c("Year","Lat1","Lon1"))
# BUM_Length$Include1<-ifelse(BUM_Length$freq<3,0,1)

# BUMUnique5<-unique(BUM_Length[,c("Year","Lat5","Lon5","Vessel")])
# UniqueCount5<-plyr::count(BUMUnique5,c("Year","Lat5","Lon5"))
# BUM_Length<-merge(BUM_Length,UniqueCount5,by=c("Year","Lat5","Lon5"))
# BUM_Length$Include5<-ifelse(BUM_Length$freq.y<3,0,1)
# BUMMapping1<-subset(BUM_Length,Include1==1)
# BUMLMapping5<-subset(BUM_Length,Include5==1)

# BUMUnique5<-unique(BUM_Length[,c("Year","Quarter","Lat5","Lon5","Vessel")])
# UniqueCount5<-plyr::count(BUMUnique5,vars=c("Year","Quarter","Lat5","Lon5"))
# BUM_Length<-merge(BUM_Length,UniqueCount5,by=c("Year","Quarter","Lat5","Lon5"))
# BUM_Length$IncludeQ5<-ifelse(BUM_Length$freq.y<3,0,1)
# BUMLMappingQ5<-subset(BUM_Length,IncludeQ5==1)




##Mean Length in 5x5
SLen5<-aggregate(BUM_Length$Length,by=list(BUM_Length$Lat5,BUM_Length$Lon5),mean)
names(SLen5)<-c("Lat","Lon","Length")
# mean(BUMLMapping5$Length)
# 166.34

# png("Figures\\MeanLengthOverall.png",height=4, width=4, units="in", res=200)
# ggplot()+
#     geom_point(aes(y=Lat,x=Lon,fill=Length),data=SLen5,shape=21,color="grey50",size=7)+
#     geom_polygon(data=hawaiiMap,aes(x=long,y=lat,group=group),color="white") +
#     scale_fill_gradient2(low='black',high='red',midpoint = 165) +
#     theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey50"),
#           panel.grid.minor = element_line(colour = "grey50"),
#           strip.background = element_blank()) +
#     scale_x_continuous(limits=c(-180,-130), minor_breaks = seq(-180,-130,5))+
#     scale_y_continuous(limits=c(-5,40),minor_breaks=seq(-5,40,5), breaks=seq(0,40,10))+
#     coord_fixed(1)
# dev.off()

SLenQ5<-aggregate(BUM_Length$Length,by=list(BUM_Length$Quarter,BUM_Length$Lat5,BUM_Length$Lon5),mean)
names(SLenQ5)<-c("Quarter","Lat","Lon","Length")

# aggregate(BUMLMappingQ5$Length,by=list(BUMLMappingQ5$Quarter),mean)
# Q length (cm)
# 1 159.18
# 2 168.98
# 3 170.46
# 4 162.31

# png("Figures\\MeanLengthQuarters.png",height=6, width=6, units="in", res=200)
# ggplot()+
#     geom_point(aes(y=Lat,x=Lon,fill=Length),data=SLenQ5,shape=21,size=5,color="gray50")+
#     geom_polygon(data=hawaiiMap,aes(x=long,y=lat,group=group)) +
#     scale_fill_gradient2(low='black',high='red', midpoint=165) +
#     theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey50"),
#           panel.grid.minor = element_line(colour = "grey50"),
#           strip.background = element_blank()) +
#     scale_x_continuous(limits=c(-180,-130), minor_breaks = seq(-180,-130,5))+
#     scale_y_continuous(limits=c(-5,35),minor_breaks=seq(-5,35,5), breaks=seq(0,30,10))+
#     coord_fixed(1)+
#     facet_wrap(~Quarter)
# dev.off()

# png("Figures\\LengthHistogram.png",height=6, width=6, units="in", res=200)
# ggplot()+
#     geom_histogram(aes(x=Length),data=BUM_Length, bins=50,fill="grey90",color="black") +
#     theme_bw() #+
#    geom_vline(aes(xintercept=179.76 ),color="red",linetype="dashed") # for adding 50% maturity
# dev.off()

# png("Figures\\LengthHistogramQuarterly.png",height=6, width=6, units="in", res=200)
# ggplot()+
#     geom_histogram(aes(x=Length),data=BUM_Length, bins=50,color="black",fill="grey90") +
#     theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
#           panel.grid.minor = element_blank(),
#           strip.background = element_blank(),
#           axis.text=element_text(size=14), 
#           axis.title=element_text(size=16),
#           strip.text = element_text(size=16))+
#     geom_vline(aes(xintercept=179.76),data=data.frame(1),color="red",linetype="dashed") +
#     facet_wrap(~Quarter) 
# dev.off()

# png("Figures\\LengthHistogramAnnual1.png",height=12, width=6, units="in", res=200)
# ggplot()+
#     geom_histogram(aes(x=Length),data=subset(BUM_Length,Year<1998), bins=50,color="black",fill="grey90") +
#     theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
#           panel.grid.minor = element_blank(),
#           strip.background = element_blank(),
#           axis.text=element_text(size=14), 
#           axis.title=element_text(size=16),
#           strip.text = element_text(size=16))+
#     facet_wrap(~Year, nrow=4) 
# dev.off()

# png("Figures\\LengthHistogramAnnual2.png",height=12, width=6, units="in", res=200)
# ggplot()+
#     geom_histogram(aes(x=Length),data=subset(BUM_Length,Year>=1998&Year<2002), bins=50,color="black",fill="grey90") +
#     theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
#           panel.grid.minor = element_blank(),
#           strip.background = element_blank(),
#           axis.text=element_text(size=14), 
#           axis.title=element_text(size=16),
#           strip.text = element_text(size=16))+
#     facet_wrap(~Year, nrow=4) 
# dev.off()

# png("Figures\\LengthHistogramAnnual3.png",height=12, width=6, units="in", res=200)
# ggplot()+
#     geom_histogram(aes(x=Length),data=subset(BUM_Length,Year>=2002&Year<2006), bins=50,color="black",fill="grey90") +
#     theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
#           panel.grid.minor = element_blank(),
#           strip.background = element_blank(),
#           axis.text=element_text(size=14), 
#           axis.title=element_text(size=16),
#           strip.text = element_text(size=16))+
#     facet_wrap(~Year, nrow=4) 
# dev.off()

# png("Figures\\LengthHistogramAnnual4.png",height=12, width=6, units="in", res=200)
# ggplot()+
#     geom_histogram(aes(x=Length),data=subset(BUM_Length,Year>=2006&Year<2010), bins=50,color="black",fill="grey90") +
#     theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
#           panel.grid.minor = element_blank(),
#           strip.background = element_blank(),
#           axis.text=element_text(size=14), 
#           axis.title=element_text(size=16),
#           strip.text = element_text(size=16))+
#     facet_wrap(~Year, nrow=4) 
# dev.off()

# png("Figures\\LengthHistogramAnnual5.png",height=12, width=6, units="in", res=200)
# ggplot()+
#     geom_histogram(aes(x=Length),data=subset(BUM_Length,Year>=2010&Year<2014), bins=50,color="black",fill="grey90") +
#     theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
#           panel.grid.minor = element_blank(),
#           strip.background = element_blank(),
#           axis.text=element_text(size=14), 
#           axis.title=element_text(size=16),
#           strip.text = element_text(size=16))+
#     facet_wrap(~Year, nrow=4) 
# dev.off()

# png("Figures\\LengthHistogramAnnual6.png",height=12, width=6, units="in", res=200)
# ggplot()+
#     geom_histogram(aes(x=Length),data=subset(BUM_Length,Year>=2014&Year<2018), bins=50,color="black",fill="grey90") +
#     theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
#           panel.grid.minor = element_blank(),
#           strip.background = element_blank(),
#           axis.text=element_text(size=14), 
#           axis.title=element_text(size=16),
#           strip.text = element_text(size=16))+
#     facet_wrap(~Year, nrow=4) 
# dev.off()

# png("Figures\\LengthHistogramAnnual7.png",height=12, width=6, units="in", res=200)
# ggplot()+
#     geom_histogram(aes(x=Length),data=subset(BUM_Length,Year>=2018), bins=50,color="black",fill="grey90") +
#     theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
#           panel.grid.minor = element_blank(),
#           strip.background = element_blank(),
#           axis.text=element_text(size=14), 
#           axis.title=element_text(size=16),
#           strip.text = element_text(size=16))+
#     facet_wrap(~Year, nrow=4) 
# dev.off()

# MonthLabels<-c("1"="Jan","2"="Feb","3"="Mar","4"="Apr","5"="May","6"="June","7"="July","8"="Aug","9"="Sept","10"="Oct","11"="Nov","12"="Dec")
# png("Figures\\LengthHistogramMonthly.png",height=12, width=16, units="in", res=200)
# ggplot()+
#     geom_histogram(aes(x=Length),data=BUM_Length, bins=50,color="black",fill="grey90") +
#     theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
#           panel.grid.minor = element_blank(),
#           strip.background = element_blank(),
#           axis.text=element_text(size=14), 
#           axis.title=element_text(size=16),
#           strip.text = element_text(size=16))+
#     geom_vline(aes(xintercept=179.76 ),data=data.frame(1),color="red",linetype="dashed") +
#     facet_wrap(~Month, labeller=labeller(Month=MonthLabels)) 
# dev.off()
# png("Figures\\LengthDensityMonthly.png",height=12, width=16, units="in", res=200)
# ggplot()+
#     geom_density(aes(x=Length),fill="grey90", data=BUM_Length) + 
#     theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey50"),
#           panel.grid.minor = element_blank(),
#           strip.background = element_blank(),
#           axis.text=element_text(size=14), 
#           axis.title=element_text(size=16),
#           strip.text = element_text(size=16)) +
#     facet_wrap(~Month, labeller=labeller(Month=MonthLabels))
# dev.off()

# png("Figures\\LengthDensityMonthlySet.png",height=12, width=16, units="in", res=200)
# ggplot()+
#     geom_density(aes(x=Length, fill=SetType, color=SetType), alpha=0.15, data=BUM_Length) + 
#     theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
#           panel.grid.minor = element_blank(),
#           strip.background = element_blank(),
#           axis.text=element_text(size=14), 
#           axis.title=element_text(size=16),
#           strip.text = element_text(size=16)) +
#     scale_color_manual(values=c("D"="black","S"="red"))+
#     scale_fill_manual(values=c("D"="black","S"="red"))+
#     scale_y_continuous(limits=c(0,0.05))+
#     facet_wrap(~Month, labeller=labeller(Month=MonthLabels))
# dev.off()

# png("Figures\\LengthDensityMonthlySex.png",height=12, width=16, units="in", res=200)
# ggplot()+
#     geom_density(aes(x=Length, fill=Sex, color=Sex), alpha=0.15, data=BUM_Length) + 
#     theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
#           panel.grid.minor = element_blank(),
#           strip.background = element_blank(),
#           axis.text=element_text(size=14), 
#           axis.title=element_text(size=16),
#           strip.text = element_text(size=16)) +
#     scale_color_manual(values=c("F"="black","M"="red", "U"="blue"))+
#     scale_fill_manual(values=c("F"="black","M"="red", "U"="blue")) +
#     facet_wrap(~Month, labeller=labeller(Month=MonthLabels))
# dev.off()

# png("Figures\\LengthDensitySet.png",height=12, width=16, units="in", res=200)
# ggplot()+
#     geom_density(aes(x=Length,fill=SetType,colour=SetType),alpha=0.15, data=BUM_Length) + 
#     theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
#           panel.grid.minor = element_blank(),
#           strip.background = element_blank(),
#           axis.text=element_text(size=14), 
#           axis.title=element_text(size=16), 
#           legend.text = element_text(size=14), legend.title = element_text(size=16)) +
#     scale_color_manual(values=c("D"="black","S"="red"))+
#     scale_fill_manual(values=c("D"="black","S"="red"))
# dev.off()

# png("Figures\\LengthDensitySex.png",height=12, width=16, units="in", res=200)
# ggplot()+
#     geom_density(aes(x=Length,fill=Sex,colour=Sex),alpha=0.15, data=BUM_Length) + 
#     theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
#           panel.grid.minor = element_blank(),
#           strip.background = element_blank(),
#           axis.text=element_text(size=14), 
#           axis.title=element_text(size=16), 
#           legend.text = element_text(size=14), legend.title = element_text(size=16)) +
#     scale_color_manual(values=c("F"="black","M"="red", "U"="blue"))+
#     scale_fill_manual(values=c("F"="black","M"="red", "U"="blue"))
# dev.off()
## Don't need as already set is based on 10 HPF
#BUM_Length$SetType2<-ifelse(BUM_Length$HPF>10,"D","S")
#png("Figures\\LengthDensitySet10hpf.png",height=12, width=16, units="in", res=200)
#ggplot()+
#    geom_density(aes(x=Length,fill=SetType2,colour=SetType2),alpha=0.15, data=BUM_Length) + theme_bw() 
#dev.off()



# png("Figures\\LengthBoxplot.png",height=4, width=10, units="in", res=300)
# q=boxplot(BUM_Length$Length~BUM_Length$Year, xlab="Year",ylab="Eye-Fork Length (cm)",ylim=c(0,350))
# text(x=1:26,y=10,labels=q$n, cex=0.75)
# dev.off()


###Tables for the paper:

# BUM_Length$Bin5<-ceiling(BUM_Length$Length/5)*5

# Length_Table<- BUM_Length %>%
#     count(Year, Quarter, Fleet, Bin5) %>%
#     arrange(Bin5) %>%
#     pivot_wider(names_from=Bin5,
#                 values_from=n, 
#                 values_fill=0
#                 )
# write.csv(Length_Table, "BUM_Length_table.csv")

