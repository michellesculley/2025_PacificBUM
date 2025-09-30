library(ggplot2)
library(mgcv)
library(maps)
library(maptools)
library(reshape2)
setwd("C:\\Users\\michelle.sculley\\Documents\\2021 BUM ASSESS")

BUMLength<-read.csv('BUM_length_94_20.csv', header=TRUE)

us<-readShapePoly("G:\\Swordfish\\tl_2016_us_state.shp")
hawaii<-subset(us,NAME=="Hawaii")
hawaiiMap<-fortify(hawaii)





BUM_Length<-data.frame("Year"=BUMLength[,"HAULBEGIN_YR"],
                       "Month"=BUMLength[,"HAULBEGIN_MON"],
                       "Day"=BUMLength[,"HAULBEGIN_DAY"],
                       "HPF"=BUMLength[,"HKS_PER_FLT"],
                       "Lat"=BUMLength[,"LATITUDE"],
                       "Length"=BUMLength[,"EYE_FORK_LEN"],
                       "Vessel"=BUMLength[,"PERMIT_NUM"],
                       "Lon"=BUMLength[,"LONG"]*-1,
                       "Target"=BUMLength[,"TARGET_SPECIES_CODE"],
                       "Sex"=BUMLength[,"GENDER"])
BUM_Length<-subset(BUM_Length,!is.na(HPF)|!is.na(Lat)|!is.na(Lon))
BUM_Length<-subset(BUM_Length, !is.na(Length))
BUM_Length<-subset(BUM_Length,Year!=2020)

Q1<-which(BUM_Length$Month>=1&BUM_Length$Month<=3)
Q2<-which(BUM_Length$Month>=4&BUM_Length$Month<=6)
Q3<-which(BUM_Length$Month>=7&BUM_Length$Month<=9)
BUM_Length$Quarter<-4
BUM_Length[Q1,"Quarter"]<-1
BUM_Length[Q2,"Quarter"]<-2
BUM_Length[Q3,"Quarter"]<-3   

BUM_Length$SetType<-ifelse(BUM_Length$Year<2004&BUM_Length$HPF<=10,"S",
                           ifelse(BUM_Length$Year>=2004&BUM_Length$HPF<=14,"S","D"))
BUM_Length$Lon<-ifelse(BUM_Length$Lon<0,BUM_Length$Lon,BUM_Length$Lon-360)
BUM_Length$Lat1<-ceiling(BUM_Length$Lat)
BUM_Length$Lon1<-ceiling(BUM_Length$Lon)
BUM_Length$Lat5<-(ceiling(BUM_Length$Lat/5)*5)-2.5
BUM_Length$Lon5<-(ceiling(BUM_Length$Lon /5)*5)-2.5

BUM_Length$Sex<-ifelse(BUM_Length$Sex=="F","F", ifelse(BUM_Length$Sex=="M","M","U"))

library(plyr)
BUMUnique<-unique(BUM_Length[,c("Year","Lat1","Lon1","Vessel")])
UniqueCount<-plyr::count(BUMUnique,c("Year","Lat1","Lon1"))
BUM_Length<-merge(BUM_Length,UniqueCount,by=c("Year","Lat1","Lon1"))
BUM_Length$Include1<-ifelse(BUM_Length$freq<3,0,1)

BUMUnique5<-unique(BUM_Length[,c("Year","Lat5","Lon5","Vessel")])
UniqueCount5<-plyr::count(BUMUnique5,c("Year","Lat5","Lon5"))
BUM_Length<-merge(BUM_Length,UniqueCount5,by=c("Year","Lat5","Lon5"))
BUM_Length$Include5<-ifelse(BUM_Length$freq.y<3,0,1)
BUMMapping1<-subset(BUM_Length,Include1==1)
BUMMapping5<-subset(BUM_Length,Include5==1)

BUMUnique5<-unique(BUM_Length[,c("Year","Quarter","Lat5","Lon5","Vessel")])
UniqueCount5<-plyr::count(BUMUnique5,vars=c("Year","Quarter","Lat5","Lon5"))
BUM_Length<-merge(BUM_Length,UniqueCount5,by=c("Year","Quarter","Lat5","Lon5"))
BUM_Length$IncludeQ5<-ifelse(BUM_Length$freq.y<3,0,1)
BUMMappingQ5<-subset(BUM_Length,IncludeQ5==1)




##Mean Length in 5x5
SLen5<-aggregate(BUMMapping5$Length,by=list(BUMMapping5$Lat5,BUMMapping5$Lon5),mean)
names(SLen5)<-c("Lat","Lon","Length")
mean(BUMMapping5$Length)
# 166.34

png("Figures\\MeanLengthOverall.png",height=4, width=4, units="in", res=200)
ggplot()+
    geom_point(aes(y=Lat,x=Lon,fill=Length),data=SLen5,shape=21,color="grey50",size=7)+
    geom_polygon(data=hawaiiMap,aes(x=long,y=lat,group=group),color="white") +
    scale_fill_gradient2(low='black',high='red',midpoint = 165) +
    theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey50"),
          panel.grid.minor = element_line(colour = "grey50"),
          strip.background = element_blank()) +
    scale_x_continuous(limits=c(-180,-130), minor_breaks = seq(-180,-130,5))+
    scale_y_continuous(limits=c(-5,40),minor_breaks=seq(-5,40,5), breaks=seq(0,40,10))+
    coord_fixed(1)
dev.off()

SLenQ5<-aggregate(BUMMappingQ5$Length,by=list(BUMMappingQ5$Quarter,BUMMappingQ5$Lat5,BUMMappingQ5$Lon5),mean)
names(SLenQ5)<-c("Quarter","Lat","Lon","Length")

aggregate(BUMMappingQ5$Length,by=list(BUMMappingQ5$Quarter),mean)
# Q length (cm)
# 1 159.18
# 2 168.98
# 3 170.46
# 4 162.31

png("Figures\\MeanLengthQuarters.png",height=6, width=6, units="in", res=200)
ggplot()+
    geom_point(aes(y=Lat,x=Lon,fill=Length),data=SLenQ5,shape=21,size=5,color="gray50")+
    geom_polygon(data=hawaiiMap,aes(x=long,y=lat,group=group)) +
    scale_fill_gradient2(low='black',high='red', midpoint=165) +
    theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey50"),
          panel.grid.minor = element_line(colour = "grey50"),
          strip.background = element_blank()) +
    scale_x_continuous(limits=c(-180,-130), minor_breaks = seq(-180,-130,5))+
    scale_y_continuous(limits=c(-5,35),minor_breaks=seq(-5,35,5), breaks=seq(0,30,10))+
    coord_fixed(1)+
    facet_wrap(~Quarter)
dev.off()

png("Figures\\LengthHistogram.png",height=6, width=6, units="in", res=200)
ggplot()+
    geom_histogram(aes(x=Length),data=BUM_Length, bins=50,fill="grey90",color="black") +
    theme_bw() #+
   geom_vline(aes(xintercept=179.76 ),color="red",linetype="dashed") # for adding 50% maturity
dev.off()

png("Figures\\LengthHistogramQuarterly.png",height=6, width=6, units="in", res=200)
ggplot()+
    geom_histogram(aes(x=Length),data=BUM_Length, bins=50,color="black",fill="grey90") +
    theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.text=element_text(size=14), 
          axis.title=element_text(size=16),
          strip.text = element_text(size=16))+
    geom_vline(aes(xintercept=179.76),data=data.frame(1),color="red",linetype="dashed") +
    facet_wrap(~Quarter) 
dev.off()

png("Figures\\LengthHistogramAnnual1.png",height=12, width=6, units="in", res=200)
ggplot()+
    geom_histogram(aes(x=Length),data=subset(BUM_Length,Year<1998), bins=50,color="black",fill="grey90") +
    theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.text=element_text(size=14), 
          axis.title=element_text(size=16),
          strip.text = element_text(size=16))+
    facet_wrap(~Year, nrow=4) 
dev.off()

png("Figures\\LengthHistogramAnnual2.png",height=12, width=6, units="in", res=200)
ggplot()+
    geom_histogram(aes(x=Length),data=subset(BUM_Length,Year>=1998&Year<2002), bins=50,color="black",fill="grey90") +
    theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.text=element_text(size=14), 
          axis.title=element_text(size=16),
          strip.text = element_text(size=16))+
    facet_wrap(~Year, nrow=4) 
dev.off()

png("Figures\\LengthHistogramAnnual3.png",height=12, width=6, units="in", res=200)
ggplot()+
    geom_histogram(aes(x=Length),data=subset(BUM_Length,Year>=2002&Year<2006), bins=50,color="black",fill="grey90") +
    theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.text=element_text(size=14), 
          axis.title=element_text(size=16),
          strip.text = element_text(size=16))+
    facet_wrap(~Year, nrow=4) 
dev.off()

png("Figures\\LengthHistogramAnnual4.png",height=12, width=6, units="in", res=200)
ggplot()+
    geom_histogram(aes(x=Length),data=subset(BUM_Length,Year>=2006&Year<2010), bins=50,color="black",fill="grey90") +
    theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.text=element_text(size=14), 
          axis.title=element_text(size=16),
          strip.text = element_text(size=16))+
    facet_wrap(~Year, nrow=4) 
dev.off()

png("Figures\\LengthHistogramAnnual5.png",height=12, width=6, units="in", res=200)
ggplot()+
    geom_histogram(aes(x=Length),data=subset(BUM_Length,Year>=2010&Year<2014), bins=50,color="black",fill="grey90") +
    theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.text=element_text(size=14), 
          axis.title=element_text(size=16),
          strip.text = element_text(size=16))+
    facet_wrap(~Year, nrow=4) 
dev.off()

png("Figures\\LengthHistogramAnnual6.png",height=12, width=6, units="in", res=200)
ggplot()+
    geom_histogram(aes(x=Length),data=subset(BUM_Length,Year>=2014&Year<2018), bins=50,color="black",fill="grey90") +
    theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.text=element_text(size=14), 
          axis.title=element_text(size=16),
          strip.text = element_text(size=16))+
    facet_wrap(~Year, nrow=4) 
dev.off()

png("Figures\\LengthHistogramAnnual7.png",height=12, width=6, units="in", res=200)
ggplot()+
    geom_histogram(aes(x=Length),data=subset(BUM_Length,Year>=2018), bins=50,color="black",fill="grey90") +
    theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.text=element_text(size=14), 
          axis.title=element_text(size=16),
          strip.text = element_text(size=16))+
    facet_wrap(~Year, nrow=4) 
dev.off()

MonthLabels<-c("1"="Jan","2"="Feb","3"="Mar","4"="Apr","5"="May","6"="June","7"="July","8"="Aug","9"="Sept","10"="Oct","11"="Nov","12"="Dec")
png("Figures\\LengthHistogramMonthly.png",height=12, width=16, units="in", res=200)
ggplot()+
    geom_histogram(aes(x=Length),data=BUM_Length, bins=50,color="black",fill="grey90") +
    theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.text=element_text(size=14), 
          axis.title=element_text(size=16),
          strip.text = element_text(size=16))+
    geom_vline(aes(xintercept=179.76 ),data=data.frame(1),color="red",linetype="dashed") +
    facet_wrap(~Month, labeller=labeller(Month=MonthLabels)) 
dev.off()
png("Figures\\LengthDensityMonthly.png",height=12, width=16, units="in", res=200)
ggplot()+
    geom_density(aes(x=Length),fill="grey90", data=BUM_Length) + 
    theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey50"),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.text=element_text(size=14), 
          axis.title=element_text(size=16),
          strip.text = element_text(size=16)) +
    facet_wrap(~Month, labeller=labeller(Month=MonthLabels))
dev.off()

png("Figures\\LengthDensityMonthlySet.png",height=12, width=16, units="in", res=200)
ggplot()+
    geom_density(aes(x=Length, fill=SetType, color=SetType), alpha=0.15, data=BUM_Length) + 
    theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.text=element_text(size=14), 
          axis.title=element_text(size=16),
          strip.text = element_text(size=16)) +
    scale_color_manual(values=c("D"="black","S"="red"))+
    scale_fill_manual(values=c("D"="black","S"="red"))+
    scale_y_continuous(limits=c(0,0.05))+
    facet_wrap(~Month, labeller=labeller(Month=MonthLabels))
dev.off()

png("Figures\\LengthDensityMonthlySex.png",height=12, width=16, units="in", res=200)
ggplot()+
    geom_density(aes(x=Length, fill=Sex, color=Sex), alpha=0.15, data=BUM_Length) + 
    theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.text=element_text(size=14), 
          axis.title=element_text(size=16),
          strip.text = element_text(size=16)) +
    scale_color_manual(values=c("F"="black","M"="red", "U"="blue"))+
    scale_fill_manual(values=c("F"="black","M"="red", "U"="blue")) +
    facet_wrap(~Month, labeller=labeller(Month=MonthLabels))
dev.off()

png("Figures\\LengthDensitySet.png",height=12, width=16, units="in", res=200)
ggplot()+
    geom_density(aes(x=Length,fill=SetType,colour=SetType),alpha=0.15, data=BUM_Length) + 
    theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.text=element_text(size=14), 
          axis.title=element_text(size=16), 
          legend.text = element_text(size=14), legend.title = element_text(size=16)) +
    scale_color_manual(values=c("D"="black","S"="red"))+
    scale_fill_manual(values=c("D"="black","S"="red"))
dev.off()

png("Figures\\LengthDensitySex.png",height=12, width=16, units="in", res=200)
ggplot()+
    geom_density(aes(x=Length,fill=Sex,colour=Sex),alpha=0.15, data=BUM_Length) + 
    theme(panel.background = element_blank(),  panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          axis.text=element_text(size=14), 
          axis.title=element_text(size=16), 
          legend.text = element_text(size=14), legend.title = element_text(size=16)) +
    scale_color_manual(values=c("F"="black","M"="red", "U"="blue"))+
    scale_fill_manual(values=c("F"="black","M"="red", "U"="blue"))
dev.off()
## Don't need as already set is based on 10 HPF
#BUM_Length$SetType2<-ifelse(BUM_Length$HPF>10,"D","S")
#png("Figures\\LengthDensitySet10hpf.png",height=12, width=16, units="in", res=200)
#ggplot()+
#    geom_density(aes(x=Length,fill=SetType2,colour=SetType2),alpha=0.15, data=BUM_Length) + theme_bw() 
#dev.off()



png("Figures\\LengthBoxplot.png",height=4, width=10, units="in", res=300)
q=boxplot(BUM_Length$Length~BUM_Length$Year, xlab="Year",ylab="Eye-Fork Length (cm)",ylim=c(0,350))
text(x=1:26,y=10,labels=q$n, cex=0.75)
dev.off()


###Tables for the paper:

BUM_Length$Bin5<-ceiling(BUM_Length$Length/5)*5
table(BUM_Length$Year,BUM_Length$Bin5,BUM_Length$Quarter)[,,1]
table(BUM_Length$Year,BUM_Length$Bin5,BUM_Length$Quarter)[,,2]
table(BUM_Length$Year,BUM_Length$Bin5,BUM_Length$Quarter)[,,3]
table(BUM_Length$Year,BUM_Length$Bin5,BUM_Length$Quarter)[,,4]
Length_Table<-table(BUM_Length$Year,BUM_Length$Bin5,BUM_Length$Quarter)
write.csv(Length_Table, "BUM_Length_table.csv")

