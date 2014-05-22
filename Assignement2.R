Data <- read.csv(bzfile("C:/Users/JosePortatil/Documents/GitHub/RepData_PerrAss2/repdata-data-StormData.csv.bz2"))

setwd("C:/Users/JosePortatil/Documents/GitHub/RepData_PerrAss2/")
#Load library 
require(car)

#Se debe depurar los eventos y ponerlos en categorias. 
##Group events. 
#A little should be more . 

Data$EVTYPE<-with(Data,recode(EVTYPE,"  
                        c('avalanche', 'avalance', 'blizzard', 'chill', 'cold', 'cool', 'glaze', 'hypothermia', 'hyperthermia', 
'ice', 'icy', 'freez', 'frost', 'low temp', 'sleet', 'snow', 'wint')='COLD / ICE';
                        c('below normal precip', 'dry', 'drie', 'drought', 'fire', 'heat', 'high temp', 'hot', 'warm')
='HEAT / DROUGHT / FIRE';
                        c('fog', 'vog')='FOG';
                         c('coast', 'cstl', 'current', 'dam fail', 'dam break', 'drizzle', 'drown', 'erosion', 'erosin', 
'flood', 'floood', 'fld', 'heavy shower', 'high water', 'high waves', 'lake', 'landslump', 'marine', 'precip', 'rain', 
'rising water', 'river', 'rogue wave', 'slide', 'stream', 'sea', 'seiche', 'surf', 'swell', 'tide', 'tidal', 'torrent', 'wet')='RAIN / FLOOD / HIGH SEAS';
                      c('burst', 'cloud', 'depression', 'floyd', 'funnel', 'gust', 'hail', 'hurricane', 'landspout', 
'storm', 'southeast', 'thunderstorm', 'thundertsorm', 'thundestorm', 'tornado', 'torndao', 'tstm', 'turbulence', 'typhoon', 
'wall', 'waterspout', 'water spout', 'wayterspout', 'wind', 'wnd')
='WIND / STORM / TORNADO';
c('lightning', 'ligntning', 'lighting')='LIGHTNING';
c('tsunami', 'volcan')='VOLCANIC ERUPTION / EARTHQUAKE / TSUNAMI';
                        c('dust')='DUST'"))


with(Data,levels(EVTYPE)

library(plyr)
INJURIESSummary<-ddply(Data,~EVTYPE,summarise,sum_INJURIES=sum(INJURIES,na.rm=TRUE))
INJURIESSummary
#Extract events with injuries higher than 100 

EventsInjuries<-subset(INJURIESSummary,sum_INJURIES>100)

#Order 
EventsInjuries <- EventsInjuries[order(EventsInjuries$sum_INJURIES),]

# Simple Dotplot Injuries 
dotchart(EventsInjuries$sum_INJURIES,labels=EventsInjuries$EVTYPE,cex=.5,
         main="Event type Vs Injuries", 
         xlab="SUm of Injuries caused")


#Extract Events with fatalities 
FATALITIESSummary<-ddply(Data,~EVTYPE,summarise,sum_FATALITIES=sum(FATALITIES,na.rm=TRUE))
FATALITIESSummary

#Extract events with fatalities higher than 10

EventsFATALITIES<-subset(FATALITIESSummary,sum_FATALITIES>10)
EventsFATALITIES

#Order 
EventsFATALITIES <- EventsFATALITIES[order(EventsFATALITIES$sum_FATALITIES),]

# Simple Dotplot
dotchart(EventsFATALITIES$sum_FATALITIES,labels=EventsFATALITIES$EVTYPE,cex=.45,
         main="Event type Vs Fatalities", 
         xlab="SUm of Fatalities caused")



#Susbet the ones with Billions B , then apply GROUP BY , then plot points in billions. 

EventsBillions<-subset(Data,PROPDMGEXP=="B")
CostSummary<-ddply(EventsBillions,~EVTYPE,summarise,sum_COST=sum(PROPDMG,na.rm=TRUE))
CostSummary

#Order 
CostSummary <- CostSummary[order(CostSummary$sum_COST),]

# Simple Dotplot
dotchart(CostSummary$sum_COST,labels=CostSummary $EVTYPE,cex=.5,
         main="Event type Vs Cost [Billions]", 
         xlab="Cost in Billions ")