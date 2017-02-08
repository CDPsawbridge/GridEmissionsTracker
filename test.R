require(gridExtra)
require(ggplot2)
require(dplyr)
require(reshape2)
colVec<-c("#5B1A18", "#C93312","#29211F", "#DC863B","#899DA4","#ABDDDE","#78B7C5" ,"#FD6467", "#0B775E","#FAEFD1", "#EBCC2A","#3B9AB2", "#02401B")


# using a clever little subset argument here that means you can use different geoms on differe facets
  ggplot(data=bmPlotFrame4,aes(y=value,x=as.POSIXlt(timestamp)))+
  geom_area(data=subset(bmPlotFrame4,variable %in% c("MW","tCO2e")),aes(fill=Source))+
  geom_line(data=subset(bmPlotFrame4,variable %in% c("tCO2e/MWh")),aes(colour=Source))+
  facet_grid(variable~.,scales="free_y")+
  ggtitle('UK Energy Mix and Greenhouse Gas Emissions over the last 24hrs')+
  theme(strip.text.y = element_text(size = 14,face="bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),    
        axis.text.x  = element_text(size=16),
        axis.text.y  = element_text(size=16),
        legend.text = element_text(size=16),
        legend.position="bottom",
        legend.title = element_text(face="bold",size=16),
        title = element_text(face="bold",size=16))+
  scale_fill_manual(values=c(colVec,colVec))

# calculate the grid intensity, remeber to multiply by 12 to get it into MWh (or actually divide it really) because it's done in 5 minute time slots
# have added the +0.0000000000000000001 to avoid dividing by 0
  bmPlotFrame3<-bmPlotFrame2 %>% 
  dcast(timestamp+colCol+Source~variable) %>% 
  group_by(timestamp) %>% 
  summarise(value=sum(tCO2e,na.rm=T)/(sum(MW,na.rm=T)+0.0000000000000000001)*12) %>%
  mutate(variable="tCO2e/MWh",
         Source="Current",
         colCol="#DC863F")


# rbind on values for the IEA and daily average, not using an abline because it only appears on one of the facets
bmPlotFrame4<- rbind(mutate(bmPlotFrame3,value=mean(bmPlotFrame3$value,na.rm=T) ,Source="Daily Avergae"), 
                     mutate(bmPlotFrame3,value=0.45885,Source="IEA Official Value (2012)"),
                     bmPlotFrame3,
                     bmPlotFrame2)
                     



