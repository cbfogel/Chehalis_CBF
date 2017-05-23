library(tidyr)
library(dplyr)
library(lubridate)

setwd("C:/Chehalis/Habitats/R")
SmStream<-read.delim("SmStream_Final.txt",header=TRUE)

SmStream_cap<-SmStream%>%
  mutate(CurrTemp=ifelse(Tm_Curr_1>24,"High","Low"),Culvert=ifelse(culv1_1==0,"Below","Above"),
         SChino_pres=ifelse(EDT_SChino=="Yes"&!EDT_FChino=="Yes",1,ifelse(EDT_SChino=="Yes"&EDT_FChino=="Yes",.19,0)),
         FChino_pres=ifelse(EDT_FChino=="Yes"&!EDT_SChino=="Yes",1,ifelse(EDT_FChino=="Yes"&EDT_SChino=="Yes",.81,0)),
         SPool=AreaPool*SChino_pres,SRiff=AreaRiff*SChino_pres)%>%
  select(SubBasin,LandSlop_1,Culvert,SPool,SRiff)%>%
  arrange(SubBasin,Culvert)%>%
  unite(SubBasin,Culvert,col="Basin",sep="_")%>%
  group_by(Basin,LandSlop_1)%>%
  summarize(Pool=sum(SPool),Riff=sum(SRiff))%>%
  gather(PoolRiffle,Area,Pool,Riff)%>%
  arrange(LandSlop_1,PoolRiffle)%>%
  unite(LandSlop_1,PoolRiffle,col="LandSlope",sep="_")%>%
  spread(Basin,Area)

write.table(SmStream_cap,"C:/Chehalis/Habitats/R/SChino/trib_NoTemp.txt",sep="\t",col.names = TRUE)  
  
SmStream_cap_Temp<-SmStream%>%
  mutate(CurrTemp=ifelse(Tm_Curr_1>24,"High","Low"),Culvert=ifelse(culv1_1==0,"Below","Above"),
         SChino_pres=ifelse(EDT_SChino=="Yes"&!EDT_FChino=="Yes",1,ifelse(EDT_SChino=="Yes"&EDT_FChino=="Yes",.19,0)),
         FChino_pres=ifelse(EDT_FChino=="Yes"&!EDT_SChino=="Yes",1,ifelse(EDT_FChino=="Yes"&EDT_SChino=="Yes",.81,0)),
         SPool=AreaPool*SChino_pres,SRiff=AreaRiff*SChino_pres)%>%
  select(SubBasin,LandSlop_1,Culvert,SPool,SRiff,CurrTemp)%>%
  arrange(SubBasin,Culvert,CurrTemp)%>%
  unite(SubBasin,Culvert,CurrTemp,col="Basin",sep="_")%>%
  group_by(Basin,LandSlop_1)%>%
  summarize(PoolA=sum(SPool),RiffA=sum(SRiff))
  
  
  

NOAA_DU<-c("Above Crim","Elk to Crim","Elk Creek")
NOAA_DU
