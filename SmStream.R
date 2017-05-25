library(tidyr)
library(dplyr)
library(lubridate)

setwd("C:/Chehalis/Habitats/R")
SmStream<-read.delim("SmStream_Final.txt",header=TRUE)


##################################################################
################################################################
####################################################################
###Small stream rearing area without temperature mask, current

SmStream_cap<-SmStream%>%
  mutate(CurrTemp=ifelse(Tm_Curr_1>24,"High","Low"),Culvert=ifelse(culv1_1==0,0,1),
         SChino_pres=ifelse(EDT_SChino=="Yes"&!EDT_FChino=="Yes",1,ifelse(EDT_SChino=="Yes"&EDT_FChino=="Yes",.19,0)),
         FChino_pres=ifelse(EDT_FChino=="Yes"&!EDT_SChino=="Yes",1,ifelse(EDT_FChino=="Yes"&EDT_SChino=="Yes",.81,0)),
         SPool=AreaPool*SChino_pres,SRiff=AreaRiff*SChino_pres)%>%
  select(SubBasin,LandSlop_1,Culvert,SPool,SRiff,Basin_num)%>%
  arrange(Basin_num,Culvert)%>%
  unite(Basin_num,Culvert,col="Basin",sep=".")%>%
  mutate(Basin.Culv=as.numeric(Basin))%>%
  select(Basin.Culv,LandSlop_1,SPool,SRiff)%>%
  group_by(Basin.Culv,LandSlop_1)%>%
  summarize(Pool=sum(SPool),Riff=sum(SRiff))%>%
  gather(PoolRiffle,Area,Pool,Riff)%>%
  arrange(LandSlop_1,PoolRiffle)%>%
  unite(LandSlop_1,PoolRiffle,col="LandSlope",sep="_")%>%
  arrange(Basin.Culv)%>%
  spread(Basin.Culv,Area)%>%
  mutate(num=c(1,2,5,6,3,4,7,8,11,12,9,10,13,14,17,18,15,16,19,20,23,24,21,22,25,26,27,28,31,32,29,30))%>%
  arrange(num)

SmStream_notemp<-data.frame(SmStream_cap)
SmStream_notemp[is.na(SmStream_notemp)]<-0

write.table(SmStream_notemp,"C:/Chehalis/Habitats/R/SChino/trib_NoTemp.txt",sep="\t",col.names = TRUE)  



####################################################
#####################################################
#######################################################
##Small stream rearing capacity with temperature mask, current

SmStream_capT<-SmStream%>%
  mutate(CurrTemp=ifelse(Tm_Curr_1>24,0,1),Culvert=ifelse(culv1_1==0,0,1),
         SChino_pres=ifelse(EDT_SChino=="Yes"&!EDT_FChino=="Yes",1,ifelse(EDT_SChino=="Yes"&EDT_FChino=="Yes",.19,0)),
         FChino_pres=ifelse(EDT_FChino=="Yes"&!EDT_SChino=="Yes",1,ifelse(EDT_FChino=="Yes"&EDT_SChino=="Yes",.81,0)),
         SPool=AreaPool*SChino_pres,SRiff=AreaRiff*SChino_pres,PoolT=SPool*CurrTemp,RiffT=SRiff*CurrTemp)%>%
  select(SubBasin,LandSlop_1,Culvert,PoolT,RiffT,Basin_num)%>%
  arrange(Basin_num,Culvert)%>%
  unite(Basin_num,Culvert,col="Basin",sep=".")%>%
  mutate(Basin.Culv=as.numeric(Basin))%>%
  select(Basin.Culv,LandSlop_1,PoolT,RiffT)%>%
  group_by(Basin.Culv,LandSlop_1)%>%
  summarize(Pool=sum(PoolT),Riff=sum(RiffT))%>%
  gather(PoolRiffle,Area,Pool,Riff)%>%
  arrange(LandSlop_1,PoolRiffle)%>%
  unite(LandSlop_1,PoolRiffle,col="LandSlope",sep="_")%>%
  arrange(Basin.Culv)%>%
  spread(Basin.Culv,Area)%>%
  mutate(num=c(1,2,5,6,3,4,7,8,11,12,9,10,13,14,17,18,15,16,19,20,23,24,21,22,25,26,27,28,31,32,29,30))%>%
  arrange(num)

SmStream_temp<-data.frame(SmStream_capT)
SmStream_temp[is.na(SmStream_temp)]<-0

write.table(SmStream_temp,"C:/Chehalis/Habitats/R/SChino/trib_Temp.txt",sep="\t",col.names = TRUE)  
  
##################################################################
################################################################
####################################################################
###Small stream rearing area without temperature mask, current

SmStream_capH<-SmStream%>%
  mutate(Culvert=ifelse(culv1_1==0,0,1),
         SChino_pres=ifelse(EDT_SChino=="Yes"&!EDT_FChino=="Yes",1,ifelse(EDT_SChino=="Yes"&EDT_FChino=="Yes",.19,0)),
         PoolA=HBvr_PoolA*SChino_pres,RiffA=HBvr_RiffA*SChino_pres,PondA=HBvr_PondA*SChino_pres)%>%
  select(SubBasin,LandSlop_1,Culvert,PoolA,RiffA,PondA,Basin_num)%>%
  arrange(Basin_num,Culvert)%>%
  unite(Basin_num,Culvert,col="Basin",sep=".")%>%
  mutate(Basin.Culv=as.numeric(Basin))%>%
  select(Basin.Culv,LandSlop_1,PoolA,RiffA,PondA)%>%
  group_by(Basin.Culv,LandSlop_1)%>%
  summarize(Pool=sum(PoolA),Riff=sum(RiffA),Pond=sum(PondA))%>%
  gather(PoolRifflePond,Area,Pool,Riff,Pond)%>%
  arrange(LandSlop_1,PoolRifflePond)%>%
  unite(LandSlop_1,PoolRifflePond,col="LandSlope",sep="_")%>%
  arrange(Basin.Culv)%>%
  spread(Basin.Culv,Area)%>%
  mutate(num=c(3,1,2,9,7,8,6,4,5,12,10,11,18,16,17,15,13,14,21,19,20,27,25,26,24,22,23,30,28,29,36,34,35,33,31,32,39,37,38,42,40,41,48,46,47,45,43,44))%>%
  arrange(num)

SmStream_Histnotemp<-data.frame(SmStream_capH)
SmStream_Histnotemp[is.na(SmStream_Histnotemp)]<-0

write.table(SmStream_Histnotemp,"C:/Chehalis/Habitats/R/SChino/trib_HistNoTemp.txt",sep="\t",col.names = TRUE)  



####################################################
#####################################################
#######################################################
##Small stream rearing capacity with temperature mask, Hist

SmStream_capTH<-SmStream%>%
  mutate(HistTemp=ifelse(Tm_Hist_1>24,0,1),Culvert=ifelse(culv1_1==0,0,1),
         SChino_pres=ifelse(EDT_SChino=="Yes"&!EDT_FChino=="Yes",1,ifelse(EDT_SChino=="Yes"&EDT_FChino=="Yes",.19,0)),
         PoolA=HBvr_PoolA*SChino_pres*HistTemp,RiffA=HBvr_RiffA*SChino_pres*HistTemp,PondA=HBvr_PondA*SChino_pres*HistTemp)%>%
  select(SubBasin,LandSlop_1,Culvert,PoolA,RiffA,PondA,Basin_num)%>%
  arrange(Basin_num,Culvert)%>%
  unite(Basin_num,Culvert,col="Basin",sep=".")%>%
  mutate(Basin.Culv=as.numeric(Basin))%>%
  select(Basin.Culv,LandSlop_1,PoolA,RiffA,PondA)%>%
  group_by(Basin.Culv,LandSlop_1)%>%
  summarize(Pool=sum(PoolA),Riff=sum(RiffA),Pond=sum(PondA))%>%
  gather(PoolRifflePond,Area,Pool,Riff,Pond)%>%
  arrange(LandSlop_1,PoolRifflePond)%>%
  unite(LandSlop_1,PoolRifflePond,col="LandSlope",sep="_")%>%
  arrange(Basin.Culv)%>%
  spread(Basin.Culv,Area)%>%
  mutate(num=c(3,1,2,9,7,8,6,4,5,12,10,11,18,16,17,15,13,14,21,19,20,27,25,26,24,22,23,30,28,29,36,34,35,33,31,32,39,37,38,42,40,41,48,46,47,45,43,44))%>%
  arrange(num)

SmStream_Histtemp<-data.frame(SmStream_capTH)
SmStream_Histtemp[is.na(SmStream_Histtemp)]<-0

write.table(SmStream_Histtemp,"C:/Chehalis/Habitats/R/SChino/trib_HistTemp.txt",sep="\t",col.names = TRUE)  
