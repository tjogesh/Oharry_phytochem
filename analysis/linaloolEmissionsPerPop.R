labels(main)
library(dplyr)
lin_per_pop<-main %>% 
  group_by(Pop2)%>% 
  summarise(median_lin=median(linalool)/1000,
            mean_lin=mean(linalool)/1000,
            count =n())

lin_per_pop_presence<-main %>% 
  filter(linalool>0)%>%
  group_by(Pop2)%>% 
  summarise(median_lin=median(linalool)/1000,
            mean_lin=mean(linalool)/1000,
            count =n())

lin_mean_when_present<-main %>% 
  filter(linalool>0)%>%
  summarise(mean_lin=mean(linalool, na.omit=T), 
            count_presence = n(), 
            max_lin = max(linalool), 
            min_lin = min(linalool), 
            std_mean_lin=sd(linalool))

lin_mean_when_absent<-main %>% 
  filter(linalool==0)%>%
  summarise(mean_lin=mean(linalool, na.omit=T), 
            count_presence = n(), 
            max_lin = max(linalool), 
            min_lin = min(linalool), 
            std_mean_lin=sd(linalool))
#prop don't emit linalool
185/(185+446)
#prop emit linalool
446/(185+446)

#-------
#get GH emission rates for BMR

BMR<-read.csv('BMR_GH_emissions.csv')
labels(BMR)
BMR[,6:30]<-(BMR[,6:30]/BMR$toluene.int.std)*23.6*55

#use response factors to convert compounds with external stds
BMR$linalool<-BMR$linalool*RF_linalool

lin_BMR<-BMR %>% 
  summarise(median_lin=median(linalool)/1000,
            mean_lin=mean(linalool)/1000)
lin_BMR

BMR[BMR$linalool==0,]
