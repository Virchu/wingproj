
###Results for life history adar- family

##Install libraries
library(lme4)
library(lmerTest)
library(lsmeans)
library(multcomp)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(MASS)
library(GGally)
library(ggpubr)
library(survival)
library(survminer)
##Datafiles
#complete data set
all_data <- read.csv("C:\\Users\\virgc\\Documents\\GitHub\\wingproj\\Mosquito Life history colab\\Data files\\2018_07_23 Brazil Nydar dataset.csv")
#add survival prob
#all_data$surv<- 1-(1/all_data$AL)
#create dataframe of survived to adult data
adult<-subset(all_data, all_data$Death_stat==1 & !is.na(all_data$Sex1))

#field wing data
field_wing<-read.csv("C:\\Users\\virgc\\Documents\\GitHub\\wingproj\\Mosquito Life history colab\\Data files\\2018_04_20 Field wing length 13 18 cs scaled.csv")

#factor
all_data$State<-factor(all_data$State, levels=c("Amazonas", "Rondonia", "Tocantins", "Rio de Janeiro"))
all_data$Locality<-factor(all_data$Locality, levels=c("ARS", "APR", "RPV", "RMO","TLC","TPN", "SJU"))
adult$State<-factor(adult$State, levels=c("Amazonas", "Rondonia", "Tocantins", "Rio de Janeiro"))
adult$Locality<-factor(adult$Locality, levels=c("ARS", "APR", "RPV", "RMO","TLC","TPN", "SJU"))
adult$Temp_fac<-factor(adult$Temp_num, levels=c(20,24,28))
#adult$surv<-replace(adult$surv, is.infinite(adult$surv), NA)

##Organize data- family averages, some include sex
#all_data (n=3430) with separate sexes
all.sum <- all_data %>% group_by(Fam_new, Biome, State, Locality, Sex, Lat_group) %>% summarize(mean.sLL = mean(sLL, na.rm = TRUE), 
                                                                                                sd.sLL= sd(sLL, na.rm=TRUE),
                                                                                                N.sLL=length(sLL),
                                                                                                se.sLL=sd.sLL/sqrt(N.sLL),
                                                                                                mean.surv= mean(Death_stat, na.rm=TRUE),
                                                                                                mean.Temp= mean(Temp_num, na.rm=TRUE),
                                                                                                mean.Lat=mean(Latitude, na.rm=TRUE)) %>% data.frame

#all_data (n=3430) grouping sexes
all.sum.nosex <- all_data %>% group_by(Fam_new, Biome, State, Locality, Lat_group, Temp_num) %>% summarize(mean.sLL = mean(sLL, na.rm = TRUE),
                                                                                                           N.sLL=length(sLL),
                                                                                                           sd.sLL= sd(sLL, na.rm=TRUE),
                                                                                                           mean.surv= mean(Death_stat, na.rm=TRUE),
                                                                                                           mean.Temp = mean(Temp_num, na.rm=TRUE),
                                                                                                           mean.Lat=mean(Latitude, na.rm=TRUE),
                                                                                                           se.sLL=sd.sLL/sqrt(N.sLL),
                                                                                                           mean.AL = mean(AL, na.rm=TRUE),
                                                                                                           sd.AL= sd(AL, na.rm=TRUE),
                                                                                                           N.AL=length(AL),
                                                                                                           se.AL=sd.AL/sqrt(N.AL),
                                                                                                           mean.wing = mean(Wing.length..mm., na.rm=TRUE),
                                                                                                           sd.wing= sd(Wing.length..mm., na.rm=TRUE),
                                                                                                           N.wing=length(Wing.length..mm.),
                                                                                                           se.wing=sd.wing/sqrt(N.wing),
                                                                                                           sum.AL=sum(AL, na.rm=TRUE),
                                                                                                           surv.fam=1-(N.sLL/sum.AL)) %>% data.frame


#adult (n=2652) with separate sexes
adult.sum <- adult %>% group_by(Fam_new, Biome, State, Locality, Sex, Temp_fac, Lat_group) %>% summarize(mean.sLL = mean(sLL, na.rm = TRUE), 
                                                                                                         sd.sLL= sd(sLL, na.rm=TRUE),
                                                                                                         N.sLL=length(sLL),
                                                                                                         se.sLL=sd.sLL/sqrt(N.sLL),
                                                                                                         mean.AL = mean(AL, na.rm=TRUE),
                                                                                                         sd.AL= sd(AL, na.rm=TRUE),
                                                                                                         N.AL=length(AL),
                                                                                                         se.AL=sd.AL/sqrt(N.AL),
                                                                                                         mean.wing = mean(Wing.length..mm., na.rm=TRUE),
                                                                                                         sd.wing= sd(Wing.length..mm., na.rm=TRUE),
                                                                                                         N.wing=length(Wing.length..mm.),
                                                                                                         se.wing=sd.wing/sqrt(N.wing),
                                                                                                         mean.Temp = mean(Temp_num, na.rm = TRUE), 
                                                                                                         mean.Lat = mean(Latitude, na.rm = TRUE),
                                                                                                         sum.AL=sum(AL, na.rm=TRUE),
                                                                                                         surv.fam=1-(N.sLL/sum.AL)) %>% data.frame

#adult (n=2652)grouping sexes
adult.sum.nosex <- adult %>% group_by(Fam_new, Biome, State, Locality, Temp_fac, Lat_group) %>% summarize(mean.sLL = mean(sLL, na.rm = TRUE),
                                                                                                          sd.sLL= sd(sLL, na.rm=TRUE),
                                                                                                          N.sLL=length(sLL),
                                                                                                          se.sLL=sd.sLL/sqrt(N.sLL),
                                                                                                          mean.AL = mean(AL, na.rm=TRUE),
                                                                                                          sd.AL= sd(AL, na.rm=TRUE),
                                                                                                          N.AL=length(AL),
                                                                                                          se.AL=sd.AL/sqrt(N.AL),
                                                                                                          mean.wing = mean(Wing.length..mm., na.rm=TRUE),
                                                                                                          sd.wing= sd(Wing.length..mm., na.rm=TRUE),
                                                                                                          N.wing=length(Wing.length..mm.),
                                                                                                          se.wing=sd.wing/sqrt(N.wing),
                                                                                                          mean.Temp = mean(Temp_num), na.rm = TRUE, 
                                                                                                          mean.Lat = mean(Latitude, na.rm = TRUE),
                                                                                                          sum.AL=sum(AL, na.rm=TRUE),
                                                                                                          surv.fam=1-(N.sLL/sum.AL)) %>% data.frame
adult.sum.nosex$State<-factor(adult.sum.nosex$State, levels=c("Amazonas", "Rondonia", "Tocantins", "Rio de Janeiro"))
adult.sum.nosex$Lat_group<-factor(adult.sum.nosex$Lat_group, levels=c(1,2,3))

#summary of field wing data
field.sum <- field_wing %>% group_by(Biome, State, Latitude, Lat_grouple) %>%
  summarize (mean.wing=mean(Length.mm, na.rm=TRUE),
             sd.wing= sd(Length.mm, na.rm=TRUE),
             N.wing=length(Length.mm),   
             se.wing=sd.wing/sqrt(N.wing)) %>% data.frame                                                                                                
field.sum$State<-factor(field.sum$State, levels=c("Amazonas", "Rondonia", "Tocantins", "Rio de Janeiro"))

#group for correlations
adult.sum.corsub <- adult.sum[c(1:8,12,16,23)]
adult.sum.corsub$Temp<-as.numeric(adult.sum.corsub$Temp_fac)
adult.sum.corsub$Sex0<-ifelse(adult.sum.corsub$Sex=="M", 1,0)
adult.sum.corsub$Bio<-ifelse(adult.sum.corsub$Biome=="Amazon", 0,
                             ifelse(adult.sum.corsub$Biome=="Cerrado",1,2))
adult.sum.corsub$State0<-ifelse(adult.sum.corsub$State=="Amazonas", 0,
                                ifelse(adult.sum.corsub$State=="Rondonia",1,
                                       ifelse(adult.sum.corsub$State=="Tocantins",2,3)))

#by state
#Larvae development#
#sup a.1- larvae development fam averages by state over temperature
larv.am <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.sLL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Amazonas"))+
  ggtitle("Amazonas State")+
  xlab("Temperature (°C)")+ ylab("Larvae time (days)") +
  scale_y_continuous(breaks = c(13: 24))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

larv.ro <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.sLL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Rondonia"))+
  ggtitle("Rondonia State")+
  xlab("Temperature (°C)")+ ylab("Larvae time (days)") +
  scale_y_continuous(breaks = c(13: 24))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

larv.to <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.sLL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Tocantins"))+
  ggtitle("Tocantins State")+
  xlab("Temperature (°C)")+ ylab("Larvae time (days)") +
  scale_y_continuous(breaks = c(13: 24))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

larv.rj <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.sLL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Rio de Janeiro"))+
  ggtitle("Rio de Janeiro State")+
  xlab("Temperature (°C)")+ ylab("Larvae time (days)") +
  scale_y_continuous(breaks = c(13: 24))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

#arrange plots
grid.arrange(larv.am, larv.ro, larv.to, larv.rj, ncol=2, nrow=2, top="Average larvae development time (days) by Temperature and Family")


#Adult longevity#
#sup b.1
adult.am <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.AL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Amazonas"))+
  ggtitle("Amazonas State")+
  xlab("Temperature (???C)")+ ylab("Adult time (days)") +
  scale_y_continuous(breaks = c(0:6))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

adult.ro <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.AL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Rondonia"))+
  ggtitle("Rondonia State")+
  xlab("Temperature (???C)")+ ylab("Adult time (days)") +
  scale_y_continuous(breaks = c(0:6))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

adult.to <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.AL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Tocantins"))+
  ggtitle("Tocantins State")+
  xlab("Temperature (???C)")+ ylab("Adult time (days)") +
  scale_y_continuous(breaks = c(0:6))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

adult.rj <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.AL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Rio de Janeiro"))+
  ggtitle("Rio de Janeiro State")+
  xlab("Temperature (???C)")+ ylab("Adult time (days)") +
  scale_y_continuous(breaks = c(0:6))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

#arrange plots
grid.arrange(adult.am, adult.ro, adult.to, adult.rj, ncol=2, nrow=2, top="Average Adult development time (days) by Temperature and Family")


#sup c.1
wing.am <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.AL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Amazonas"))+
  ggtitle("Amazonas State")+
  xlab("Temperature (???C)")+ ylab("Wing length (mm)") +
  scale_y_continuous(breaks = c(0:6))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

wing.ro <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.AL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Rondonia"))+
  ggtitle("Rondonia State")+
  xlab("Temperature (???C)")+ ylab("Wing length (mm)") +
  scale_y_continuous(breaks = c(0:6))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

wing.to <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.AL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Tocantins"))+
  ggtitle("Tocantins State")+
  xlab("Temperature (???C)")+ ylab("Wing length (mm)") +
  scale_y_continuous(breaks = c(0:6))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

wing.rj <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.AL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Rio de Janeiro"))+
  ggtitle("Rio de Janeiro State")+
  xlab("Temperature (???C)")+ ylab("Wing length (mm)") +
  scale_y_continuous(breaks = c(0:6))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

#arrange wing plots
grid.arrange(wing.am, wing.ro, wing.to, wing.rj, ncol=2, nrow=2, top="Average Wing length (mm) by Temperature and Family")


##ANOVAS
#larvae
larv_mod_am<-lmer(sLL~Temp_let+(1|Fam_new), data=subset(adult, State=="Amazonas"))
anova(larv_mod_am) #both Treatment and State sig as well as interaction
difflsmeans(larv_mod_am, test.effs="Temp_let") #all temp comparisons significant
l.vc.am<-VarCorr(larv_mod)
print(l.vc.am, comp=c("Variance"))

##quick plot for family diff
adult.rj <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.AL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Rio de Janeiro"))+
  ggtitle("Rio de Janeiro State")+
  xlab("Temperature (C)")+ ylab("Adult time (days)") +
  scale_y_continuous(breaks = c(0:6))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

rio_fam<-subset(all_data, State=="Rio de Janeiro")
survival.fit2<-survfit(Surv(time)~Temp_num+Fam_new, data=rio_fam)
#summarize
riores_sum<- surv_summary(survival.fit2)
ggsurv<-ggsurvplot(survival.fit2, color="Temp_num", palette=c("#619CFF", "#00BA38", "#F8766D"), conf.int=TRUE, na.rm=TRUE)
ggsurv$plot +theme_bw() + facet_grid (Fam_new~.)
