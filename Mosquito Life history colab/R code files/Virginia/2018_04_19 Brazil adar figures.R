#updated life history figures
#require(Rcpp)

library(dplyr)
library(ggplot2)
library(lme4)
library(raster)
library(cowplot)
library(car)
library(rstan)
#devtools::install_github("paul-buerkner/brms")
#read in and check data
#adult <- read.csv("C:\\Users\\Tim\\Desktop\\mosquito\\2018_03_07 Life history adult data.csv")
all_data <- read.csv("C:\\Users\\virgc\\GitHub\\wingproj\\Mosquito Life history colab\\Data files\\2018_03_28 Brazil adar16 Life history full.csv")
adult<-read.csv("C:\\Users\\virgc\\GitHub\\wingproj\\Mosquito Life history colab\\Data files\\2018_03_07 Life history adult data.csv")
##work
#all_data <- read.csv("C:\\Users\\vmc04\\Documents\\GitHub\\wingproj\\Mosquito Life history colab\\Data files\\2018_03_28 Brazil adar16 Life history full.csv")
#adult<-read.csv("C:\\Users\\vmc04\\Documents\\GitHub\\wingproj\\Mosquito Life history colab\\Data files\\2018_03_07 Life history adult data.csv")

all_data$State<-factor(all_data$State, levels=c("Amazonas", "Rondonia", "Tocantins", "Rio de Janeiro"))
all_data$Locality<-factor(all_data$Locality, levels=c("ARS", "APR", "RPV", "RMO","TLC","TPN", "SJU"))
adult$State<-factor(adult$State, levels=c("Amazonas", "Rondonia", "Tocantins", "Rio de Janeiro"))
adult$Locality<-factor(adult$Locality, levels=c("ARS", "APR", "RPV", "RMO","TLC","TPN", "SJU"))


#all_data (n=3430) with separate sexes
all.sum <- all_data %>% group_by(Fam_new, Biome, State, Locality, Sex, Lat_group) %>% summarize(mean.sLL = mean(sLL, na.rm = TRUE), 
                                                                                                sd.sLL= sd(sLL, na.rm=TRUE),
                                                                                                N.sLL=length(sLL),
                                                                                                se.sLL=sd.sLL/sqrt(N.sLL),
                                                                                                mean.surv= mean(Death_stat, na.rm=TRUE),
                                                                                                mean.Temp= mean(Temp_num, na.rm=TRUE),
                                                                                                mean.Lat=mean(Latitude, na.rm=TRUE)) %>% data.frame
#all_data (n=3430) grouping sexes
all.sum.nosex <- all_data %>% group_by(Fam_new, Biome, State, Locality, Lat_group) %>% summarize(mean.sLL = mean(sLL, na.rm = TRUE),
                                                                                                           mean.surv= mean(Death_stat, na.rm=TRUE),
                                                                                                           mean.Temp = mean(Temp_num, na.rm=TRUE),
                                                                                                           mean.Lat=mean(Latitude, na.rm=TRUE)) %>% data.frame

#adult (n=2652) with separate sexes
adult.sum <- adult %>% group_by(Fam_new, Biome, State, Locality, Sex, Temp_fac, Lat_grouple) %>% summarize(mean.sLL = mean(sLL, na.rm = TRUE), 
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
                                                                                                         mean.Lat = mean(Latitude, na.rm = TRUE)) %>% data.frame
                                                                                                      
#adult (n=2652)grouping sexes
adult.sum.nosex <- adult %>% group_by(Fam_new, Biome, State, Locality, Temp_fac, Lat_grouple) %>% summarize(mean.sLL = mean(sLL, na.rm = TRUE),
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
                                                                                                          mean.Lat = mean(Latitude, na.rm = TRUE)) %>% data.frame
adult.sum.nosex$State<-factor(adult.sum.nosex$State, levels=c("Amazonas", "Rondonia", "Tocantins", "Rio de Janeiro"))

larv.state.line<-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.sLL, group=interaction(Biome,Lat_grouple),colour=State, shape=Biome, linetype = Lat_grouple))+
  geom_line(aes(linetype = Lat_grouple),size=1,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_errorbar(aes(ymin=mean.sLL-se.sLL, ymax=mean.sLL+se.sLL),stat="summary", colour="black",linetype=1,size=.8,width=.6, position=position_dodge(0.5))+
  geom_point(colour="black", size=4,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_point(aes(shape = Biome), size=2.1, position=position_dodge(0.5), stat="summary", fun.y="mean")+
  ggtitle("Average larvae development by temperature and state")+
  scale_y_continuous(breaks = c(13: 24))+ theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=10))+
  scale_x_discrete(limit = c(20, 24,28))+
  labs(x="Temperature (C)", y="Larvae development (days)", linetype="Latitude group" )

larv.state.line1<-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.sLL, group=State,colour=State))+
  stat_summary(fun.y=mean, 
               fun.ymin = function(x) mean(x) - sd(x), 
               fun.ymax = function(x) mean(x) + sd(x),
               geom="pointrange")+
  stat_summary(fun.y = mean,
               geom = "line")+
  ggtitle("Average larvae time (day) \n by Temperature and State")+
  xlab("Temperature (C)")+ ylab("Larvae time (days)") +
  scale_y_continuous(breaks = c(13: 24))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=10))


larv.locality.line<-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.sLL, group=Locality,colour=Locality))+
  stat_summary(fun.y=mean, 
               fun.ymin = function(x) mean(x) - sd(x), 
               fun.ymax = function(x) mean(x) + sd(x),
               geom="pointrange")+
  stat_summary(fun.y = mean,
               geom = "line")+
  #geom_point(aes(shape =State))+
  #geom_path(aes(color = State))+
  #geom_errorbar(aes(ymin=mean.sLL-se.sLL, ymax=mean.sLL+se.sLL))+
  ggtitle("Average larvae time (day) \n by Temperature and Locality")+
  xlab("Temperature (C)")+ ylab("Larvae time (days)") +
  scale_y_continuous(breaks = c(13: 24))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=10))
                                                                                                     
#adult life
adult.state.line<-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.AL, group=interaction(Biome,Lat_grouple),colour=State, shape=Biome, linetype = Lat_grouple))+
  geom_line(aes(linetype = Lat_grouple),size=1,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_errorbar(aes(ymin=mean.AL-se.AL, ymax=mean.AL+se.AL),stat="summary", colour="black",linetype=1,size=.8,width=.6, position=position_dodge(0.5))+
  geom_point(colour="black", size=4,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_point(aes(shape = Biome), size=2.1, position=position_dodge(0.5), stat="summary", fun.y="mean")+
  ggtitle("Average adult life by temperature and state")+
  scale_y_continuous(breaks = c(1: 6))+ theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=10))+
  scale_x_discrete(limit = c(20, 24,28))+
  labs(x="Temperature (C)", y="Adult time (days)", linetype="Latitude group" )

#wing length
wing.state.line<-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.wing, group=interaction(Biome,Lat_grouple),colour=State, shape=Biome, linetype = Lat_grouple))+
  geom_line(aes(linetype = Lat_grouple),size=1,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_errorbar(aes(ymin=mean.wing-se.wing, ymax=mean.wing+se.wing),stat="summary", colour="black",linetype=1,size=.8,width=.6, position=position_dodge(0.5))+
  geom_point(colour="black", size=4,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_point(aes(shape = Biome), size=2.1, position=position_dodge(0.5), stat="summary", fun.y="mean")+
  ggtitle("Average wing length by temperature and state")+
  #scale_y_continuous(breaks = c(1: 6))+ theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=10))+
  scale_x_discrete(limit = c(20, 24,28))+
  labs(x="Temperature (C)", y="Adult time (days)", linetype="Latitude group" )

