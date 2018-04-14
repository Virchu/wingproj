#updated life history figures
library(brms)
library(dplyr)
library(ggplot2)
library(lme4)
library(raster)
library(cowplot)
library(car)
library(Rcpp)
#read in and check data
#adult <- read.csv("C:\\Users\\Tim\\Desktop\\mosquito\\2018_03_07 Life history adult data.csv")
all_data <- read.csv("C:\\Users\\virgc\\Documents\\GitHub\\wingproj\\Mosquito Life history colab\\Data files\\2018_03_28 Brazil adar16 Life history full.csv")
adult<-read.csv("C:\\Users\\virgc\\Documents\\GitHub\\wingproj\\Mosquito Life history colab\\Data files\\2018_03_07 Life history adult data.csv")
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
                                                                                                     

