###Results for life history adar- region

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
library(stargazer)
##Datafiles
#complete data set
all_data <- read.csv("C:\\Users\\vmc04\\Documents\\GitHub\\wingproj\\Mosquito Life history colab\\Data files\\2018_03_28 Brazil adar16 Life history full.csv")
#add survival prob
#all_data$surv<- 1-(1/all_data$AL)
#create dataframe of survived to adult data
adult<-subset(all_data, all_data$Death_stat==1 & !is.na(all_data$Sex1))
#field wing data
field_wing<-read.csv("C:\\Users\\vmc04\\Documents\\GitHub\\wingproj\\Mosquito Life history colab\\Data files\\2018_04_20 Field wing length 13 18 cs scaled.csv")

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
##Plots of data

#Larvae development#
#sup a.1- larvae development fam averages by state over temperature
larv.am <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.sLL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Amazonas"))+
  ggtitle("Amazonas State")+
  xlab("Temperature (???C)")+ ylab("Larvae time (days)") +
  scale_y_continuous(breaks = c(13: 24))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

larv.ro <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.sLL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Rondonia"))+
  ggtitle("Rondonia State")+
  xlab("Temperature (???C)")+ ylab("Larvae time (days)") +
  scale_y_continuous(breaks = c(13: 24))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

larv.to <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.sLL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Tocantins"))+
  ggtitle("Tocantins State")+
  xlab("Temperature (???C)")+ ylab("Larvae time (days)") +
  scale_y_continuous(breaks = c(13: 24))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

larv.rj <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.sLL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Rio de Janeiro"))+
  ggtitle("Rio de Janeiro State")+
  xlab("Temperature (???C)")+ ylab("Larvae time (days)") +
  scale_y_continuous(breaks = c(13: 24))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

#arrange plots
grid.arrange(larv.am, larv.ro, larv.to, larv.rj, ncol=2, nrow=2, top="Average larvae development time (days) by Temperature and Family")

#fig a.1- larvae development state averages over temperature
larv.state.line<-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.sLL, group=interaction(Biome,Lat_group),colour=State, shape=Biome, linetype = Lat_group))+
  geom_line(aes(linetype = Lat_group),size=1,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_errorbar(aes(ymin=mean.sLL-se.sLL, ymax=mean.sLL+se.sLL),stat="summary", colour="black",linetype=1,size=.8,width=.6, position=position_dodge(0.5))+
  geom_point(colour="black", size=4,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_point(aes(shape = Biome), size=2.1, position=position_dodge(0.5), stat="summary", fun.y="mean")+
  ggtitle("Average larvae development (days)")+
  scale_y_continuous(breaks = c(13: 24))+ theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=10))+
  labs(x="Temperature (°C)", y="Larvae development (days)", linetype="Latitude group" )

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

#adult life
#fig b.1
adult.state.line<-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.AL, group=interaction(Biome,Lat_group),colour=State, shape=Biome, linetype = Lat_group))+
  geom_line(aes(linetype = Lat_group),size=1,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_errorbar(aes(ymin=mean.AL-se.AL, ymax=mean.AL+se.AL),stat="summary", colour="black",linetype=1,size=.8,width=.6, position=position_dodge(0.5))+
  geom_point(colour="black", size=4,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_point(aes(shape = Biome), size=2.1, position=position_dodge(0.5), stat="summary", fun.y="mean")+
  ggtitle("Average adult life (days)")+
  scale_y_continuous(breaks = c(1: 6))+ theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=10))+
  labs(x="Temperature (°C)", y="Adult time (days)", linetype="Latitude group" )

#Wing length#
wing.state.line<-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.wing, group=interaction(Biome,Lat_group),colour=State, shape=Biome, linetype = Lat_group))+
  geom_line(aes(linetype = Lat_group),size=1,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_errorbar(aes(ymin=mean.wing-se.wing, ymax=mean.wing+se.wing),stat="summary", colour="black",linetype=1,size=.8,width=.6, position=position_dodge(0.5))+
  geom_point(colour="black", size=4,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_point(aes(shape = Biome), size=2.1, position=position_dodge(0.5), stat="summary", fun.y="mean")+
  ggtitle("Average wing length (mm)")+
  scale_y_continuous(breaks = c(2.5: 3.1))+ theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=10))+
  labs(x="Temperature (°C)", y="Wing length (mm)", linetype="Latitude group" )
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
##prob survival
surv.state.line<-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=surv.fam, group=interaction(Biome,Lat_group),colour=State, shape=Biome, linetype = Lat_group))+
  geom_line(aes(linetype = Lat_group),size=1,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  #geom_errorbar(aes(ymin=mean.surv-se.surv, ymax=mean.surv+se.surv),stat="summary", colour="black",linetype=1,size=.8,width=.6, position=position_dodge(0.5))+
  geom_point(colour="black", size=4,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_point(aes(shape = Biome), size=2.1, position=position_dodge(0.5), stat="summary", fun.y="mean")+
  ggtitle("Average probability of survival")+
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=10))+
  labs(x="Temperature (???C)", y="Probability of survival", linetype="Latitude group" )
#arrange plots
grid.arrange(wing.am, wing.ro, wing.to, wing.rj, ncol=2, nrow=2, top="Average Wing length (mm) by Temperature and Family")


####bar graphs####
library(psych)
larvp.sum<-describeBy(adult$sLL, list(adult$Temp_fac, adult$State),mat=TRUE, digits=2)
colnames(larvp.sum)[3]<-"State"
colnames(larvp.sum)[2]<-"Temp_fac"
larvp.sum$State<-factor(larvp.sum$State, levels= c("Amazonas", "Rondonia","Tocantins", "Rio de Janeiro" ))
lar_bar<-merge(larvp.sum, lar_cld, by=c("State", "Temp_fac"), all.x=TRUE)

al.sum<-describeBy(adult$AL, list(adult$Temp_fac, adult$State),mat=TRUE, digits=2)
colnames(al.sum)[3]<-"State"
colnames(al.sum)[2]<-"Temp_fac"
al.sum$State<-factor(al.sum$State, levels= c("Amazonas", "Rondonia","Tocantins", "Rio de Janeiro" ))
al_bar<-merge(al.sum, al_cld, by=c("State", "Temp_fac"), all.x=TRUE)

wing.sum<-describeBy(adult$Wing.length..mm., list(adult$Temp_fac, adult$State),mat=TRUE, digits=2)
colnames(wing.sum)[3]<-"State"
colnames(wing.sum)[2]<-"Temp_fac"
wing.sum$State<-factor(wing.sum$State, levels= c("Amazonas", "Rondonia","Tocantins", "Rio de Janeiro" ))
wing_bar<-merge(wing.sum, wing_cld, by=c("State", "Temp_fac"), all.x=TRUE)


### bar graphs
#https://stackoverflow.com/questions/35090883/remove-all-of-x-axis-labels-in-ggplot
larv.hist.temp<-ggplot(lar_bar, aes(x=State, y=mean, fill=State, group=Temp_fac))+ 
  geom_point()+theme_minimal()+ geom_bar(aes(fill=State),stat="identity",colour="black")+
  scale_fill_manual("State", values = c("Amazonas" = "black", "Rondonia" = "dark grey", "Tocantins" = "light grey", "Rio de Janeiro" = "white"))+  
  facet_grid(.~Temp_fac, switch="x") +
  theme(plot.title = element_text(hjust = 0.5),strip.text.x = element_text(size = 16), axis.text.x = element_blank(), axis.title.x= element_blank(),axis.title.y= element_text(size=20),legend.text = element_text(size=15), axis.text = element_text(size = 15))+ 
  xlab("Temperature (C)")+ylab("Days as larvae")+ 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.5)+coord_cartesian(ylim = c(12, 25)) +labs (fill="State") +
  geom_text(aes(label=.group), position=position_dodge(width=0.9), vjust=-0.9)

al.hist.temp<-ggplot(al_bar, aes(x=State, y=mean, fill=State, group=Temp_fac))+ 
  geom_point()+theme_minimal()+ geom_bar(aes(fill=State),stat="identity",colour="black")+
  scale_fill_manual("State", values = c("Amazonas" = "black", "Rondonia" = "dark grey", "Tocantins" = "light grey", "Rio de Janeiro" = "white"))+  
  facet_grid(.~Temp_fac, switch="x")+
  theme(plot.title = element_text(hjust = 0.5),strip.text.x = element_text(size = 16), axis.text.x = element_blank(), axis.title.x= element_blank(),axis.title.y= element_text(size=20),legend.text = element_text(size=15), axis.text = element_text(size = 15))+ 
  xlab("Temperature (C)")+ylab("Adult life (days)")+ geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.5)+
  coord_cartesian(ylim = c(0, 5)) +labs (fill="State")+ 
  geom_text(aes(label=.group), position=position_dodge(width=0.9), vjust=-0.8)

wing.hist.temp<-ggplot(wing_bar, aes(x=State, y=mean, fill=State, group=Temp_fac))+
  geom_point()+theme_minimal()+ 
  geom_bar(aes(fill=State),stat="identity",colour="black")+
  scale_fill_manual("State", values = c("Amazonas" = "black", "Rondonia" = "dark grey", "Tocantins" = "light grey", "Rio de Janeiro" = "white"))+ 
  facet_grid(.~Temp_fac, switch="x")+
  theme(plot.title = element_text(hjust = 0.5),strip.text.x = element_text(size = 16), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x= element_text(size=18),axis.title.y= element_text(size=20),legend.text = element_text(size=15), axis.text = element_text(size = 15))+ 
  xlab("State")+ylab("Wing length (mm)")+ geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.5)+coord_cartesian(ylim = c(2.5, 3.1)) +
  labs (fill="State")+ geom_text(aes(label=.group), position=position_dodge(width=0.9), vjust=-0.8)

###Arrange line graphs for figure 1
#line.leg<- get_legend(larv.state.line  + theme(legend.position="right"))
#fig.1<-plot_grid(larv.state.line +theme(legend.position="none"), 
              #   adult.state.line +theme(legend.position="none"),
              #   align="vh",
              #   labels= c("A","B"),
              #   hjust=-1,
               #  nrow=1)
#fig.1.leg<-plot_grid(fig.1, line.leg, ncol=1, rel_heights=c(1,.2))
ggarrange(larv.state.line, adult.state.line,labels= c("A","B"), ncol=2, nrow=1,  common.legend = TRUE, legend="right")
ggarrange(larv.state.line, adult.state.line, wing.state.line,labels= c("A","B", "C"), ncol=3, nrow=1,  common.legend = TRUE, legend="right")

#grobs... did not work
larv.hist.temp.g<-ggplotGrob(larv.hist.temp)
al.hist.temp.g<-ggplotGrob(al.hist.temp)
ggarrange(larv.hist.temp, al.hist.temp,labels= c("A","B"), ncol=2, nrow=1,  common.legend = TRUE, legend="bottom")

#wing 
ggarrange(wing)
##GLMM models
#redid with state instead of pop for SPH poster
PQL_meth_sLL<- glmmPQL(sLL~Temp_fac*State, ~1|Locality/Family, family=gaussian(link="log"), data=adult, verbose=FALSE)
summary(PQL_meth_sLL)
#test1<-glmm()
#comparing interactions
lar_ls<- lsmeans(PQL_meth_sLL, pairwise ~Temp_fac:State, adjust="tukey")
lar_cld<- cld(lar_ls, alpha=0.5, Letters=letters, adjust="tukey")


#pretty table for sLL
stargazer(PQL_meth_sLL,  title= "GLMM results for temperature and state on larvae development", digits=2, align=TRUE)
texreg(PQL_meth_sLL)
larv_tab<- summary(PQL_meth_sLL)
pander(PQL_meth_sLL)
pandoc.table(larv_tab,style = 'rmarkdown')

pander(relabel(mtable(PQL_meth_sLL,
         summary.stats=c('N', 'R-squared', 'F')),
  '(Intercept)'       = 'Constant',
  'hp'                = 'Gross horsepower',
  'factor(gear): 4/3' = 'Four forward gears',
  'factor(gear): 5/3' = 'Five forward gears',
  'N'                 = 'Observations'))

PQL_meth_al<- glmmPQL(AL~Temp_fac*State, ~1|Locality/Family, family=poisson, data=adult, verbose=FALSE)
summary(PQL_meth_al)
#comparing interactions
al_ls<- lsmeans(PQL_meth_al, pairwise ~Temp_fac:State, adjust="tukey")
al_cld<- cld(al_ls, alpha=0.5, Letters=letters, adjust="tukey")

PQL_meth_wing<- glmmPQL(Wing.length..mm.~Temp_fac*State, ~1|Locality/Family, family=gaussian(link="log"), data=adult, verbose=FALSE)
summary(PQL_meth_wing)
#comparing interactions
wing_ls<- lsmeans(PQL_meth_wing, pairwise ~Temp_fac:State, adjust="tukey")
wing_cld<- cld(wing_ls, alpha=0.5, Letters=letters, adjust="tukey")

##ANOVA tables for gen var, phen plast and G by E of traits
#overall
#Larvae
larv_mod<-lmer(sLL~Temp_let*State+(1|Fam_new)+(1|State:Temp_let:Fam_new), data=adult)
anova(larv_mod) #both Treatment and State sig as well as interaction
difflsmeans(larv_mod, test.effs="State") #all state comparisons except rio and tocantins significant
difflsmeans(larv_mod, test.effs="Temp_let") #all temp comparisons significant
l.vc<-VarCorr(larv_mod)
print(l.vc, comp=c("Variance"))
#adult
adult_mod<-lmer(AL~Temp_let*State+(1|Fam_new)+(1|State:Temp_let:Fam_new), data=adult)
anova(adult_mod) #both Treatment and State sig as well as interaction
rand(adult_mod)#get random effects table
difflsmeans(adult_mod, test.effs="State") #all state comparisons except rio and tocantins significant
difflsmeans(adult_mod, test.effs="Temp_let") #all temp comparisons significant

#wing length
wing_mod<-lmer(Wing.length..mm.~Temp_let*State+(1|Fam_new)+(1|State:Temp_let:Fam_new), data=adult)
anova(wing_mod) #both Treatment and State sig as well as interaction
rand(wing_mod)#get random effects table
difflsmeans(wing_mod, test.effs="State") #Wing.length..mm.all state comparisons except amazonas and rondonia significant
difflsmeans(wing_mod, test.effs="Temp_let") #Wing.length..mm.all temp comparisons significant

##stargazer anova tables
stargazer(larv_mod, adult_mod, wing_mod, type="text", title= "ANOVA tables across states", digits=2)
surv_mod<-lmer(surv~Temp_let*State+(1|Fam_new)+(1|State:Temp_let:Fam_new), data=adult)
anova(surv_mod) #both Treatment and State sig as well as interaction
rand(surv_mod)#get random effects table
difflsmeans(surv_mod, test.effs="State") #Wing.length..mm.all state comparisons except amazonas and rondonia significant
difflsmeans(surv_mod, test.effs="Temp_let") #Wing.length..mm.all temp comparisons significant
###ANOVAs by population for comparison

###ANOVAS with sex for supplemental
#male=1
larv_mod1<-lmer(sLL~Temp_let*State*Sex1+(1|Fam_new)+(1|State:Temp_let:Fam_new), data=adult)
anova(larv_mod1) #both Treatment and State sig as well as interaction

adult_mod1<-lmer(AL~Temp_let*State*Sex1+(1|Fam_new)+(1|State:Temp_let:Fam_new), data=adult)
anova(adult_mod1)

wing_mod1<-lmer(Wing.length..mm.~Temp_let*State*Sex1+(1|Fam_new)+(1|State:Temp_let:Fam_new), data=adult)
anova(wing_mod1)


### Correlations: 
#added avg. surv (11), debating removing it...
adult.sum.corsub1<-adult.sum.corsub[c(1,14,7,15,4,12,13,8:11)]
names(adult.sum.corsub1)<-c("Family", "Biome", "Latitude", "State", "Locality","Temp", "Sex", "Avg.Larvae", "Avg.Adult", "Avg.Wing", "Avg.Surv")
ggcorr(adult.sum.corsub1,
       label=TRUE,
       label_alpha=0.75,
       hjust=0.4)

#smaller graph
#removed sex (7) and surv (11)
cor1<-ggcorr(adult.sum.corsub1[,c(4,6, 8:10)],
       label=TRUE,
       label_alpha=0.75,
       hjust=0.65,
       layout.exp = 1)

#larvae and wing correlation plot
larwing<-lm(adult.sum.corsub1$Avg.Wing~adult.sum.corsub1$Avg.Larvae)
larvwing.cor <-
  ggplot(adult.sum.corsub, aes(x=mean.sLL, y=mean.wing,group=Temp_fac))+
  geom_point(aes(colour=Temp_fac))+
  geom_smooth(method='lm')+
  geom_abline(colour="black", size=1.5, alpha=0.5, intercept = 1.985, slope = 0.03894)+
  #ggtitle("Larvae development and Wing length")+
  xlab("Average larvae development (days)")+ ylab("Average wing length (mm)") +
  theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

#larvae and adult life correlation plot
laradult<-lm(adult.sum.corsub1$Avg.Adult~adult.sum.corsub1$Avg.Larvae)
larvadult.cor <-
  ggplot(adult.sum.corsub, aes(x=mean.sLL, y=mean.AL,group=Temp_fac))+
  geom_point(aes(colour=Temp_fac))+
  geom_smooth(method='lm')+
  geom_abline(colour="black", size=1.5, alpha=0.5, intercept = -1.2521, slope = 0.2394)+
  #ggtitle("Larvae development and Adult lifespan")+
  xlab("Average larvae development (days)")+ ylab("Average adult life (days)") +
  theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

#wing and adult life correlation plot
wingadult<-lm(adult.sum.corsub1$Avg.Adult~adult.sum.corsub1$Avg.Wing)
wingadult.cor <-
  ggplot(adult.sum.corsub, aes(x=mean.wing, y=mean.AL,group=Temp_fac))+
  geom_point(aes(colour=Temp_fac))+
  geom_smooth(method='lm')+
  geom_abline(colour="black", size=1.5, alpha=0.5, intercept = -10.62, slope = 5.089)+
  #ggtitle("Wing length and Adult lifespan")+
  xlab("Average wing length (mm)")+ ylab("Average adult life (days)") +
  theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

grid.arrange(cor1,  wingadult.cor,larvadult.cor, larvwing.cor, ncol=2,nrow=2,top="Correlation between larvae development, wing length and adult lifespan")


##by state instead of temp
larvwing.cor1 <-
  ggplot(adult.sum.corsub, aes(x=mean.sLL, y=mean.wing,group=State))+
  geom_point(aes(colour=State))+
  geom_smooth(method='lm')+
  geom_abline(colour="black", size=1.5, alpha=0.5, intercept = 1.985, slope = 0.03894)+
  #ggtitle("Larvae development and Wing length")+
  xlab("Average larvae development (days)")+ ylab("Average wing length (mm)") +
  theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

#larvae and adult life correlation plot
laradult<-lm(adult.sum.corsub1$Avg.Adult~adult.sum.corsub1$Avg.Larvae)
larvadult.cor1 <-
  ggplot(adult.sum.corsub, aes(x=mean.sLL, y=mean.AL,group=State))+
  geom_point(aes(colour=State))+
  geom_smooth(method='lm')+
  geom_abline(colour="black", size=1.5, alpha=0.5, intercept = -1.2521, slope = 0.2394)+
  #ggtitle("Larvae development and Adult lifespan")+
  xlab("Average larvae development (days)")+ ylab("Average adult life (days)") +
  theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

#wing and adult life correlation plot
wingadult<-lm(adult.sum.corsub1$Avg.Adult~adult.sum.corsub1$Avg.Wing)
wingadult.cor1 <-
  ggplot(adult.sum.corsub, aes(x=mean.wing, y=mean.AL,group=State))+
  geom_point(aes(colour=State))+
  geom_smooth(method='lm')+
  geom_abline(colour="black", size=1.5, alpha=0.5, intercept = -10.62, slope = 5.089)+
  #ggtitle("Wing length and Adult lifespan")+
  xlab("Average wing length (mm)")+ ylab("Average adult life (days)") +
  theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

grid.arrange(cor1,  wingadult.cor1,larvadult.cor1, larvwing.cor1, ncol=2,nrow=2,top="Correlation between larvae development, wing length and adult lifespan by state")


###survival

survival.fit1<-survfit(Surv(time)~Temp_fac+State, data=adult)
ggsurv<-ggsurvplot(survival.fit1,  conf.int=TRUE, color="Temp_fac", palette=c("#619CFF", "#00BA38", "#F8766D"),
                   title="Kaplan-Meier Survival",legend.title="Temperature (C)",
                   xlab= "Time (days)",
                   font.main= c(20, "bold"),
                   font.x= 15,
                   font.y=15,
                   risk.table = "abs_pct")
ggsurv$plot +theme_bw() + facet_grid (State~.) +theme(plot.title = element_text(hjust = 0.5))
#risk.table
survtab<-ggsurv$data.survplot
cumsurvtab<-ggsurv$data.survtable
#fit cox model
cox<- coxph(Surv(time)~Temp_fac+State, data=adult)
summary(cox)

cox1<- coxph(Surv(time)~Temp_num*Latitude, data=adult)
summary(cox1)

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
