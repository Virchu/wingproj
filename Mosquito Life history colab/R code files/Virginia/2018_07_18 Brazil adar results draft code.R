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
                                                                                                            mean.pup = mean(PL, na.rm=TRUE),
                                                                                                            sd.pup= sd(PL, na.rm=TRUE),
                                                                                                            N.pup=length(PL),
                                                                                                            se.pup=sd.pup/sqrt(N.pup),
                                                                                                            mean.em = mean(Emtime, na.rm=TRUE),
                                                                                                            sd.em= sd(Emtime, na.rm=TRUE),
                                                                                                            N.em=length(Emtime),
                                                                                                            se.em=sd.em/sqrt(N.em),
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
##Plots of data

#fig 3.A- larvae development state averages over temperature
larv.state.line<-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.sLL, group=interaction(Biome,Lat_group),colour=State, shape=Biome, linetype = Lat_group))+
  geom_line(aes(linetype = Lat_group),size=1,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_errorbar(aes(ymin=mean.sLL-se.sLL, ymax=mean.sLL+se.sLL),stat="summary", colour="black",linetype=1,size=.8,width=.6, position=position_dodge(0.5))+
  geom_point(colour="black", size=4,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_point(aes(shape = Biome), size=2.1, position=position_dodge(0.5), stat="summary", fun.y="mean")+
  ggtitle("Average larvae development (days)")+
  scale_y_continuous(breaks = c(13: 24))+ theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=10))+
  labs(x="Temperature (°C)", y="Larvae development (days)", linetype="Latitude group" )+
  scale_linetype_manual(values=c(1,2,3),labels=c("Low latitude", "Mid latitude", "High latitude"))


#adult life
#fig 3.B
adult.state.line<-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.AL, group=interaction(Biome,Lat_group),colour=State, shape=Biome, linetype = Lat_group))+
  geom_line(aes(linetype = Lat_group),size=1,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_errorbar(aes(ymin=mean.AL-se.AL, ymax=mean.AL+se.AL),stat="summary", colour="black",linetype=1,size=.8,width=.6, position=position_dodge(0.5))+
  geom_point(colour="black", size=4,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_point(aes(shape = Biome), size=2.1, position=position_dodge(0.5), stat="summary", fun.y="mean")+
  ggtitle("Average adult life (days)")+
  scale_y_continuous(breaks = c(1: 6))+ theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=10))+
  labs(x="Temperature (°C)", y="Adult time (days)", linetype="Latitude group" )+
  scale_linetype_manual(values=c(1,2,3),labels=c("Low latitude", "Mid latitude", "High latitude"))

#Supplement bit plots
#Pupal development
#
pup.state.line<-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.pup, group=interaction(Biome,Lat_group),colour=State, shape=Biome, linetype = Lat_group))+
  geom_line(aes(linetype = Lat_group),size=1,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_errorbar(aes(ymin=mean.pup-se.pup, ymax=mean.pup+se.pup),stat="summary", colour="black",linetype=1,size=.8,width=.6, position=position_dodge(0.5))+
  geom_point(colour="black", size=4,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_point(aes(shape = Biome), size=2.1, position=position_dodge(0.5), stat="summary", fun.y="mean")+
  ggtitle("Pupal development (days)")+ theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=10))+
  labs(x="Temperature (°C)", y="Pupal development (days)", linetype="Latitude group" )+
  scale_linetype_manual(values=c(1,2,3),labels=c("Low latitude", "Mid latitude", "High latitude"))

em.state.line<-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.em, group=interaction(Biome,Lat_group),colour=State, shape=Biome, linetype = Lat_group))+
  geom_line(aes(linetype = Lat_group),size=1,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_errorbar(aes(ymin=mean.em-se.em, ymax=mean.em+se.em),stat="summary", colour="black",linetype=1,size=.8,width=.6, position=position_dodge(0.5))+
  geom_point(colour="black", size=4,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_point(aes(shape = Biome), size=2.1, position=position_dodge(0.5), stat="summary", fun.y="mean")+
  ggtitle("Emergence time (days)")+ theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=10))+
  labs(x="Temperature (°C)", y="Emergence time (days)", linetype="Latitude group" )+
  scale_linetype_manual(values=c(1,2,3),labels=c("Low latitude", "Mid latitude", "High latitude"))

#Wing length
#fig 5.B
wing.state.line<-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.wing, group=interaction(Biome,Lat_group),colour=State, shape=Biome, linetype = Lat_group))+
  geom_line(aes(linetype = Lat_group),size=1,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_errorbar(aes(ymin=mean.wing-se.wing, ymax=mean.wing+se.wing),stat="summary", colour="black",linetype=1,size=.8,width=.6, position=position_dodge(0.5))+
  geom_point(colour="black", size=4,position=position_dodge(0.5), stat="summary", fun.y="mean")+
  geom_point(aes(shape = Biome), size=2.1, position=position_dodge(0.5), stat="summary", fun.y="mean")+
  ggtitle("Average wing length (mm)")+
  scale_y_continuous(breaks = c(2.5 , 3), limits=c(2.5,3))+ theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=10))+
  labs(x="Temperature (°C)", y="Wing length (mm)", linetype="Latitude group" )+
  scale_linetype_manual(values=c(1,2,3),labels=c("Low latitude", "Mid latitude", "High latitude"))

#Wing length
wing20<-subset(adult.sum.nosex, Temp_fac==20)
wing24<-subset(adult.sum.nosex, Temp_fac==24)
wing28<-subset(adult.sum.nosex, Temp_fac==28)
lm20<-lm(mean.wing~mean.Lat, data=subset(adult.sum.nosex, Temp_fac==20))
#lm 20 2.7075, -0.012230
#r2= .5527
wing.state.line.20<-
  ggplot(wing20, aes(x=abs(mean.Lat), y=mean.wing))+
  geom_errorbar(aes(ymin=mean.wing-se.wing, ymax=mean.wing+se.wing),stat="summary", colour="black",linetype=1,size=.8,width=.6)+
  geom_point(aes(colour=State), size=2, stat="summary", fun.y="mean")+
  ggtitle("Average wing length (mm) at 20C")+ theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=10))+
  labs(x="Latitude (S)", y="Wing length (mm)")+
  geom_smooth(colour="red",method="lm")
  #geom_abline( intercept = 2.707527,slope= -0.01223)

 # text(5, 2.8, "y=2.708-0.0122x, R^2=0.553")

lm24<-lm(mean.wing~mean.Lat, data=subset(adult.sum.nosex, Temp_fac==24))
#lm 20 2.576580, -.00951
#r2= .445
wing.state.line.24<-
  ggplot(wing24, aes(x=abs(mean.Lat), y=mean.wing))+
  geom_errorbar(aes(ymin=mean.wing-se.wing, ymax=mean.wing+se.wing),stat="summary", colour="black",linetype=1,size=.8,width=.6)+
  geom_point(aes(colour=State), size=2, stat="summary", fun.y="mean")+
  ggtitle("Average wing length (mm) at 24C")+ theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=10))+
  labs(x="Latitude (S)", y="Wing length (mm)")+
  geom_smooth(colour="red",method="lm")

 # geom_abline( intercept = 2.57658,slope= -0.00951)

lm28<-lm(mean.wing~mean.Lat, data=subset(adult.sum.nosex, Temp_fac==28))

#lm 20 2.493399, -.005682
#r2= .445
wing.state.line.28<-
  ggplot(wing28, aes(x=abs(mean.Lat), y=mean.wing))+
  geom_errorbar(aes(ymin=mean.wing-se.wing, ymax=mean.wing+se.wing),stat="summary", colour="black",linetype=1,size=.8,width=.6)+
  geom_point(aes(colour=State), size=2, stat="summary", fun.y="mean")+
  ggtitle("Average wing length (mm) at 28C")+ theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=10))+
  labs(x="Latitude (S)", y="Wing length (mm)")+
  geom_smooth(colour="red",method="lm")
  #geom_abline( intercept = 2.493399,slope= -0.005682)

#fig 5.A.
grid.arrange(wing.state.line.20, wing.state.line.24, wing.state.line.28, ncol=1, nrow=3)


####GLMM####
##GLMM models
#redid with state instead of pop for SPH poster
PQL_meth_sLL<- glmmPQL(sLL~Temp_fac*State, ~1|Locality/Family, family=gaussian(link="log"), data=adult, verbose=FALSE)
summary(PQL_meth_sLL)
#test1<-glmm()
#comparing interactions
lar_ls<- lsmeans(PQL_meth_sLL, pairwise ~Temp_fac:State, adjust="tukey")
lar_cld<- cld(lar_ls, alpha=0.5, Letters=letters, adjust="tukey", reverse=TRUE)

PQL_meth_al<- glmmPQL(AL~Temp_fac*State, ~1|Locality/Family, family=poisson, data=adult, verbose=FALSE)
summary(PQL_meth_al)
#comparing interactions
al_ls<- lsmeans(PQL_meth_al, pairwise ~Temp_fac:State, adjust="tukey")
al_cld<- cld(al_ls, alpha=0.5, Letters=letters, adjust="tukey", reverse=TRUE)

PQL_meth_wing<- glmmPQL(Wing.length..mm.~Temp_fac*State, ~1|Locality/Family, family=gaussian(link="log"), data=adult, verbose=FALSE)
summary(PQL_meth_wing)
#comparing interactions
wing_ls<- lsmeans(PQL_meth_wing, pairwise ~Temp_fac:State, adjust="tukey")
wing_cld<- cld(wing_ls, alpha=0.5, Letters=letters, adjust="tukey", reverse=TRUE)

PQL_meth_PL<- glmmPQL(PL~Temp_fac*State, ~1|Locality/Family, family=gaussian(link="log"), data=adult, verbose=FALSE)
summary(PQL_meth_PL)
#test1<-glmm()
#comparing interactions
pup_ls<- lsmeans(PQL_meth_PL, pairwise ~Temp_fac:State, adjust="tukey")
pup_cld<- cld(pup_ls, alpha=0.5, Letters=letters, adjust="tukey", reverse=TRUE)

PQL_meth_em<- glmmPQL(Emtime~Temp_fac*State, ~1|Locality/Family, family=gaussian(link="log"), data=adult, verbose=FALSE)
summary(PQL_meth_em)
#test1<-glmm()
#comparing interactions
em_ls<- lsmeans(PQL_meth_em, pairwise ~Temp_fac:State, adjust="tukey")
em_cld<- cld(em_ls, alpha=0.5, Letters=letters, adjust="tukey", reverse=TRUE)
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

pl.sum<-describeBy(adult$PL, list(adult$Temp_fac, adult$State),mat=TRUE, digits=2)
colnames(pl.sum)[3]<-"State"
colnames(pl.sum)[2]<-"Temp_fac"
pl.sum$State<-factor(pl.sum$State, levels= c("Amazonas", "Rondonia","Tocantins", "Rio de Janeiro" ))
pup_bar<-merge(pl.sum, pup_cld, by=c("State", "Temp_fac"), all.x=TRUE)

em.sum<-describeBy(adult$Emtime, list(adult$Temp_fac, adult$State),mat=TRUE, digits=2)
colnames(em.sum)[3]<-"State"
colnames(em.sum)[2]<-"Temp_fac"
em.sum$State<-factor(pl.sum$State, levels= c("Amazonas", "Rondonia","Tocantins", "Rio de Janeiro" ))
em_bar<-merge(em.sum, em_cld, by=c("State", "Temp_fac"), all.x=TRUE)


### bar graphs
#https://stackoverflow.com/questions/35090883/remove-all-of-x-axis-labels-in-ggplot
larv.hist.temp<-ggplot(lar_bar, aes(x=State, y=mean, fill=State, group=Temp_fac))+ 
  geom_point()+theme_minimal()+ geom_bar(aes(fill=State),stat="identity",colour="black")+
  scale_fill_manual("State", values = c("Amazonas" = "black", "Rondonia" = "dark grey", "Tocantins" = "light grey", "Rio de Janeiro" = "white"))+  
  facet_grid(.~Temp_fac, switch="x") +
  theme(plot.title = element_text(hjust = 0.5),strip.text.x = element_text(size = 16), axis.text.x = element_blank(), axis.title.x= element_blank(),axis.title.y= element_text(size=20),legend.text = element_text(size=15), axis.text = element_text(size = 15))+ 
  xlab("Temperature (C)")+ylab("Days as larvae")+ 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.5)+coord_cartesian(ylim = c(12, 25)) +labs (fill="State") +
  geom_text(aes(label=.group), position=position_dodge(width=1), vjust=-1.6)


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

pup.hist.temp<-ggplot(pup_bar, aes(x=State, y=mean, fill=State, group=Temp_fac))+
  geom_point()+theme_minimal()+ 
  geom_bar(aes(fill=State),stat="identity",colour="black")+
  scale_fill_manual("State", values = c("Amazonas" = "black", "Rondonia" = "dark grey", "Tocantins" = "light grey", "Rio de Janeiro" = "white"))+ 
  facet_grid(.~Temp_fac, switch="x")+
  theme(plot.title = element_text(hjust = 0.5),strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x= element_text(size=12),axis.title.y= element_text(size=12),legend.text = element_text(size=15), axis.text = element_text(size = 15))+ 
  xlab("State")+ylab("Pupal development time (days)")+ geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.5)+coord_cartesian(ylim = c(1, 3.5)) +
  labs (fill="State")+ geom_text(aes(label=.group), position=position_dodge(width=0.9), vjust=-0.8)

em.hist.temp<-ggplot(em_bar, aes(x=State, y=mean, fill=State, group=Temp_fac))+
  geom_point()+theme_minimal()+ 
  geom_bar(aes(fill=State),stat="identity",colour="black")+
  scale_fill_manual("State", values = c("Amazonas" = "black", "Rondonia" = "dark grey", "Tocantins" = "light grey", "Rio de Janeiro" = "white"))+ 
  facet_grid(.~Temp_fac, switch="x")+
  theme(plot.title = element_text(hjust = 0.5),strip.text.x = element_text(size = 12), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x= element_text(size=12),axis.title.y= element_text(size=12),legend.text = element_text(size=15), axis.text = element_text(size = 15))+ 
  xlab("State")+ylab("Emergence time (days)")+ geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.5)+coord_cartesian(ylim = c(12,28)) +
  labs (fill="State")+ geom_text(aes(label=.group), position=position_dodge(width=0.9), vjust=-0.8)
###Arrange line graphs for figures

ggarrange(larv.state.line, adult.state.line,labels= c("A","B"), ncol=2, nrow=1,  common.legend = TRUE, legend="right")
ggarrange(larv.state.line, adult.state.line, wing.state.line,labels= c("A","B", "C"), ncol=3, nrow=1,  common.legend = TRUE, legend="right")



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

grid.arrange(cor1,  larvadult.cor,wingadult.cor, larvwing.cor, ncol=2,nrow=2,top="Correlation between larvae development, wing length and adult lifespan")

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
survival.fit2<-survfit(Surv(Lifespan)~Temp_fac+State, data=adult)
ggsurv1<-ggsurvplot(survival.fit2,  conf.int=TRUE, color="Temp_fac", palette=c("#619CFF", "#00BA38", "#F8766D"),
                   title="Kaplan-Meier Survival",legend.title="Temperature (C)",
                   xlab= "Time (days)",
                   font.main= c(20, "bold"),
                   font.x= 15,
                   font.y=15,
                   risk.table = "abs_pct")
ggsurv1$plot +theme_bw() + facet_grid (State~.) +theme(plot.title = element_text(hjust = 0.5))
#risk.table
survtab<-ggsurv1$data.survplot
cumsurvtab<-ggsurv1$data.survtable
#fit cox model
cox<- coxph(Surv(Lifespan)~Temp_fac+State, data=adult)
summary(cox)

cox1<- coxph(Surv(Lifespan)~Temp_num*Latitude, data=adult)
summary(cox1)

