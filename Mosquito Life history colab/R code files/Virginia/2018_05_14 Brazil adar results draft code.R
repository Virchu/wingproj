###Results for life history adar- region

##Install libraries
library(lme4)
library(lmerTest)
library(multcomp)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(MASS)

##Datafiles
#complete data set
all_data <- read.csv("C:\\Users\\virgc\\Documents\\GitHub\\wingproj\\Mosquito Life history colab\\Data files\\2018_03_28 Brazil adar16 Life history full.csv")
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
all.sum.nosex <- all_data %>% group_by(Fam_new, Biome, State, Locality, Lat_group) %>% summarize(mean.sLL = mean(sLL, na.rm = TRUE),
                                                                                                 mean.surv= mean(Death_stat, na.rm=TRUE),
                                                                                                 mean.Temp = mean(Temp_num, na.rm=TRUE),
                                                                                                 mean.Lat=mean(Latitude, na.rm=TRUE)) %>% data.frame

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
                                                                                                           mean.Temp = mean(Temp_num), na.rm = TRUE, 
                                                                                                           mean.Lat = mean(Latitude, na.rm = TRUE)) %>% data.frame

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
                                                                                                            mean.Lat = mean(Latitude, na.rm = TRUE)) %>% data.frame
adult.sum.nosex$State<-factor(adult.sum.nosex$State, levels=c("Amazonas", "Rondonia", "Tocantins", "Rio de Janeiro"))
adult.sum.nosex$Lat_group<-factor(adult.sum.nosex$Lat_group, levels=c(1,2,3))

field.sum <- field_wing %>% group_by(Biome, State, Latitude, Lat_grouple) %>%
  summarize (mean.wing=mean(Length.mm, na.rm=TRUE),
             sd.wing= sd(Length.mm, na.rm=TRUE),
             N.wing=length(Length.mm),   
             se.wing=sd.wing/sqrt(N.wing)) %>% data.frame                                                                                                
field.sum$State<-factor(field.sum$State, levels=c("Amazonas", "Rondonia", "Tocantins", "Rio de Janeiro"))
##Plots of data

#Larvae development#
#sup a.1- larvae development fam averages by state over temperature
larv.am <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.sLL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Amazonas"))+
  ggtitle("Amazonas State")+
  xlab("Temperature (C)")+ ylab("Larvae time (days)") +
  scale_y_continuous(breaks = c(13: 24))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

larv.ro <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.sLL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Rondonia"))+
  ggtitle("Rondonia State")+
  xlab("Temperature (C)")+ ylab("Larvae time (days)") +
  scale_y_continuous(breaks = c(13: 24))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

larv.to <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.sLL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Tocantins"))+
  ggtitle("Tocantins State")+
  xlab("Temperature (C)")+ ylab("Larvae time (days)") +
  scale_y_continuous(breaks = c(13: 24))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

larv.rj <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.sLL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Rio de Janeiro"))+
  ggtitle("Rio de Janeiro State")+
  xlab("Temperature (C)")+ ylab("Larvae time (days)") +
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
  ggtitle("Average larvae development by temperature and state")+
  scale_y_continuous(breaks = c(13: 24))+ theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=10))+
  labs(x="Temperature (C)", y="Larvae development (days)", linetype="Latitude group" )

#Adult longevity#
#sup b.1
adult.am <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.AL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Amazonas"))+
  ggtitle("Amazonas State")+
  xlab("Temperature (C)")+ ylab("Adult time (days)") +
  scale_y_continuous(breaks = c(0:6))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

adult.ro <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.AL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Rondonia"))+
  ggtitle("Rondonia State")+
  xlab("Temperature (C)")+ ylab("Adult time (days)") +
  scale_y_continuous(breaks = c(0:6))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

adult.to <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.AL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Tocantins"))+
  ggtitle("Tocantins State")+
  xlab("Temperature (C)")+ ylab("Adult time (days)") +
  scale_y_continuous(breaks = c(0:6))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

adult.rj <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.AL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Rio de Janeiro"))+
  ggtitle("Rio de Janeiro State")+
  xlab("Temperature (C)")+ ylab("Adult time (days)") +
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
  ggtitle("Average adult life by temperature and state")+
  scale_y_continuous(breaks = c(1: 6))+ theme_classic() +
  theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=10))+
  scale_x_discrete(limit = c(20, 24,28))+
  labs(x="Temperature (C)", y="Adult time (days)", linetype="Latitude group" )

#Wing length#
#sup c.1
wing.am <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.AL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Amazonas"))+
  ggtitle("Amazonas State")+
  xlab("Temperature (C)")+ ylab("Wing length (mm)") +
  scale_y_continuous(breaks = c(0:6))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

wing.ro <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.AL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Rondonia"))+
  ggtitle("Rondonia State")+
  xlab("Temperature (C)")+ ylab("Wing length (mm)") +
  scale_y_continuous(breaks = c(0:6))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

wing.to <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.AL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Tocantins"))+
  ggtitle("Tocantins State")+
  xlab("Temperature (C)")+ ylab("Wing length (mm)") +
  scale_y_continuous(breaks = c(0:6))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

wing.rj <-
  ggplot(adult.sum.nosex, aes(x=Temp_fac, y=mean.AL, group=Fam_new))+
  geom_line(aes(colour=Fam_new), subset(adult.sum.nosex, State=="Rio de Janeiro"))+
  ggtitle("Rio de Janeiro State")+
  xlab("Temperature (C)")+ ylab("Wing length (mm)") +
  scale_y_continuous(breaks = c(0:6))+ theme_classic() +theme(plot.title = element_text(hjust = 0.5), legend.text = element_text(size=7))

#arrange plots
grid.arrange(wing.am, wing.ro, wing.to, wing.rj, ncol=2, nrow=2, top="Average Wing length (mm) by Temperature and Family")


##GLMM models
#redid with state instead of pop for SPH poster
PQL_meth_sLL<- glmmPQL(sLL~Temp_fac*State, ~1|Locality/Family, family=gaussian(link="log"), data=adult, verbose=FALSE)
summary(PQL_meth_sLL)
#comparing interactions
lar_ls<- lsmeans(PQL_meth_sLL, pairwise ~Temp_fac:State, adjust="tukey")
lar_cld<- cld(lar_ls, alpha=0.5, Letters=letters, adjust="tukey")

PQL_meth_em<- glmmPQL(Emtime~Temp_fac*Pop, ~1|Locality/Family, family=gaussian(link="log"), data=adult, verbose=FALSE)
summary(PQL_meth_em)
#comparing interactions
em_ls<- lsmeans(PQL_meth_em, pairwise ~Temp_fac:Pop, adjust="tukey")
em_cld<- cld(em_ls, alpha=0.5, Letters=letters, adjust="tukey")

PQL_meth_al<- glmmPQL(AL~Temp_fac*State, ~1|Locality/Family, family=poisson, data=adar_adult, verbose=FALSE)
summary(PQL_meth_al)
#comparing interactions
al_ls<- lsmeans(PQL_meth_al, pairwise ~Temp_fac:State, adjust="tukey")
al_cld<- cld(al_ls, alpha=0.5, Letters=letters, adjust="tukey")

PQL_meth_wing<- glmmPQL(Wing.length..mm.~Temp_fac*State, ~1|Locality/Family, family=gaussian(link="log"), data=adar_adult, verbose=FALSE)
summary(PQL_meth_wing)
#comparing interactions
wing_ls<- lsmeans(PQL_meth_wing, pairwise ~Temp_fac:State, adjust="tukey")
wing_cld<- cld(wing_ls, alpha=0.5, Letters=letters, adjust="tukey")

##ANOVA tables for gen var, phen plast and G by E
#Larvae
larv_mod<-lmer(sLL~Temp_let*State+(1|Fam_new)+(1|State:Temp_let:Fam_new), data=adar_adult)
anova(larv_mod) #both Treatment and State sig as well as interaction
  difflsmeans(larv_mod, test.effs="State") #all state comparisons except rio and tocantins significant
difflsmeans(larv_mod, test.effs="Temp_let") #all temp comparisons significant
l.vc<-VarCorr(larv_mod)
print(l.vc, comp=c("Variance"))
#adult
adult_mod<-lmer(AL~Temp_let*State+(1|Fam_new)+(1|State:Temp_let:Fam_new), data=adar_adult)
anova(adult_mod) #both Treatment and State sig as well as interaction
rand(adult_mod)#get random effects table
difflsmeans(adult_mod, test.effs="State") #all state comparisons except rio and tocantins significant
difflsmeans(adult_mod, test.effs="Temp_let") #all temp comparisons significant

#wing length
wing_mod<-lmer(Wing.length..mm.~Temp_let*State+(1|Fam_new)+(1|State:Temp_let:Fam_new), data=adar_adult)
anova(wing_mod) #both Treatment and State sig as well as interaction
rand(wing_mod)#get random effects table
difflsmeans(wing_mod, test.effs="State") #Wing.length..mm.all state comparisons except amazonas and rondonia significant
difflsmeans(wing_mod, test.effs="Temp_let") #Wing.length..mm.all temp comparisons significant

###ANOVAS with sex for supplemental
#male=1
larv_mod1<-lmer(sLL~Temp_let*State*Sex1+(1|Fam_new)+(1|State:Temp_let:Fam_new), data=adar_adult)
anova(larv_mod1) #both Treatment and State sig as well as interaction

adult_mod1<-lmer(AL~Temp_let*State*Sex1+(1|Fam_new)+(1|State:Temp_let:Fam_new), data=adar_adult)
anova(adult_mod1)

wing_mod1<-lmer(Wing.length..mm.~Temp_let*State*Sex1+(1|Fam_new)+(1|State:Temp_let:Fam_new), data=adar_adult)
anova(wing_mod1)