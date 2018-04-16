###Brazil Adar data and figures 4/5/18
##
#Setting up workspace
library(ggplot2) 
library(lme4)
library(lsmeans)
library(survival)
library(brms)
library(dplyr)
library(raster)
library(car)
library(Rcpp)

#Datafiles
adar_full<-read.csv("C:\\Users\\virgc\\Documents\\GitHub\\wingproj\\Mosquito Life history colab\\Data files\\2018_03_28 Brazil adar16 Life history full.csv")
adar_adult<-subset(adar_full, adar_full$Death_stat==1 & !is.na(adar_full$Sex1))

#quickview of data
scatterplotMatrix(~sLL+Biome+State+Temp_num, data = adar_adult)
scatterplotMatrix(~AL+Biome+State+Temp_num, data = adar_adult)
scatterplotMatrix(~Wing.length..mm.+Biome+State+Temp_num, data = adar_adult)

#larvae plot
ggplot(data = adar_adult, aes(x = Temp_num, y = sLL, color = Biome))+
  geom_jitter( size = 3)+
  geom_smooth(method = "lm")

adar_adult1<-adar_adult
levels(adar_adult1$State)<-c("Amazonas", "Rondonia", "Tocantins", "Rio de Janeiro")
adar_adult1$Temp_fac<-factor(adar_adult1$Temp_num)
legend_title <- "Temperature"
larv.scatter<-ggplot(adar_adult1, aes(x=Temp_fac, y= sLL, col=Temp_fac)) + geom_jitter(position=position_jitter(width=.2, height=0))+ facet_grid(.~State)+ ggtitle("Larvae development (days) ") +labs(colour="Temperature")+theme(plot.title = element_text(hjust = 0.5))+ xlab("Temperature (C)")+ylab("Days as larvae") 

em.scatter<-ggplot(adar_adult1, aes(x=Temp_fac, y= Emtime, col=Temp_fac))+ geom_jitter(position=position_jitter(width=.2, height=0))+ facet_grid(.~State)+ ggtitle("Time to emergence(days)")+labs(colour="Temperature")+theme(plot.title = element_text(hjust = 0.5))+ xlab("Temperature (C)")+ylab("Days to emergence")

adult.scatter<-ggplot(adar_adult1, aes(x=Temp_fac, y= AL, col=Temp_fac))+ geom_jitter(position=position_jitter(width=.2, height=0))+ facet_grid(.~State)+ ggtitle("Adult life (days) ")+labs(colour="Temperature")+theme(plot.title = element_text(hjust = 0.5))+ xlab("Temperature (C)")+ylab("Days as Adult")

wing.scatter<-ggplot(adar_adult1, aes(x=Temp_fac, y=Wing.length..mm., col=Temp_fac))+ geom_jitter(position=position_jitter(width=.2, height=0))+ facet_grid(.~State)+ ggtitle("Wing length (mm)")+labs(colour="Temperature")+theme(plot.title = element_text(hjust = 0.5))+ xlab("Temperature (C)")+ylab("Wing length (mm)")

##genetic variation
#subset data
#https://stackoverflow.com/questions/45326363/using-split-to-create-new-dataframes-in-r-and-rename-based-on-variable-conditio
local<-c("APR", "ARS", "RMO", "RPV", "TPN", "TLC", "SJU")
temps<-c(20,24,28)
grp.df <- adar_adult %>% 
  group_by(Locality, Temp_num) %>%
  mutate(newnames=paste0(Locality, Temp_num))
split.df<-split(grp.df, grp.df$newnames)
for (I in 1:length(split.df)){assign(unique(split.df[[I]]$newnames), split.df[[I]])}


grploc.df <- adar_adult %>% 
  group_by(Locality) %>%
  mutate(newnames=paste0(Locality, "_all"))
splitloc.df<-split(grploc.df, grploc.df$newnames)
for (I in 1:length(splitloc.df)){assign(unique(splitloc.df[[I]]$newnames), splitloc.df[[I]])}

grpstate.df <- adar_adult %>% 
  group_by(State) %>%
  mutate(newnames=paste0(gsub(" ", "",State), "_all"))
splitstate.df<-split(grpstate.df, grpstate.df$newnames)
for (I in 1:length(splitstate.df)){assign(unique(splitstate.df[[I]]$newnames), splitstate.df[[I]])}

grptem.df <- adar_adult %>% 
  group_by(Temp_num)%>%
  mutate(newnames=paste0("T", Temp_num, "_all"))
splittem.df<-split(grptem.df, grptem.df$newnames)
for (I in 1:length(splittem.df)){assign(unique(splittem.df[[I]]$newnames), splittem.df[[I]])}

#run ANOVAs on individual level data
#https://stackoverflow.com/questions/23961929/correct-use-of-sapply-with-anova-on-multiple-subsets-in-r
x <- unique(grp.df$newnames)

models <- sapply(x, function(my) {
  lm(sLL ~ Latitude * Temp_num, data=grp.df, newnames==my)}, simplify=FALSE)
#isue with factors so called on Latitude

ANOVA.tables <- sapply(models, anova, simplify=FALSE)
#does anovas within each Lat*Temp

#genetic variation locality level
APR_an<-aov(sLL~Temp_num, data=APR_all)
ARS_an<-aov(sLL~Temp_num, data=ARS_all)
RMO_an<-aov(sLL~Temp_num, data=RMO_all)
RPV_an<-aov(sLL~Temp_num, data=RPV_all)
TLC_an<-aov(sLL~Temp_num, data=TLC_all)
TPN_an<-aov(sLL~Temp_num, data=TPN_all)
SJU_an<-aov(sLL~Temp_num, data=SJU_all)

#phenotypic plasticity
T20_an<-aov(sLL~Locality, data=T20_all)
T24_an<-aov(sLL~Locality, data=T24_all)
T28_an<-aov(sLL~Locality, data=T28_all)

T20s_an<-aov(sLL~State, data=T20_all)
T24s_an<-aov(sLL~State, data=T24_all)
T28s_an<-aov(sLL~State, data=T28_all)

library(lme4)
library(lmerTest)
#genetic variance for plasticity
larv_mod<-lmer(sLL~Temp_num*State+(1|Fam_new)+(1|State:Temp_num:Fam_new), data=adar_adult)
anova(larv_mod)
rand(larv_mod)#get random effects table
#both Treatment and State sig as well as interaction
larv_loc_an<-aov(sLL~Temp_num*Locality, data=adar_adult)
summary(larv_loc_an)

##correlation
lar_al_cor<-lm(AL~sLL, data=adar_adult)
#not by temp, 28 is closest (p=0.088)
lar_20_cor<-lm(AL~sLL, data=T20_all)
lar_24_cor<-lm(AL~sLL, data=T24_all)
lar_28_cor<-lm(AL~sLL, data=T28_all)
#all sig by state
lar_AM_cor<-lm(AL~sLL, data=Amazonas_all)
lar_RO_cor<-lm(AL~sLL, data=Rondonia_all)
lar_TO_cor<-lm(AL~sLL, data=Tocantins_all)
lar_RJ_cor<-lm(AL~sLL, data=RiodeJaneiro_all)

#sig  by all
wing_al_cor<-lm(Wing.length..mm.~sLL, data=adar_adult)
#ALL SIG BY TEMP
wing_20_cor<-lm(Wing.length..mm.~sLL, data=T20_all)
wing_24_cor<-lm(Wing.length..mm.~sLL, data=T24_all)
wing_28_cor<-lm(Wing.length..mm.~sLL, data=T28_all)
#all sig by state
wing_AM_cor<-lm(Wing.length..mm.~sLL, data=Amazonas_all)
wing_RO_cor<-lm(Wing.length..mm.~sLL, data=Rondonia_all)
wing_TO_cor<-lm(Wing.length..mm.~sLL, data=Tocantins_all)
wing_RJ_cor<-lm(Wing.length..mm.~sLL, data=RiodeJaneiro_all)



#bayesian things
# summarize to family means
adar_adult$Temp_fac <- as.factor(adar_adult$Temp_num)

# with separate sexes
adult.sum <- adar_adult %>% group_by(Fam_new, Biome, State, Locality, Sex, Temp_fac, Lat_group) %>% summarize(mean.sLL = mean(sLL, na.rm = TRUE), 
                                                                                                           mean.Temp = mean(Temp_num), na.rm = TRUE, 
                                                                                                           mean.Lat = mean(Latitude, na.rm = TRUE)) %>% data.frame
#grouping sexes
adult.sum.nosex <- adar_adult %>% group_by(Fam_new, Biome, State, Locality, Temp_fac, Lat_group) %>% summarize(mean.sLL = mean(sLL, na.rm = TRUE), 
                                                                                                            mean.Temp = mean(Temp_num), na.rm = TRUE, 
                                                                                                            mean.Lat = mean(Latitude, na.rm = TRUE)) %>% data.frame
