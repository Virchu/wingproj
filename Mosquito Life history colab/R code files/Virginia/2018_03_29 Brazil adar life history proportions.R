#proportions dead or alive at adult eclosion 
library(dplyr)
library(lme4)
#setwd("C:/Users/virgc/Dropbox/Life history data/Data files")
getwd("C:/Users/virgc/Dropbox/Life history data/Data files")
#datafiles
lifehistshort<-read.csv("2018_03_28 Brazil adar16 Life history full.csv", header=TRUE)
lifehistshort$Pop<-paste(lifehistshort$Biome, lifehistshort$Lat_group)
lifehistshort$Temp_fac<-factor(lifehistshort$Temp_num)
lifehistshort$Lat_fac<-factor(lifehistshort$Latitude)

head(lifehistshort)


life_count<-read.csv("2018_03_28 Life hist totals for proportion survived.csv", header=TRUE)

deadoralive<- lifehistshort %>% group_by(Fam_new, Biome, State, Locality, Temp_fac, Lat_fac) %>% summarize(sum.alive = sum(Death_stat), na.rm = TRUE, sum.dead = length(Death_stat), na.rm = TRUE, 
                                                                                                           mean.Temp = mean(Temp_num), na.rm = TRUE, 
                                                                                                           mean.Lat = mean(Latitude, na.rm = TRUE)) %>% data.frame
deadoralive$true.dead<-deadoralive$sum.dead-deadoralive$sum.alive
#true.dead are the ones that died pre adult, while sum.alive are all that became adults


#binomial regression
life_prop<-glm(cbind(true.dead, sum.alive)~Temp_fac*State, data=deadoralive, family=binomial)
life_prop_temp<-glm(cbind(true.dead, sum.alive)~Temp_fac, data=deadoralive, family=binomial)
life_prop_state<-glm(cbind(true.dead, sum.alive)~State, data=deadoralive, family=binomial)
# model summaries
summary(life_prop_temp)
summary(life_prop_state)
summary(life_prop)#lowest AIC


#Cochran-Mantel-Haenszel Chi squared, proportion alive and dead
#by temp: 28 sig diff from 24|20, 20 not sig diff 24
table( lifehistshort$Temp_fac, lifehistshort$Death_stat)
prop.test(table(lifehistshort$Temp_fac, lifehistshort$Death_stat), correct=FALSE)
pairwise.prop.test(table(lifehistshort$Temp_fac, lifehistshort$Death_stat), correct=FALSE)

#by state:Cerrado Amazonas sig diff to Rio and Tocantins, Tocantins is not sig diff to Rio
table( lifehistshort$State, lifehistshort$Death_stat)
prop.test(table(lifehistshort$State, lifehistshort$Death_stat), correct=FALSE)
pairwise.prop.test(table(lifehistshort$State, lifehistshort$Death_stat), correct=FALSE)

#Used mantelhaen.test
#State, p=0.06361
datab=mutate(life_count, State=factor(State, levels=unique(State)),
             Temperature=factor(Temperature, levels=unique(Temperature)),
             Status=factor(Status, levels=unique(Status)))

datab.xtabs=xtabs(Count~Biome+Temperature+Status, data=datab)
ftable(datab.xtabs)
mantelhaen.test(datab.xtabs)
pairwise.prop.test(ftable(datab.xtabs), correct=FALSE)

