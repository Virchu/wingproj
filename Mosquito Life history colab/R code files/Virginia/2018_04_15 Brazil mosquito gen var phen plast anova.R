#
#https://rcompanion.org/rcompanion/d_07.html

#setup
library(lme4)
library(lmerTest)
library(multcomp)
library(dplyr)

#Datafiles
adar_full<-read.csv("C:\\Users\\virgc\\GitHub\\wingproj\\Mosquito Life history colab\\Data files\\2018_03_28 Brazil adar16 Life history full.csv")
adar_adult<-subset(adar_full, adar_full$Death_stat==1 & !is.na(adar_full$Sex1))

#group by state to look within state
grpstate.df <- adar_adult %>% 
  group_by(State) %>%
  mutate(newnames=paste0(gsub(" ", "",State), "_all"))
splitstate.df<-split(grpstate.df, grpstate.df$newnames)
for (I in 1:length(splitstate.df)){assign(unique(splitstate.df[[I]]$newnames), splitstate.df[[I]])}
#outputs Amazonas_all, Rondonia_all, Tocantins_all (RiodeJaneior_all has just the 1 site)

#genetic variance for plasticity
#larvae development
larv_mod<-lmer(sLL~Temp_let*State+(1|Fam_new)+(1|State:Temp_let:Fam_new), data=adar_adult)
anova(larv_mod) #both Treatment and State sig as well as interaction
rand(larv_mod)#get random effects table
difflsmeans(larv_mod, test.effs="State") #all state comparisons except rio and tocantins significant
difflsmeans(larv_mod, test.effs="Temp_let") #all temp comparisons significant

larv_modb<-lmer(sLL~Temp_let*Biome+(1|Fam_new)+(1|Biome:Temp_let:Fam_new), data=adar_adult)
anova(larv_modb) #both Treatment and State sig as well as interaction
rand(larv_modb)#get random effects table
difflsmeans(larv_modb, test.effs="Biome") #all state comparisons except rio and tocantins significant
difflsmeans(larv_modb, test.effs="Temp_let") #all temp comparisons significant

larv_modl<-lmer(sLL~Temp_let*Latitude+(1|Fam_new)+(1|Latitude:Temp_let:Fam_new), data=adar_adult)
anova(larv_modl) #both Treatment and State sig as well as interaction
rand(larv_modl)#get random effects table
difflsmeans(larv_modl, test.effs="Latitude") #all state comparisons except rio and tocantins significant
difflsmeans(larv_modl, test.effs="Temp_let") #all temp comparisons significant

#posthoc
larv_post=glht(larv_mod, linfct=mcp(State="Tukey"))
larv_ps=summary(larv_post, test=adjusted("single-step"))
larv_ps
cld(larv_ps, level=0.05, decreasing=TRUE)

larv_mod1<-lmer(sLL~Temp_let*Locality+(1|Fam_new)+(1|Temp_let:Locality:Fam_new), data=adar_adult)
anova(larv_mod1) #both Treatment and Locality sig as well as interaction
rand(larv_mod1)#get random effects table- both random terms sig
difflsmeans(larv_mod1, test.effs="Locality") #all Locality comparisons except rio and tocantins (TPN) significant
difflsmeans(larv_mod1, test.effs="Temp_let") #all temp comparisons significant

#checking Sex at state level
larv_mod2<-lmer(sLL~Temp_let*State*Sex+(1|Fam_new)+(1|State:Temp_let:Fam_new), data=adar_adult)
anova(larv_mod2) #both Treatment and State sig as well as interaction
rand(larv_mod2)#get random effects table
difflsmeans(larv_mod2, test.effs="State") #all state comparisons except rio and tocantins significant
difflsmeans(larv_mod2, test.effs="Temp_let") #all temp comparisons significant
difflsmeans(larv_mod2, test.effs="Sex") #sexes are significantly different

#within Amazonas comparison
larv_mod3<-lmer(sLL~Temp_let*Locality*Sex+(1|Fam_new)+(1|Temp_let:Fam_new), data=Amazonas_all)
anova(larv_mod3) #both Treatment and State sig as well as interaction
rand(larv_mod3)#get random effects table
difflsmeans(larv_mod3, test.effs="Temp_let")
difflsmeans(larv_mod3, test.effs="Locality")

larv_mod4<-lmer(sLL~Temp_let*Locality+(1|Fam_new)+(1|Temp_let:Fam_new), data=Amazonas_all)
anova(larv_mod4)

##adult life
adult_mod<-lmer(AL~Temp_let*State+(1|Fam_new)+(1|State:Temp_let:Fam_new), data=adar_adult)
anova(adult_mod) #both Treatment and State sig as well as interaction
rand(adult_mod)#get random effects table
difflsmeans(adult_mod, test.effs="State") #all state comparisons except rio and tocantins significant
difflsmeans(adult_mod, test.effs="Temp_let") #all temp comparisons significant

adult_mod1<-lmer(AL~Temp_let*Locality+(1|Fam_new)+(1|Temp_let:Locality:Fam_new), data=adar_adult)
anova(adult_mod1) #both Treatment and Locality sig as well as interaction
rand(adult_mod1)#get random effects table- both random terms sig
difflsmeans(adult_mod1, test.effs="Locality") #all Locality comparisons except rio and tocantins (TPN) significant
difflsmeans(adult_mod1, test.effs="Temp_let") #all temp comparisons significant
