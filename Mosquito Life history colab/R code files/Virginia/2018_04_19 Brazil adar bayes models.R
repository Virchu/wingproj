#ylibrary(Rcpp)
library(brms)
library(dplyr)
library(ggplot2)
library(lme4)
library(raster)
library(cowplot)
library(car)
#library(rstan)
#library(shinystan)
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
all.sum.nosex <- all_data %>% group_by(Fam_new, Biome, State, Locality,Temp_num, Lat_group, Death_stat) %>% summarize(mean.sLL = mean(sLL, na.rm = TRUE),
                                                                                                 mean.Temp = mean(Temp_num, na.rm=TRUE),
                                                                                                 mean.Lat=mean(Latitude, na.rm=TRUE),
                                                                                                 count=length(Death_stat)) %>% data.frame

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

###bayes model attempts
#from tim
# no group-level effect (Random Effect)

#trying out model to see if temp or state affected # dead as larvae
get_prior(count~ Temp_num + State+(1|Fam_new), data = all.sum.nosex)

brm0 <- brm(count~ 1+ Temp_num + State, data = all.sum.nosex,
              chains = 2, cores = 2, control= list(adapt_delta = 0.9), #SAMPLING PARAMETER SPACE
              iter = 3000, warmup = 1500, thin = 5, 
              prior =  c(prior(normal(0, 1), "b"),
                         prior(normal(0, 1), "Intercept"),
                         prior(normal(0, 1), "sigma")))
brm0.1 <- brm(count~ 1+ Temp_num + State+ (1|Fam_new), data = all.sum.nosex,
            chains = 2, cores = 2, control= list(adapt_delta = 0.9), #SAMPLING PARAMETER SPACE
            iter = 3000, warmup = 1500, thin = 5, 
            prior =  c(prior(normal(0, 1), "b"),
                       prior(normal(0, 1), "Intercept"),
                       prior(normal(0, 1), "sigma"),
                       prior(normal(0, 1), "sd")))

brm0.11 <- brm(count~ 1+ Temp_num + State+ (1|Fam_new), data = all.sum.nosex,
              chains = 2, cores = 2, control= list(adapt_delta = 0.9), #SAMPLING PARAMETER SPACE
              iter = 3000, warmup = 1500, thin = 5)
# model checking
summary(brm0)
fixef(brm0)
bayes_R2(brm0) # Bayesian R2 estimate, get confidence interval
plot(brm0)
plot(marginal_effects(brm0), points=TRUE)

summary(brm0.1)
fixef(brm0.1)
bayes_R2(brm0.1) # Bayesian R2 estimate, get confidence interval
plot(brm0.1)
plot(marginal_effects(brm0.1), points=TRUE)
#larvae development

get_prior(mean.sLL~ Temp_num * State+(1|Fam_new), data = all.sum.nosex)

brm1 <- brm(mean.sLL~ 1+ Temp_fac + State, data = adult.sum.nosex,
            chains = 2, cores = 2, control= list(adapt_delta = 0.9), #SAMPLING PARAMETER SPACE
            iter = 3000, warmup = 1500, thin = 5, 
            prior =  c(prior(normal(0, 1), "b"),
                       prior(normal(0, 1), "Intercept"),
                       prior(normal(0, 1), "sigma")))

brm1.m <- brm(mean.sLL~ 1+ Temp_fac * State, data = adult.sum.nosex,
            chains = 2, cores = 2, control= list(adapt_delta = 0.9), #SAMPLING PARAMETER SPACE
            iter = 3000, warmup = 1500, thin = 5, 
            prior =  c(prior(normal(0, 1), "b"),
                       prior(normal(0, 1), "Intercept"),
                       prior(normal(0, 1), "sigma")))

#compare to all data (with dead larvae, n=778)?
brm1.1 <- brm(mean.sLL~ 1+ Temp_num + State, data = all.sum.nosex,
            chains = 2, cores = 2, control= list(adapt_delta = 0.9), #SAMPLING PARAMETER SPACE
            iter = 3000, warmup = 1500, thin = 5, 
            prior =  c(prior(normal(0, 1), "b"),
                       prior(normal(0, 1), "Intercept"),
                       prior(normal(0, 1), "sigma")))

#how to set priors for random effect?

brm1.2 <- brm(mean.sLL~ 1+ Temp_num + State+ (1|Fam_new), data = all.sum.nosex,
              chains = 2, cores = 2, control= list(adapt_delta = 0.9), #SAMPLING PARAMETER SPACE
              iter = 3000, warmup = 1500, thin = 5, 
              prior =  c(prior(normal(0, 1), "b"),
                         prior(normal(0, 1), "Intercept"),
                         prior(normal(0, 1), "sigma"),
                         prior(normal(0, 10), "sd")))

# model checking
summary(brm1)
fixef(brm1)
bayes_R2(brm1) # Bayesian R2 estimate, get confidence interval
plot(marginal_effects(brm1), points=TRUE)


summary(brm1.1)
fixef(brm1.1)
bayes_R2(brm1.1)
#similar so dead doesn't matter?

# use shiny to take a look at model outputs, diagnostic plots
#launch_shiny(brm1)

#adult life
get_prior(mean.AL~ Temp_fac + State, data = adult.sum.nosex)

brm2 <- brm(mean.AL~ 1+ Temp_fac + State, data = adult.sum.nosex,
            chains = 2, cores = 2, control= list(adapt_delta = 0.9), #SAMPLING PARAMETER SPACE
            iter = 3000, warmup = 1500, thin = 5, 
            prior =  c(prior(normal(0, 1), "b"),
                       prior(normal(0, 1), "Intercept"),
                       prior(normal(0, 1), "sigma")))
summary(brm2)
fixef(brm2)
bayes_R2(brm2)


#wing length
get_prior(mean.wing~ Temp_fac + State, data = adult.sum.nosex)

brm3 <- brm(mean.wing~ 1+ Temp_fac + State, data = adult.sum.nosex,
            chains = 2, cores = 2, control= list(adapt_delta = 0.9), #SAMPLING PARAMETER SPACE
            iter = 3000, warmup = 1500, thin = 5, 
            prior =  c(prior(normal(0, 1), "b"),
                       prior(normal(0, 1), "Intercept"),
                       prior(normal(0, 1), "sigma")))
summary(brm3)
fixef(brm3)
bayes_R2(brm3)
#multivariate model to look at correlation
get_prior(cbind(mean.sLL, mean.AL, mean.wing)~ Temp_fac+State, data=adult.sum.nosex)

multi_adult<- brm(cbind(mean.sLL, mean.AL, mean.wing)~ 1+Temp_fac+State, data=adult.sum.nosex, 
                  chains=2, cones=2,control= list(adapt_delta = 0.9),
                  iter = 3000, warmup = 1500, thin = 5, 
                  prior =  c(prior(normal(0, 1), "b"),
                             prior(normal(0, 1), "cor"),
                             prior(normal(0, 1), "Intercept"),
                             prior(normal(0, 1), "sigma"),
                             prior(normal(0, 1), "rescor")
                             ))