library(dplyr)
library(ggplot2)
library(lme4)
library(brms)
library(gdata)
library(lattice)
library(car)
library(cowplot)

#read in and check data
adult <- read.csv("C:\\Users\\Tim\\Desktop\\mosquito\\2018_03_07 Life history adult data.csv")
head(adult)
str(adult)
scatterplotMatrix(~sLL+Biome+Pop+Temp_num, data = adult)

# initial plot
ggplot(data = adult, aes(x = Temp_num, y = sLL, color = Biome))+
  geom_jitter( size = 3)+
  geom_smooth(method = "lm")
  
# individual lms for each biome
lm1.amazon <- lm(sLL ~ Temp_num, data = filter(adult, Biome=="Amazon"))
lm1.cerrado <- lm(sLL ~ Temp_num, data = filter(adult, Biome=="Cerrado"))
lm1.mata <- lm(sLL ~ Temp_num, data = filter(adult, Biome=="Mata Atlantica"))

summary(lm1.amazon)
summary(lm1.cerrado)
summary(lm1.mata)


# summarize to family means
head(adult)
adult$Temp_fac <- as.factor(adult$Temp_fac)

# with separate sexes
adult.sum <- adult %>% group_by(Fam_new, Biome, State, Locality, Sex, Temp_fac, Lat_grouple) %>% summarize(mean.sLL = mean(sLL, na.rm = TRUE), 
                                                                                       mean.Temp = mean(Temp_num), na.rm = TRUE, 
                                                                                       mean.Lat = mean(Latitude, na.rm = TRUE)) %>% data.frame
#grouping sexes
adult.sum.nosex <- adult %>% group_by(Fam_new, Biome, State, Locality, Temp_fac, Lat_grouple) %>% summarize(mean.sLL = mean(sLL, na.rm = TRUE), 
                                                                                    mean.Temp = mean(Temp_num), na.rm = TRUE, 
                                                                                    mean.Lat = mean(Latitude, na.rm = TRUE)) %>% data.frame

# boxplots using different grouping factors
ggplot()+
  geom_boxplot(data = adult.sum.nosex, aes(x = Temp_fac, y = mean.sLL, color = Biome))

ggplot()+
  geom_boxplot(data = adult.sum.nosex, aes(x = Temp_fac, y = mean.sLL, color = State, fill = Biome))
# should order State by latitude

ggplot()+
  geom_boxplot(data = adult.sum, aes(x = interaction(Temp_fac, Sex), y = mean.sLL, color = Biome))


# family-level lms for each biome
lm2.amazon <- lm(mean.sLL ~ Temp_fac, data = filter(adult.sum.nosex, Biome=="Amazon"))
lm2.cerrado <- lm(mean.sLL ~ Temp_fac, data = filter(adult.sum.nosex, Biome=="Cerrado"))
lm2.mata <- lm(mean.sLL ~ Temp_fac, data = filter(adult.sum.nosex, Biome=="Mata Atlantica"))

summary(lm2.amazon)
summary(lm2.cerrado)
summary(lm2.mata)


# lme4 models
head(adult.sum.nosex)
scatterplotMatrix(~Temp_fac|Lat_grouple, data = adult.sum.nosex)
adult.sum.nosex$Lat.fac <- as.factor(adult.sum.nosex$mean.Lat)

# model with Biome
lme1 <- lmer(mean.sLL ~ 1 + Temp_fac + (1+ Temp_fac|Biome), data = adult.sum.nosex, REML = FALSE, 
             control = lmerControl(optimizer ="Nelder_Mead"))
# model with Latitude Group
lme2 <- lmer(mean.sLL ~ 1 + Temp_fac + (1+ Temp_fac|Lat_grouple), data = adult.sum.nosex, REML = FALSE, 
             control = lmerControl(optimizer ="Nelder_Mead"))
# Model with Latitude as a factor
lme3 <- lmer(mean.sLL ~ 1 + Temp_fac + (1+ Temp_fac|Lat.fac), data = adult.sum.nosex, REML = FALSE, 
             control = lmerControl(optimizer ="Nelder_Mead"))
lme4 <- lmer(mean.sLL ~ 1 + Temp_fac*State + (1+ Temp_fac|Lat.fac), data = adult.sum.nosex, REML = FALSE)
lm4b <- lm(mean.sLL ~ 1 + Temp_fac*State , data = adult.sum.nosex)
summary(lm4b)


library(lmerTest)
lme4 <- lmer(mean.sLL ~ 1 + Temp_fac*State + (1+ Temp_fac|Lat.fac), data = adult.sum.nosex, REML = FALSE)
summary(lme4)
anova(lme4)
ranef(lme4)

AIC(lme4, lm4b)

r.squaredGLMM(lme4)
r.squaredGLMM(lm4b)


#model with state nested in biome
lme5 <- lmer(mean.sLL ~ 1 + Temp_fac + State + (1+ Temp_fac|Lat.fac), data = adult.sum.nosex, REML = FALSE, 
             control = lmerControl(optimizer ="Nelder_Mead"))


library(nlme)
lme4.a <- lme(mean.sLL ~ 1 + Temp_fac*State, random=~ 1+Temp_fac|Lat.fac, data = adult.sum.nosex)



levels(adult.sum.nosex$Lat.fac)
# model summaries
summary(lme1)
summary(lme2)
summary(lme3)
summary(lme4)

coef(lme1)
coef(lme2)
coef(lme3)


AIC(lme1, lme2, lme3, lme4, lme5)

fixef(lme1)
fixef(lme2)
fixef(lme3)


library(MuMIn)
r.squaredGLMM(lme1)
r.squaredGLMM(lme2)
r.squaredGLMM(lme3)



# read in mat and map from worldclim
library(raster)
mat <- raster("D:\\bioclim\\bio1-9_30s_bil\\bio_1.bil")
map <- raster("D:\\bioclim\\bio10-19_30s_bil\\bio_12.bil")

head(adult.sum.nosex)
summary(lm(mean.sLL~mean.Temp, data = filter(adult.sum.nosex, State == "Amazonas")))
summary(lm(mean.sLL~mean.Temp, data = filter(adult.sum.nosex, State == "Rondonia")))



ggplot()+
  geom_point(data = adult.sum.nosex, aes(x = mean.Temp, y = mean.sLL, color = State), method = 'lm')+
  geom_smooth(data = adult.sum.nosex, aes(x = mean.Temp, y = mean.sLL, color = State), method = 'lm')


vignettes(brms)





