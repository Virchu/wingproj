# mosquito Bayesain analysis with PCA

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
adult <- read.csv("C:\\Users\\virgc\\Documents\\GitHub\\wingproj\\Mosquito Life history colab\\Data files\\2018_03_28 Brazil adar16 Life history full.csv")
#all<- 2018_03_28 Brazil adar16 Life history full.csv
head(adult)
str(adult)
scatterplotMatrix(~sLL+Biome+State+Temp_num, data = adult)

# initial plot
ggplot(data = adult, aes(x = Temp_num, y = sLL, color = Biome))+
  geom_jitter( size = 3)+
  geom_smooth(method = "lm")


# summarize to family means
head(adult)
adult$Temp_fac <- as.factor(adult$Temp_num)

# with separate sexes
adult.sum <- adult %>% group_by(Fam_new, Biome, State, Locality, Sex, Temp_fac, Lat_group) %>% summarize(mean.sLL = mean(sLL, na.rm = TRUE), 
                                                                                                           mean.Temp = mean(Temp_num), na.rm = TRUE, 
                                                                                                           mean.Lat = mean(Latitude, na.rm = TRUE)) %>% data.frame
#grouping sexes
adult.sum.nosex <- adult %>% group_by(Fam_new, Biome, State, Locality, Temp_fac, Lat_group) %>% summarize(mean.sLL = mean(sLL, na.rm = TRUE), 
                                                                                                            mean.Temp = mean(Temp_num), na.rm = TRUE, 
                                                                                                            mean.Lat = mean(Latitude, na.rm = TRUE)) %>% data.frame



# brms code
library(brms)
adult.sum.nosex$Lat.fac <- as.factor(adult.sum.nosex$mean.Lat)


# no group-level effect (Random Effect)
get_prior(mean.sLL~ Temp_fac + State, data = adult.sum.nosex)

brm1 <- brm(mean.sLL~ 1 +  Temp_fac + State, data = adult.sum.nosex,
    chains = 2, cores = 2, control= list(adapt_delta = 0.9),
    iter = 3000, warmup = 1500, thin = 5, 
    prior =  c(prior(normal(0, 1), "b"),
               prior(normal(0, 1), "Intercept"),
               prior(normal(0, 1), "sigma")))


# model checking
summary(brm1)
fixef(brm1)
bayes_R2(brm1) # Bayesian R2 estimate

# use shiny to take a look at model outputs
launch_shinystan(brm1)
# compare predicted values to observed mean.sLL values
pp_check(brm1, nsamples = 100) # note that there is a bit of a right-skew in the observed data, which *could* be fixed by logging mean.sLL


# compare to lm model
lm4 <- lm(mean.sLL ~ 1 + Temp_fac + State, data = adult.sum.nosex)
summary(lm4)



# With Lat as a group-level effect (Random Effect)
get_prior(mean.sLL~ Temp_fac + State + (1 + Temp_fac|Lat.fac), data = adult.sum.nosex)
brm2 <- brm(mean.sLL~ Temp_fac + State + (1 + Temp_fac|Lat.fac), data = adult.sum.nosex,
            chains = 2, cores = 2, control= list(adapt_delta = 0.9),
            iter = 3000, warmup = 1500, thin = 5, 
            prior =  c(prior(normal(0, 1), "b"),
                       prior(normal(0, 1), "sd"),
                       prior(normal(0, 1), "Intercept"),
                       prior(normal(0, 1), "sigma")))



summary(brm2)
pp_check(brm2, nsamples=100)
bayes_R2(brm2)
coef(brm2)

# what about with continuous Temp?

brm2 <- brm(mean.sLL~ mean.Temp + State + (1 + mean.Temp|Lat.fac), data = adult.sum.nosex,
            chains = 2, cores = 2, control= list(adapt_delta = 0.9),
            iter = 3000, warmup = 1500, thin = 5, 
            prior =  c(prior(normal(0, 1), "b"),
                       prior(normal(0, 1), "sd"),
                       prior(normal(0, 1), "Intercept"),
                       prior(normal(0, 1), "sigma")))


summary(brm2)
pp_check(brm2, nsamples=100)
bayes_R2(brm2)
coef(brm2)




#---------------------------------------------------------------------------------------------------------------------------#
# do PCA on env for each state

#get state shapefile
library(rgdal)
library(gdata)
library(dplyr)
states <- readOGR(dsn = "D:\\brazil_maps\\br_unidades_da_federacao", layer = "BRUFE250GC_SIR")
plot(states)

levels(adult$State)
states@data


amazonas <- states[states@data$CD_GEOCUF==13,] %>% drop.levels
tocantins <- states[states@data$CD_GEOCUF==17,] %>% drop.levels
rio <- states[states@data$CD_GEOCUF==33,] %>% drop.levels
rond <- states[states@data$CD_GEOCUF==11,] %>% drop.levels
plot(rond)

states[states@data$CD_GEOCUF %in% c(13,17,33,11),] %>% drop.levels

states.sub <- states[states@data$CD_GEOCUF %in% c(13,17,33,11),] %>% drop.levels


plot(states)
plot(states.sub, col = "purple", add = TRUE)

# generate random points for each region

am.points <- spsample(amazonas, 100, "random")@coords %>% data.frame
toc.points <- spsample(tocantins, 100, "random")@coords  %>% data.frame
rio.points <- spsample(rio, 100, "random")@coords  %>% data.frame
ron.points <- spsample(rond, 100, "random")@coords  %>% data.frame

am.points$state  <- "Amazonas"
toc.points$state  <- "Tocantins"
rio.points$state  <- "Rio de Janeiro"
ron.points$state  <- "Rondonia"

comb.points <- rbind(am.points, toc.points, rio.points, ron.points)

plot(states)
plot(states.sub, col = "purple" ,add = TRUE)
points(am.points)
points(toc.points)
points(rio.points)
points(ron.points)

# get worldclim data
bio1 <- raster("D:\\bioclim\\bio1-9_30s_bil\\bio_1.bil")
bio2 <- raster("D:\\bioclim\\bio1-9_30s_bil\\bio_2.bil")
bio3 <- raster("D:\\bioclim\\bio1-9_30s_bil\\bio_3.bil")
bio4 <- raster("D:\\bioclim\\bio1-9_30s_bil\\bio_4.bil")
bio5 <- raster("D:\\bioclim\\bio1-9_30s_bil\\bio_5.bil")
bio6 <- raster("D:\\bioclim\\bio1-9_30s_bil\\bio_6.bil")
bio7 <- raster("D:\\bioclim\\bio1-9_30s_bil\\bio_7.bil")
bio8 <- raster("D:\\bioclim\\bio1-9_30s_bil\\bio_8.bil")
bio9 <- raster("D:\\bioclim\\bio1-9_30s_bil\\bio_9.bil")

bio10 <- raster("D:\\bioclim\\bio10-19_30s_bil\\bio_10.bil")
bio11 <- raster("D:\\bioclim\\bio10-19_30s_bil\\bio_11.bil")
bio12 <- raster("D:\\bioclim\\bio10-19_30s_bil\\bio_12.bil")
bio13 <- raster("D:\\bioclim\\bio10-19_30s_bil\\bio_13.bil")
bio14 <- raster("D:\\bioclim\\bio10-19_30s_bil\\bio_14.bil")
bio15 <- raster("D:\\bioclim\\bio10-19_30s_bil\\bio_15.bil")
bio16 <- raster("D:\\bioclim\\bio10-19_30s_bil\\bio_16.bil")
bio17 <- raster("D:\\bioclim\\bio10-19_30s_bil\\bio_17.bil")
bio18 <- raster("D:\\bioclim\\bio10-19_30s_bil\\bio_18.bil")
bio19 <- raster("D:\\bioclim\\bio10-19_30s_bil\\bio_19.bil")


bioclim.stack <- stack(bio1, bio2, bio3, bio4, bio5, bio6, bio7, bio8, bio9, bio10, bio11, bio12, bio13, bio14, bio15, 
                       bio16, bio17, bio18, bio19)



env.data <- raster::extract(bioclim.stack, comb.points[, c("x", "y")])
env.data.s <-env.data

# scale environmental data prior to PCA  -probably not necessary
# for (i in 1:19){
#   
#   env.data.s[, i] <-  as.numeric(scale(env.data.s[, i]))
#   
# }

comb.points.env <- cbind(comb.points, env.data)


library(FactoMineR)
head(comb.points.env)
res.pca <- PCA(comb.points.env[, c("state", "bio_1", "bio_12", "bio_18", "bio_6", "bio_3")], quali.sup = 1)
plot(res.pca)

res.pca.data <- res.pca$ind$coord %>% data.frame

comb.points.env$PC1 <- res.pca.data$Dim.1
comb.points.env$PC2 <- res.pca.data$Dim.2

library(ggplot2)
library(cowplot)

ggplot(data = comb.points.env, aes(x = PC1, y = PC2, color = state, fill = state))+
  geom_point(size = 4)+
  stat_ellipse(geom = "polygon", alpha = 0.5)


#-------------------------------------------------------------------------------------------------------------------------#
library(brms)
#within-state do we detect significant reaction norms and are families sigificantly different?
head(adult)
brm.am <- brm(mean.sLL~1 + Temp_fac +(1 + Temp_fac|Fam_new), data = filter(adult.sum.nosex, State == "Amazonas"))
summary(brm.am)
coef(brm.am)
plot(brm.am)


brm.toc <- brm(mean.sLL~1 + Temp_fac +(1 + Temp_fac|Fam_new), data = filter(adult.sum.nosex, State == "Tocantins"))
summary(brm.toc)
coef(brm.toc)
plot(brm.toc)

brm.rio <- brm(mean.sLL~1 + Temp_fac +(1 + Temp_fac|Fam_new), data = filter(adult.sum.nosex, State == "Rio de Janeiro"))
summary(brm.rio)
coef(brm.rio)
plot(brm.rio)

brm.ron <- brm(mean.sLL~1 + Temp_fac +(1 + Temp_fac|Fam_new), data = filter(adult.sum.nosex, State == "Rondonia"))
summary(brm.ron)
coef(brm.ron)
plot(brm.ron)

# plot these 
marginal_effects(brm.am) # no overlap between reg coeff CI's
marginal_effects(brm.toc)  #  reg coeff CI's of 24 and 28 overlap
marginal_effects(brm.rio)  #  reg coeff CI's of 24 and 28 overlap
marginal_effects(brm.ron) # no overlap between reg coeff CI's

# in ggplot2

am.rn <- 
ggplot(data = filter(adult.sum.nosex, State == "Amazonas"), aes(x = Temp_fac, y = mean.sLL, 
                                                                               group = interaction(Fam_new)))+
  geom_point(aes(color = interaction(Fam_new)))+
  geom_path(aes(color = interaction(Fam_new)))

am.wi.rn <- 
  ggplot(data = filter(adult.sum.nosex, State == "Amazonas"), aes(x = Temp_fac, y = mean.sLL, 
                                                                  group = interaction(Locality)))+
  geom_point(aes(color = interaction(Locality)))+
  geom_path(aes(color = interaction(Locality)))

toc.rn <- 
ggplot(data = filter(adult.sum.nosex, State == "Tocantins"), aes(x = Temp_fac, y = mean.sLL, 
                                                                 group = interaction(Fam_new)))+
  geom_point(aes(color = interaction(Fam_new)))+
  geom_path(aes(color = interaction(Fam_new)))

rio.rn <- 
ggplot(data = filter(adult.sum.nosex, State == "Rio de Janeiro"), aes(x = Temp_fac, y = mean.sLL, 
                                                                 group = interaction(Fam_new)))+
  geom_point(aes(color = interaction(Fam_new)))+
  geom_path(aes(color = interaction(Fam_new)))

rod.rn <- 
ggplot(data = filter(adult.sum.nosex, State == "Rondonia"), aes(x = Temp_fac, y = mean.sLL, 
                                                                      group = interaction(Fam_new)))+
  geom_point(aes(color = interaction(Fam_new)))+
  geom_path(aes(color = interaction(Fam_new)))

toc.wi.rn <- 
  ggplot(data = filter(adult.sum.nosex, State == "Tocantins"), aes(x = Temp_fac, y = mean.sLL, 
                                                                   group = interaction(Locality)))+
  geom_point(aes(color = interaction(Locality)))+
  geom_path(aes(color = interaction(Locality)))

rio.wi.rn <- 
  ggplot(data = filter(adult.sum.nosex, State == "Rio de Janeiro"), aes(x = Temp_fac, y = mean.sLL, 
                                                                        group = interaction(Locality)))+
  geom_point(aes(color = interaction(Locality)))+
  geom_path(aes(color = interaction(Locality)))

rod.wi.rn <- 
  ggplot(data = filter(adult.sum.nosex, State == "Rondonia"), aes(x = Temp_fac, y = mean.sLL, 
                                                                  group = interaction(Locality)))+
  geom_point(aes(color = interaction(Locality)))+
  geom_path(aes(color = interaction(Locality)))
plot_grid(am.rn, 
          toc.rn, 
          rio.rn, 
          rod.rn, ncol = 2)

plot_grid(am.wi.rn, 
          toc.wi.rn, 
          rio.wi.rn, 
          rod.wi.rn, ncol = 2)

ggplot(data = filter(adult.sum.nosex), aes(x = Temp_fac, y = mean.sLL, 
                                                                  group = interaction(Fam_new, State)))+
  geom_point(aes(color = interaction(State)))+
  geom_path(aes(color = interaction(State)))

#-------------------#
# continuous  temps

brm.am.c <- brm(mean.sLL~1 + mean.Temp +(1 + mean.Temp|Fam_new), data = filter(adult.sum.nosex, State == "Amazonas"))
summary(brm.am.c)
coef(brm.am)
plot(brm.am.c)
marginal_effects(brm.am.c)

brm.toc.c <- brm(mean.sLL~1 + mean.Temp +(1 + mean.Temp|Fam_new), data = filter(adult.sum.nosex, State == "Tocantins"))
summary(brm.toc.c)
coef(brm.toc)
plot(brm.toc.c)
marginal_effects(brm.toc.c)

brm.rio.c <- brm(mean.sLL~1 + mean.Temp +(1 + mean.Temp|Fam_new), data = filter(adult.sum.nosex, State == "Rio de Janeiro"))
summary(brm.rio.c)
coef(brm.rio)
plot(brm.rio.c)
marginal_effects(brm.rio.c)

brm.ron.c <- brm(mean.sLL~1 + mean.Temp +(1 + mean.Temp|Fam_new), data = filter(adult.sum.nosex, State == "Rondonia"))
summary(brm.ron.c)
coef(brm.ron)
plot(brm.ron.c)
a <- marginal_effects(brm.ron.c)
str(a)
plot_grid(marginal_effects(brm.am.c),
          marginal_effects(brm.toc.c),
          marginal_effects(brm.rio.c),
          marginal_effects(brm.ron.c), ncol = 2)


#-------------------#




#-------------------------------------------------------------------------------------------------------------------------#

# look at patterns with individual data

brm.am.w <- brm(sLL~1 + Temp_fac +(1 + Temp_fac|Fam_new), data = filter(adult, State == "Amazonas"))
summary(brm.am.w)
summary(brm.am.w)
coef(brm.am)
plot(brm.am)

brm.toc <- brm(mean.sLL~1 + Temp_fac +(1 + Temp_fac|Fam_new), data = filter(adult.sum.nosex, State == "Tocantins"))
summary(brm.toc)
coef(brm.toc)
plot(brm.toc)

brm.rio <- brm(mean.sLL~1 + Temp_fac +(1 + Temp_fac|Fam_new), data = filter(adult.sum.nosex, State == "Rio de Janeiro"))
summary(brm.rio)
coef(brm.rio)
plot(brm.rio)

brm.ron <- brm(mean.sLL~1 + Temp_fac +(1 + Temp_fac|Fam_new), data = filter(adult.sum.nosex, State == "Rondonia"))
summary(brm.ron)
coef(brm.ron)
plot(brm.ron)

# plot these 
marginal_effects(brm.am) # no overlap between reg coeff CI's
marginal_effects(brm.toc)  #  reg coeff CI's of 24 and 28 overlap
marginal_effects(brm.rio)  #  reg coeff CI's of 24 and 28 overlap
marginal_effects(brm.ron) # no overlap between reg coeff CI's

# in ggplot2

am.rn <- 
  ggplot(data = filter(adult.sum.nosex, State == "Amazonas"), aes(x = Temp_fac, y = mean.sLL, 
                                                                  group = interaction(Fam_new)))+
  geom_point(aes(color = interaction(Fam_new)), size = 2)+
  geom_path(aes(color = interaction(Fam_new)))

toc.rn <- 
  ggplot(data = filter(adult.sum.nosex, State == "Tocantins"), aes(x = Temp_fac, y = mean.sLL, 
                                                                   group = interaction(Fam_new)))+
  geom_point(aes(color = interaction(Fam_new)), size = 2)+
  geom_path(aes(color = interaction(Fam_new)))

rio.rn <- 
  ggplot(data = filter(adult.sum.nosex, State == "Rio de Janeiro"), aes(x = Temp_fac, y = mean.sLL, 
                                                                        group = interaction(Fam_new)))+
  geom_point(aes(color = interaction(Fam_new)), size = 2)+
  geom_path(aes(color = interaction(Fam_new)))

rod.rn <- 
  ggplot(data = filter(adult.sum.nosex, State == "Rondonia"), aes(x = Temp_fac, y = mean.sLL, 
                                                                  group = interaction(Fam_new)))+
  geom_point(aes(color = interaction(Fam_new)), size = 2)+
  geom_path(aes(color = interaction(Fam_new)))

plot_grid(am.rn, 
          toc.rn, 
          rio.rn, 
          rod.rn, ncol = 2)


ggplot(data = filter(adult.sum.nosex), aes(x = Temp_fac, y = mean.sLL, 
                                           group = interaction(Fam_new, State)))+
  geom_point(aes(color = interaction(State)), size = 3)+
  geom_path(aes(color = interaction(State)))




#-------------------------------------------------------------------------------------------------------------------------#
# survival data

lifehistshort<-read.csv("C:\\Users\\Tim\\Desktop\\mosquito\\2018_03_28 Brazil adar16 Life history full.csv", header=TRUE)
lifehistshort$Pop<-paste(lifehistshort$Biome, lifehistshort$Lat_group)
lifehistshort$Temp_fac<-factor(lifehistshort$Temp_num)
lifehistshort$Lat_fac<-factor(lifehistshort$Latitude)

head(lifehistshort)


life_count<-read.csv("C:\Users\\Tim\\Desktop\\mosquito\\2018_03_28 Life hist totals for proportion survived.csv", header=TRUE)

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

market.size <- rep(n,800)


  
deadoralive$n <- deadoralive$true.dead+deadoralive$sum.alive
deadoralive$true.dead
deadoralive$sum.alive
deadoralive$prop.dead <- deadoralive$true.dead/deadoralive$n

bin.1 <- brm(true.dead| trials(n) ~ Temp_fac, data = deadoralive,
          family = binomial("probit"))

summary(bin.1)
plot(bin.1)
plot(marginal_effects(bin.1), points = TRUE)

bin.2 <- brm(true.dead| trials(n) ~ Temp_fac*State, data = deadoralive,
             family = binomial("probit"))

bin.3 <- brm(true.dead| trials(n) ~ State, data = deadoralive,
             family = binomial("probit"))



summary(bin.2)
summary(bin.3)

LOO(bin.1, bin.2, bin.3)

plot(marginal_effects(bin.2))
plot(marginal_effects(bin.3))
summary(bin.2)


ggplot()+
  geom_point(data = deadoralive, aes(x = mean.Temp, y = prop.dead, color = State))+
  geom_smooth(data = deadoralive, aes(x = mean.Temp, y = prop.dead, color = State), method = "lm", se = F)


## Probit regression using the binomial family
n <- sample(1:10, 100, TRUE) # number of trials
success <- rbinom(100, size = n, prob = 0.4)
x <- rnorm(100)
data2 <- data.frame(n, success, x)
fit2 <- brm(success | trials(n) ~ x, data = data2,
            family = binomial("probit"))
summary(fit2)
## Survival regressi


