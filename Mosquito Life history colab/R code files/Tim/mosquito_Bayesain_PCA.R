# mosquito Bayesain analysis with PCA

library(brms)
library(dplyr)
library(ggplot2)
library(lme4)
library(raster)

#read in and check data
adult <- read.csv("C:\\Users\\Tim\\Desktop\\mosquito\\2018_03_07 Life history adult data.csv")
head(adult)
str(adult)
scatterplotMatrix(~sLL+Biome+Pop+Temp_num, data = adult)

# initial plot
ggplot(data = adult, aes(x = Temp_num, y = sLL, color = Biome))+
  geom_jitter( size = 3)+
  geom_smooth(method = "lm")


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



# brms code
library(brms)
adult.sum.nosex$Lat.fac <- as.factor(adult.sum.nosex$mean.Lat)


# no group-level effect (Random Effect)
get_prior(mean.sLL~ Temp_fac + State, data = adult.sum.nosex)

brm1 <- brm(mean.sLL~ Temp_fac + State, data = adult.sum.nosex,
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
res.pca <- PCA(comb.points.env[, c(3:22)], quali.sup = 1)
plot(res.pca)

res.pca.data <- res.pca$ind$coord %>% data.frame

comb.points.env$PC1 <- res.pca.data$Dim.1
comb.points.env$PC2 <- res.pca.data$Dim.2

library(ggplot2)
library(cowplot)

ggplot(data = comb.points.env, aes(x = PC1, y = PC2, color = state, fill = state))+
  geom_point(size = 4)+
  stat_ellipse(geom = "polygon", alpha = 0.5)


library(brms)
brm.am <- brm(mean.sLL~mean.Temp, data = filter(adult.sum.nosex, State == "Amazonas"))
summary(brm.am)

brm.toc <- brm(mean.sLL~mean.Temp, data = filter(adult.sum.nosex, State == "Tocantins"))
summary(brm.toc)

brm.rio <- brm(mean.sLL~mean.Temp, data = filter(adult.sum.nosex, State == "Rio de Janeiro"))
summary(brm.rio)

brm.ron <- brm(mean.sLL~mean.Temp, data = filter(adult.sum.nosex, State == "Rondonia"))
summary(brm.ron)

summary(lm(mean.sLL~mean.Temp, data = filter(adult.sum.nosex, State == "Amazonas")))
