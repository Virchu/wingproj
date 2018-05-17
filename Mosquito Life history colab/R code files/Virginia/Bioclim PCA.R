#Virginia bioclim PCA

adult <- read.csv
# do PCA on env for each state

#get state shapefile
library(rgdal)
library(gdata)
library(dplyr)
library(raster)
states <- readOGR(dsn = "C:\\Users\\virgc\\Desktop\\br_unidades_da_federacao", layer = "BRUFE250GC_SIR")
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
plot(states.sub, col = "green", add = TRUE)

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
# as .bil?
#bio1 <- raster("D:\\bioclim\\bio1-9_30s_bil\\bio_1.bil")

bio1 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim 30secbil\\bio_1.bil")
bio2 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim 30secbil\\bio_2.bil")
bio3 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim 30secbil\\bio_3.bil")
bio4 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim 30secbil\\bio_4.bil")
bio5 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim 30secbil\\bio_5.bil")
bio6 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim 30secbil\\bio_6.bil")
bio7 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim 30secbil\\bio_7.bil")
bio8 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim 30secbil\\bio_8.bil")
bio9 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim 30secbil\\bio_9.bil")

bio10 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim 30secbil\\bio_10.bil")
bio11 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim 30secbil\\bio_11.bil")
bio12 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim 30secbil\\bio_12.bil")
bio13 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim 30secbil\\bio_13.bil")
bio14 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim 30secbil\\bio_14.bil")
bio15 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim 30secbil\\bio_15.bil")
bio16 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim 30secbil\\bio_16.bil")
bio17 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim 30secbil\\bio_17.bil")
bio18 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim 30secbil\\bio_18.bil")
bio19 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim 30secbil\\bio_19.bil")


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
  ggtitle("Present (1960-1990) bioclimatic PCA")+
  stat_ellipse(geom = "polygon", alpha = 0.5)


#future projection
#Laporta and ALimi- 2070 RCP85, GISS-E2-R from NASA

fut_bio1 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi701.tif")
fut_bio2 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi702.tif")
fut_bio3 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi703.tif")
fut_bio4 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi704.tif")
fut_bio5 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi705.tif")
fut_bio6 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi706.tif")
fut_bio7 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi707.tif")
fut_bio8 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi708.tif")
fut_bio9 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi709.tif")

fut_bio10 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi7010.tif")
fut_bio11 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi7011.tif")
fut_bio12 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi7012.tif")
fut_bio13 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi7013.tif")
fut_bio14 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi7014.tif")
fut_bio15 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi7015.tif")
fut_bio16 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi7016.tif")
fut_bio17 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi7017.tif")
fut_bio18 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi7018.tif")
fut_bio19 <- raster("C:\\Users\\virgc\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi7019.tif")

future.stack <- stack(fut_bio1, fut_bio2, fut_bio3, fut_bio4, fut_bio5, fut_bio6, fut_bio7, fut_bio8, fut_bio9, fut_bio10, fut_bio11, fut_bio12, fut_bio13, fut_bio14, fut_bio15, 
                      fut_bio16, fut_bio17, fut_bio18, fut_bio19)

fut_env.data <- raster::extract(future.stack, comb.points[, c("x", "y")])
fut_env.data.s <-fut_env.data

fut.comb.points.env <- cbind(comb.points, fut_env.data)


head(fut.comb.points.env)
fut.res.pca <- PCA(fut.comb.points.env[, c(3:22)], quali.sup = 1)
plot(fut.res.pca)
summary(fut.res.pca)
fut.res.pca
fut.res.pca.data <- fut.res.pca$ind$coord %>% data.frame

fut.comb.points.env$PC1 <- fut.res.pca.data$Dim.1
fut.comb.points.env$PC2 <- fut.res.pca.data$Dim.2

library(ggplot2)
library(cowplot)

ggplot(data = fut.comb.points.env, aes(x = PC1, y = PC2, color = state, fill = state))+
  geom_point(size = 4)+
  ggtitle("Future (2070) bioclimatic PCA")+
  stat_ellipse(geom = "polygon", alpha = 0.5)


###different bioclim map
#https://adnguyen.github.io/demos/RasterPCA_demo.html
#libraries related to maps
library(sp)
library(raster)
library(maps)
library(mapdata)
library(RStoolbox)# rasterPCA function

#packages for reading in data
library(data.table)