#Virginia bioclim PCA

adult <- read.csv
# do PCA on env for each state

#get state shapefile
library(rgdal)
library(gdata)
library(dplyr)
library(raster)
states <- readOGR(dsn = "C:\\Users\\virgc\\Desktop\\br_unidades_da_federacao", layer = "BRUFE250GC_SIR")
#work comp
states <- readOGR(dsn = "C:\\Users\\vmc04\\Desktop\\br_unidades_da_federacao", layer = "BRUFE250GC_SIR")

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
write.csv(comb.points, file = "LH regional PCA data points.csv",row.names=FALSE)

#used saved points from initial PCA from June 3, 2018
comb.points<- read.csv("C:\\Users\\virgc\\Documents\\GitHub\\wingproj\\Mosquito Life history colab\\R code files\\Virginia\\LH regional PCA data points.csv")
plot(states)
plot(states.sub, col = "purple" ,add = TRUE)
points(am.points)
points(toc.points)
points(rio.points)
points(ron.points)

# get worldclim data
# as .bil?
#bio1 <- raster("D:\\bioclim\\bio1-9_30s_bil\\bio_1.bil")

#home comp
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

#work comp
bio1 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim 30secbil\\bio_1.bil")
bio2 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim 30secbil\\bio_2.bil")
bio3 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim 30secbil\\bio_3.bil")
bio4 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim 30secbil\\bio_4.bil")
bio5 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim 30secbil\\bio_5.bil")
bio6 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim 30secbil\\bio_6.bil")
bio7 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim 30secbil\\bio_7.bil")
bio8 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim 30secbil\\bio_8.bil")
bio9 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim 30secbil\\bio_9.bil")

bio10 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim 30secbil\\bio_10.bil")
bio11 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim 30secbil\\bio_11.bil")
bio12 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim 30secbil\\bio_12.bil")
bio13 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim 30secbil\\bio_13.bil")
bio14 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim 30secbil\\bio_14.bil")
bio15 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim 30secbil\\bio_15.bil")
bio16 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim 30secbil\\bio_16.bil")
bio17 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim 30secbil\\bio_17.bil")
bio18 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim 30secbil\\bio_18.bil")
bio19 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim 30secbil\\bio_19.bil")

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

present<-ggplot(data = comb.points.env, aes(x = PC1, y = PC2, color = state, fill = state))+
  geom_point(size = 4)+
  xlab("PC1- 51.21%")+ ylab("PC2-27.7%")+
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

#lab comp
fut_bio1 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi701.tif")
fut_bio2 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi702.tif")
fut_bio3 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi703.tif")
fut_bio4 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi704.tif")
fut_bio5 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi705.tif")
fut_bio6 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi706.tif")
fut_bio7 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi707.tif")
fut_bio8 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi708.tif")
fut_bio9 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi709.tif")

fut_bio10 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi7010.tif")
fut_bio11 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi7011.tif")
fut_bio12 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi7012.tif")
fut_bio13 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi7013.tif")
fut_bio14 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi7014.tif")
fut_bio15 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi7015.tif")
fut_bio16 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi7016.tif")
fut_bio17 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi7017.tif")
fut_bio18 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi7018.tif")
fut_bio19 <- raster("C:\\Users\\vmc04\\Desktop\\Bioclim70 gs85bi GISS\\gs85bi7019.tif")

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

future<-ggplot(data = fut.comb.points.env, aes(x = PC1, y = PC2, color = state, fill = state))+
  geom_point(size = 4)+
  xlab("PC1- 47.46%")+ ylab("PC2-32.63%")+
  ggtitle("Future (2070) bioclimatic PCA")+
  stat_ellipse(geom = "polygon", alpha = 0.5)
#renaming so same variable
library(data.table)
setnames(fut.comb.points.env, old=c("gs85bi701" , "gs85bi702" , "gs85bi703" , "gs85bi704",  "gs85bi705" , "gs85bi706" ,
                                    "gs85bi707" , "gs85bi708" , "gs85bi709" , "gs85bi7010", "gs85bi7011", "gs85bi7012",
                                    "gs85bi7013", "gs85bi7014", "gs85bi7015",
                                    "gs85bi7016", "gs85bi7017", "gs85bi7018" ,"gs85bi7019"),new=c("bio_1" , "bio_2" ,
                                                                                                  "bio_3" , "bio_4" , "bio_5"  ,"bio_6",  "bio_7",  "bio_8",  "bio_9",  "bio_10",
                                                                                                  "bio_11" ,"bio_12", "bio_13", "bio_14", "bio_15", "bio_16", "bio_17", "bio_18" ,"bio_19"))

###composite PCA
comb.points.env$year<- 1990
fut.comb.points.env$year<-2070
all.points.env<- merge(comb.points.env, fut.comb.points.env, by="x")
#remove extra columns

head(all.points.env)
all.pca <- PCA(all.points.env[, c(3:22,28:46)], quali.sup = 1)
plot(all.pca)

all.pca.data <- all.pca$ind$coord %>% data.frame

all.points.env$PC1all <- all.pca.data$Dim.1
all.points.env$PC2all <- all.pca.data$Dim.2

all<-ggplot(data = all.points.env, aes(x = PC1all, y = PC2all, color = state.x, fill = state.x))+
  geom_point(size = 4)+
  ggtitle("Bioclimatic PCA (1990 and 2070)")+
  xlab("PC1- 48.46%")+ ylab("PC2-30.32%")+labs(colour="State", fill="State")+
  stat_ellipse(geom = "polygon", alpha = 0.5)

ggarrange(present, future, all,labels= c("A","B","C"), ncol=1,nrow=3, common.legend = TRUE, legend="bottom")


#reshaping data from all.points.env into new pca per Tim (7/3) suggestions
#adding year
comb.copy<-comb.points.env
fut.copy<-fut.comb.points.env

new.1990.comb.points<-melt(comb.copy, id=c("x", "y", "state","year", "PC1", "PC2"))
new.2070.comb.points<-melt(fut.copy, id=c("x", "y", "state","year", "PC1", "PC2"))
new.all.comb.points<-rbind(new.1990.comb.points, new.2070.comb.points, by=c("x", "y", "variable", "state", "PC1", "PC2"))
#trim off bottom row
new.all.comb.points<-new.all.comb.points[c(1:15200),c(1:8)]

#long to wide?
library(tidyr)
new.all.comb.points.wide<- spread(new.all.comb.points, variable, value)

#change from character to numeric
new.all.comb.points.wide [,c(1:2, 5:25)]<-sapply(new.all.comb.points.wide [,c(1:2, 5:25)], as.numeric)
new.all.comb.points.wide$label<-paste(new.all.comb.points.wide$state, new.all.comb.points.wide$year, sep=" ")
new.all.comb.points.wide$label<-factor(new.all.comb.points.wide$label, levels=c("Amazonas 1990", "Rondonia 1990", "Tocantins 1990", 
                                                                                "Rio de Janeiro 1990","Amazonas 2070", 
                                                                                "Rondonia 2070", "Tocantins 2070", 
                                                                                "Rio de Janeiro 2070" ))
new.all.comb.points.wide$state<-factor(new.all.comb.points.wide$state, levels=c("Amazonas", "Rondonia", "Tocantins", "Rio de Janeiro"))
new.all.pca <- PCA(new.all.comb.points.wide[, 7:25], quali.sup = 1)
plot(new.all.pca)

new.all.pca.data <- new.all.pca$ind$coord %>% data.frame

new.all.comb.points.wide$PC1all <- all.pca.data$Dim.1
new.all.comb.points.wide$PC2all <- all.pca.data$Dim.2

new.all2<-ggplot(data = new.all.comb.points.wide, aes(x = PC1, y = PC2, color = state, fill = year))+
  geom_point(size = 4)+
  geom_text(aes(label=year), nudge_y=.4)+
  ggtitle("Bioclimatic PCA (1990 and 2070)")+
  xlab("PC1- 48.46%")+ ylab("PC2-30.32%")+
  stat_ellipse(geom = "polygon", alpha = 0.5)

new.all4<-ggplot(data = new.all.comb.points.wide, aes(x = PC1, y = PC2, fill = state , shape = year ))+
  geom_point(size = 2.5,  aes(color = state ))+
  #geom_text(aes(label=year), nudge_y=.4)+
  ggtitle("Bioclimatic PCA (1990 and 2070)")+
  xlab("PC1- 48.46%")+ ylab("PC2-30.32%")+
  stat_ellipse(geom = "polygon", alpha = 0.5)

new.all5<-ggplot(data = new.all.comb.points.wide, aes(x = PC1all, y = PC2all, fill = state , shape = year ))+
  geom_point(size = 2.5,  aes(color = state ))+
  #geom_text(aes(label=label), nudge_y=.4)+
  ggtitle("Bioclimatic PCA (1990 and 2070)")+
  xlab("PC1- 41.262%")+ ylab("PC2-32.352%")+
  stat_ellipse(geom = "polygon", alpha = 0.5)
#get pca values
#https://www.youtube.com/watch?v=CTSbxU6KLbM&list=PLnZgp6epRBbTsZEFXi_p6W48HhNyqwxIu&index=3
summary(all.pca)
#PC1= 48.457%, PC2=30.318%
summary(fut.res.pca)
#PC1= 47.456%, PC2=32.633%
summary(res.pca)
#PC1= 51.205%, PC2=27.7%
summary(new.all.pca)
#PC1= 41.262%, PC2=32.352%

#make impressive color variable factor
library(factoextra)
all.coord<-all.points.env[,c(1:3, 48,49)]
all.pca.cos2<- all.coord^2
fviz_pca_var(all.pca, col.var="contrib") +
  scale_color_gradient2(low="yellow", mid="blue", 
                        high="red", midpoint=2.4) + theme_minimal()
fviz_pca_biplot(all.pca, label="var", habillage=all.points.env$state.x,
                addEllipses = TRUE, ellipes.level=0.9)

#new 7/3/18
new.all.coord<-new.all.comb.points.wide[,c(1:3, 48,49)]
comb.pca <- PCA(comb.copy[, c(3:22)], quali.sup = 1)
fut.pca <- PCA(fut.copy[, c(3:22)], quali.sup = 1)

fviz_pca_var(comb.pca, col.var="contrib") +
  scale_color_gradient2(low="yellow", mid="blue", 
                        high="red", midpoint=2.4) + theme_minimal()
fviz_pca_biplot(comb.pca, label="var", habillage=all.points.env$state.x,
                addEllipses = TRUE, ellipes.level=0.9)
fviz_pca_biplot(fut.pca, label="var", habillage=all.points.env$state.x,
                addEllipses = TRUE, ellipes.level=0.9)

#new 7/5/18
fviz_pca_var(new.all.pca, col.var="contrib") +
  scale_color_gradient2(low="yellow", mid="blue", 
                        high="red", midpoint=2.4) + theme_minimal()
fviz_pca_ind(new.all.pca, label="none", habillage=new.all.comb.points.wide$label,
             addEllipses = TRUE, ellipes.level=0.9)
fviz_pca_biplot(new.all.pca, label="var", habillage=new.all.comb.points.wide$label,
                addEllipses = TRUE, ellipes.level=0.9)

               