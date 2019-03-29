library(lme4)
library(emmeans)
library(broom)
library(agricolae)

all_data <- read.csv("C:\\Users\\vmc04\\Documents\\GitHub\\wingproj\\Mosquito Life history colab\\Data files\\2018_03_28 Brazil adar16 Life history full.csv")
#factor
all_data$State<-factor(all_data$State, levels=c("Amazonas", "Rondonia", "Tocantins", "Rio de Janeiro"))
all_data$Locality<-factor(all_data$Locality, levels=c("ARS", "APR", "RPV", "RMO","TLC","TPN", "SJU"))

#adult only data set
adult<-subset(all_data, all_data$Death_stat==1 & !is.na(all_data$Sex1))
adult$PL<- adult$Emtime-adult$sLL
adult$time<-adult$Emtime+adult$AL
#factor
adult$State<-factor(adult$State, levels=c("Amazonas", "Rondonia", "Tocantins", "Rio de Janeiro"))
adult$Locality<-factor(adult$Locality, levels=c("ARS", "APR", "RPV", "RMO","TLC","TPN", "SJU"))
adult$Temp_fac<-factor(adult$Temp_num, levels=c(20,24,28))

#climate data from INMET and SEDAM, same used for the PCA previously
clim_dat_an<-read.csv("C:\\Users\\vmc04\\Documents\\GitHub\\wingproj\\Mosquito Life history colab\\Data files\\LH Nydar climate station annual data.csv")
clim_dat_mon<- read.csv("C:\\Users\\vmc04\\Documents\\GitHub\\wingproj\\Mosquito Life history colab\\Data files\\LH Nydar climate station 2016 data.csv")
clim_dat_tri_mon<- read.csv("C:\\Users\\vmc04\\Documents\\GitHub\\wingproj\\Mosquito Life history colab\\Data files\\LH Nydar climate station 2016 tri month data.csv")

#annual temp of site
an_temp<- clim_dat_an %>% group_by(State, Locality) %>% summarize(mean.temp= mean(Annual_mean_temp, na.rm=TRUE),
                                                                                          sd.temp=sd(Annual_mean_temp, na.rm=TRUE),
                                                                                          N.temp= length(Annual_mean_temp),
                                                                                          se.temp=sd.temp/sqrt(N.temp)) %>% data.frame

tri_mo_temp<- clim_dat_tri_mon %>% group_by(State, Locality) %>% summarize(mean.temp= mean(Temp, na.rm=TRUE),
                                                                  sd.temp=sd(Temp, na.rm=TRUE),
                                                                  N.temp= length(Temp),
                                                                  se.temp=sd.temp/sqrt(N.temp)) %>% data.frame
#degree day
#loc linear regression to get inputs for the NG equation
ARS_DD_lm<- lm(1/Emtime~Temp_num, subset(adult, Locality=="ARS"))
summary(ARS_DD_lm) #y=.002504X-.001367
ARS_m<-.002504
ARS_DD<- 1/ARS_m
ARS_tb<- -(-.001367/ARS_m)

APR_DD_lm<- lm(1/Emtime~Temp_num, subset(adult, Locality=="APR"))
summary(APR_DD_lm)#y=.002173x +.003457
APR_m<-.002173
APR_DD<- 1/APR_m
APR_tb<- -(.003457/APR_m)

RPV_DD_lm<- lm(1/Emtime~Temp_num, subset(adult, Locality=="RPV"))
summary(RPV_DD_lm)#y=.002143x +.001095
RPV_m<-.002143
RPV_DD<-1/RPV_m
RPV_tb<- -(.001095/RPV_m)

RMO_DD_lm<- lm(1/Emtime~Temp_num, subset(adult, Locality=="RMO"))
summary(RMO_DD_lm)#y=.002434x -.006784
RMO_m<-.002434
RMO_DD<-1/RMO_m
RMO_tb<- -(-0.006784/RMO_m)

TPN_DD_lm<- lm(1/Emtime~Temp_num, subset(adult, Locality=="TPN"))
summary(TPN_DD_lm)#y=0.0011789x + 0.0181808
TPN_m<-0.0011789
TPN_DD<-1/TPN_m
TPN_tb<- -(.0181808/TPN_m)

TLC_DD_lm<- lm(1/Emtime~Temp_num, subset(adult, Locality=="TLC"))
summary(TLC_DD_lm)#y=0.0016087x +0.0101904
TLC_m<-0.0016087
TLC_DD<-1/TLC_m
TLC_tb<- -(.0101904/TLC_m)

SJU_DD_lm<- lm(1/Emtime~Temp_num, subset(adult, Locality=="SJU"))
summary(SJU_DD_lm)#y= 0.002495x -0.012145
SJU_m<-0.002495
SJU_DD<-1/SJU_m
SJU_tb<- -(-.012145/SJU_m)


#NG=T(tc-tb)/K

#thermal day data of average emtime by locality with yearly average temp
therm_dat_field<-data.frame("Locality"=c("ARS", "APR","RPV","RMO","TLC","TPN","SJU"),
                            "Latitude"=c(-2.864,-3.028,-8.742,-9.223,-10.7,-10.796,-22.611),
                            "tc (Avg. year temp)"= c(27.61,27.71,25.7,25.6,27.35,27.54,21.2),
                            "m"= c(ARS_m, APR_m, RPV_m, RMO_m,  TLC_m,TPN_m, SJU_m),
                            "tb (base temp)"= c(ARS_tb, APR_tb, RPV_tb, RMO_tb, TLC_tb, TPN_tb, SJU_tb),
                            "K"=c(ARS_DD,APR_DD,RPV_DD,RMO_DD, TLC_DD,TPN_DD, SJU_DD),
                            "T (time in a year)"=365)
therm_dat_field$NG=(therm_dat_field$T*(therm_dat_field$tc-therm_dat_field$tb))/therm_dat_field$K

#table is in excel file- sheet "Locality yr temp"

#thermal day data of average emtime by locality with month of collection temperature
therm_dat_field_month<-data.frame("Locality"=c("ARS", "APR","RPV","RMO","TLC","TPN","SJU"),
                            "Latitude"=c(-2.864,-3.028,-8.742,-9.223,-10.7,-10.796,-22.611),
                            "tc (Avg. year temp)"= c(28.72,30,26.45,25.45,27.5,27.8,19.46),
                            "m"= c(ARS_m, APR_m, RPV_m, RMO_m,  TLC_m,TPN_m, SJU_m),
                            "tb (base temp)"= c(ARS_tb, APR_tb, RPV_tb, RMO_tb, TLC_tb, TPN_tb, SJU_tb),
                            "K"=c(ARS_DD,APR_DD,RPV_DD,RMO_DD, TLC_DD,TPN_DD, SJU_DD),
                            "T (time in a year)"=365)
therm_dat_field_month$NG=(therm_dat_field_month$T*(therm_dat_field_month$tc-therm_dat_field_month$tb))/therm_dat_field_month$K

#table is in excel file- sheet "Locality mo temp"

#write.table(therm_dat_field, "clipboard", sep="\t", row.names=FALSE)
#write.table(therm_dat_field_month, "clipboard", sep="\t", row.names=FALSE)

#NG summary dataframe and calculations by family to do anova and max boxplot
NG_calc_sum<- adult %>% group_by(Fam_new, Biome, State, Locality, Latitude) %>% summarize(mean.Emtime= mean(Emtime, na.rm=TRUE),
                                                                                          sd.Emtime=sd(Emtime, na.rm=TRUE),
                                                                                          N.Emtime= length(Emtime),
                                                                                          se.Emtime=sd.Emtime/sqrt(N.Emtime)) %>% data.frame
loc_temp<- data.frame("Locality"=c("ARS", "APR","RPV","RMO","TLC","TPN","SJU"),
                      "tc"= c(27.61,27.71,25.7,25.6,27.35,27.54,21.2))

NG_calc_dataframe<-adult %>% group_by(Fam_new)%>% 
  do(tidy(lm(1/Emtime~Temp_num, data= .)))%>% data.frame
NG_calc_dataframe1<-reshape(NG_calc_dataframe, idvar="Fam_new", timevar = "term", direction = "wide")

NG_calc_final<- left_join(NG_calc_sum, NG_calc_dataframe1, by="Fam_new")
NG_calc_final$K<- 1/NG_calc_final$estimate.Temp_num
NG_calc_final$tb<- -(NG_calc_final$`estimate.(Intercept)`/NG_calc_final$estimate.Temp_num)
NG_calc_final$T<-365
NG_calc_final$tc<- loc_temp$tc[match(NG_calc_final$Locality, loc_temp$Locality)]

NG_calc_final$NG<-(NG_calc_final$T*(NG_calc_final$tc-NG_calc_final$tb))/NG_calc_final$K

#write.table(NG_calc_final, "clipboard", sep="\t", row.names=FALSE)
#write.csv(NG_calc_final, "LH local NG family data.csv", row.names=FALSE)

#graph NG boxplot
NG_family<- ggplot (NG_calc_final, aes(x=abs(Latitude), y=NG))+
  geom_point(aes(color=Locality))+ggtitle("Number of Annual Generations by latitude")+
  scale_color_manual(values=c("#EE6677", "#AA3377", "#4477AA", "#66CCEE", "#228833", "#CCBB44","#000000"), aesthetics="colour" )

NG_lm<- lm(NG~Locality, data=NG_calc_final)
NG_ls<- lsmeans(NG_lm, "Locality", adjust="tukey")
NG_cld<- cld(NG_ls, alpha=0.05, Letters=letters, adjust="tukey", reverse=TRUE)

#import finished .csv
NG_calc_imp<- read.csv("C:\\Users\\vmc04\\Documents\\GitHub\\wingproj\\Mosquito Life history colab\\Life history local\\LH local NG family data.csv", header=TRUE)
NG_labels<- data.frame("Locality"=c("ARS", "APR","RPV","RMO","TLC","TPN","SJU"),
                       "Latitude"=c(-2.864,-3.028,-8.742,-9.223,-10.7,-10.796,-22.611),
                       "label"= c("a","b","c","c","cd","d","e"))
NG_calc_imp$Locality<-factor(NG_calc_imp$Locality, levels=c("ARS", "APR", "RPV", "RMO","TLC","TPN", "SJU"))

NG_calc_imp$labels<- NG_labels$label[match(NG_calc_imp$Locality, NG_labels$Locality)]

NG.summarized= NG_calc_imp %>% group_by(Latitude) %>% summarize(Max.NG=max(NG))
NG_labels$Locality<-factor(NG_labels$Locality, levels=c("ARS", "APR", "RPV", "RMO","TLC","TPN", "SJU"))
NG.summarized$labels<- NG_labels$label[match(NG.summarized$Latitude, NG_labels$Latitude)]
NG.summarized$Locality<- NG_labels$Locality[match(NG.summarized$Latitude, NG_labels$Latitude)]


NG_box<-ggplot(NG_calc_imp, aes(x=abs(Latitude), y=NG, fill=Locality))+ theme_minimal()+ geom_boxplot(width=2)+
  ggtitle("Number of Annual Generations by latitude")+
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_manual(values=c("#EE6677", "#AA3377", "#4477AA", "#66CCEE", "#228833", "#CCBB44","#000000"))+
  geom_text(data= NG.summarized, aes(label=labels, x= abs(Latitude),y= Max.NG + 0.5))

#jpeg("LH local NG boxplot.jpg", width = 450, height = 350)
#NG_box
#dev.off()
