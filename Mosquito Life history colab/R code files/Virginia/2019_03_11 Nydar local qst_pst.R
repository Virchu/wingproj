#Qst for life history local manuscript
library(lme4)
library(Pstat)

setwd("C:\\Users\\vmc04\\Documents\\GitHub\\wingproj\\Mosquito Life history colab\\Life history local")


##Input data files
#raw complete data set
all_data <- read.csv("C:\\Users\\vmc04\\Documents\\GitHub\\wingproj\\Mosquito Life history colab\\Data files\\2018_03_28 Brazil adar16 Life history full.csv")
#laptop
all_data <- read.csv("C:\\Users\\virgc\\GitHub\\wingproj\\Mosquito Life history colab\\Data files\\2018_03_28 Brazil adar16 Life history full.csv")
#factor
all_data$State<-factor(all_data$State, levels=c("Amazonas", "Rondonia", "Tocantins", "Rio de Janeiro"))
all_data$Locality<-factor(all_data$Locality, levels=c("ARS", "APR", "RPV", "RMO","TLC","TPN", "SJU"))

#adult only data set
adult<-subset(all_data, all_data$Death_stat==1 & !is.na(all_data$Sex1))
adult$PL<- adult$Emtime-adult$sLL
adult$time<-adult$Emtime+adult$AL

adult$State<-factor(adult$State, levels=c("Amazonas", "Rondonia", "Tocantins", "Rio de Janeiro"))
adult$Locality<-factor(adult$Locality, levels=c("ARS", "APR", "RPV", "RMO","TLC","TPN", "SJU"))
adult$Temp_fac<-factor(adult$Temp_num, levels=c(20,24,28))

#short adult to remove columns with missing and wing length (which has some missing values)
adult_loc<-adult[, c(6,1:5,7,8,10,12:14,17,20:22)]
adult_state<-adult[, c(3,1,2,4:8,10,12:14,17,20:22)]

#pairwise pst within amazon biome
Pst(adult_state, csh=0.5, Pw=c("Amazonas","Rondonia"))
#Amazonas Rondonia 
#974     1055 
#Quant_Varia  Pst_Values
#1           ID 1.000000000
#2        Biome 1.000000000
#3     Latitude 0.999992796
#4    Lat_group 1.000000000
#5     Locality 1.000000000
#6       Family 0.990920214
#7      Fam_new 1.000000000
#8     Temp_num 0.280780461
#9         Sex1 0.475290041
#10         sLL 0.982164133
#10 Wing.length..mm.  0.7747010
#11      Emtime 0.975753000
#12          AL 0.811820377
#13        time 0.950740939
#14          PL 0.004829734
Pst(adult_state, csh=0.5)
#[1] "Populations sizes are:"
#Amazonas Rio de Janeiro       Rondonia      Tocantins 
#974            285           1055            338 
#Quant_Varia Pst_Values
#1           ID  1.0000000
#2        Biome  1.0000000
#3     Latitude  0.9999965
#4    Lat_group  1.0000000
#5     Locality  1.0000000
#6       Family  0.9958103
#7      Fam_new  1.0000000
#8     Temp_num  0.7459517
#9         Sex1  0.2617968
#10 Wing.length..mm.  0.9717519
#10         sLL  0.9842623
#11      Emtime  0.9798653
#12          AL  0.8847221
#13        time  0.9683424
#14          PL  0.8751648

Pst(adult_state, csh=0.2)
#[1] "Populations sizes are:"
#Amazonas Rio de Janeiro       Rondonia      Tocantins 
#974            285           1055            338 
#Quant_Varia Pst_Values
#1           ID  1.0000000
#2        Biome  1.0000000
#3     Latitude  0.9999913
#4    Lat_group  1.0000000
#5     Locality  1.0000000
#6       Family  0.9895912
#7      Fam_new  1.0000000
#8     Temp_num  0.5401249
#9         Sex1  0.1242330
#10 Wing.length..mm.  0.9322505
#10         sLL  0.9615631
#11      Emtime  0.9511389
#12          AL  0.7542922
#13        time  0.9244439
#14          PL  0.7371344
Pst(adult_loc, csh=0.2)
#APR ARS RMO RPV SJU TLC TPN 
#575 399 549 506 285 247  91 
#Quant_Varia Pst_Values
#1           ID  1.0000000
#2        Biome  1.0000000
#3        State  1.0000000
#4     Latitude  1.0000000
#5    Lat_group  1.0000000
#6       Family  0.9936098
#7      Fam_new  1.0000000
#8     Temp_num  0.3722663
#9         Sex1  0.0860342
#10 Wing.length..mm.  0.8788751
#10         sLL  0.9296521
#11      Emtime  0.9110010
#12          AL  0.6088723
#13        time  0.8656880
#14          PL  0.5836376

#
em<-BootPst(adult_loc, opt= "hist", va=13, bars=50)#emtime
sLL<-BootPst(adult_loc, opt= "hist", va=12, bars=50)#larv dev
AL<-BootPst(adult_loc, opt= "hist", va=14, bars=50)#adult
wing<-BootPst(adult_loc, opt= "hist", va=11, bars=50)#wing

adult_loc1<-adult_loc[,c(1,10:15)]
TracePst(adult_loc1)
trans_test=AitTrans(adult_loc1)
TracePst(trans_test, Fst=0.3, xm=3)