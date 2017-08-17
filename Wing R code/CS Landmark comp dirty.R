#so dirty for R
#The idea is that I have a file that has coordinates for 241 wings of mosquitoes. 
#I want to calculate the centroid size which I have found code that requires it be in a matrix
#The issues I have had is that I can't seem to loop through the file to make this calculation
#and ideally it would append the results to the original or new file.


setwd("C:/Users/vmc04/Dropbox/R code")
setwd("C:/Users/virgc/Dropbox/Wing project/Files for CS calc")
#read file
try13<- read.csv("13 landmarks field.csv", header=TRUE)

fill18<- read.csv("18 Lab fixed 2 APR_ARS1.csv", header=TRUE)
#make data fram
dtry13<-data.frame(try13)
#scale into mm
dtry13[,8:20]<-(dtry13[,8:20]/306.10456)
#make matrix
mtry13<-as.matrix(dtry13)

fill18<- read.csv("18 Lab fixed 2 APR_ARS1.csv", header=TRUE)
dfill18<-data.frame(fill18)
dfill18[,3:20]<-(dfill18[,3:20]/306.10456)
mtry18<-as.matrix(dfill18)

#centroid size calculation
centsiz<-function(M) {p<-dim(M)[2]
size<-sqrt(sum(apply(M,1,var))*(p-1))
list("centroid_size"=size, "scaled"=M/size)}

#subsetting just the landmark data (it now has no identifie)
cut13demo<-mtry13[,c(8:20)]
#getting the landmark data for the first sample, laid out as x and Y by rows
M3<- cut13demo[c(1:2),]
class(M3)<-"numeric"
m1<-centsiz(M3)
write.table(m1, "clipboard", sep='\t', row.names=FALSE, col.names=FALSE)


fin13<- read.csv("13 landmarks field data.csv", header=TRUE)
fin13$Locality=factor(fin13$Locality, c("ARS","APR","RPV","RMO","TLC","TPN","SJU"))

bartlett.test(centroid_size~Biome, data=fin13)

wing.morf=aov(centroid_size ~ Locality, data=fin13)
summary(wing.morf)

wingmorf<-TukeyHSD(wing.morf)
plot(wingmorf)

print(HSD.test(wing.morf, 'Locality'))

x<-aggregate(fin13$centroid_size, list(loc=fin13$Locality),length)
x1<-aggregate(fin13$centroid_size, list(loc=fin13$Locality),mean)
x2<-aggregate(fin13$centroid_size, list(loc=fin13$Locality),sd)

generate_label_df<- function(HSD, fwing){
  Tukey.levels <- HSD[[fwing]][,3]
  Tukey.labels <- multcompLetters(Tukey.levels)['Letters']
  plot.labels <- names(Tukey.labels[['Letters']])
  boxplot.df<- ddply(fwinglength, fwing, function (x) max(fivenum(x$y))+ 0.2)
  
  plot.levels<-data.frame (plot.labels, labels= Tukey.labels[['Letters']], stringsAsFactors=FALSE)
  
  labels.df<-merge(plot.levels, boxplot.df, by.x = 'plot.labels', by.y = fwing, sort = FALSE)
  return(labels.df)
}
