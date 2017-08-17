#so dirty for R
#The idea is that I have a file that has coordinates for 241 wings of mosquitoes. 
#I want to calculate the centroid size which I have found code that requires it be in a matrix
#The issues I have had is that I can't seem to loop through the file to make this calculation
#and ideally it would append the results to the original or new file.


#!!setwd("C:/Users/vmc04/Dropbox/R code")
#read file
try13<- read.csv("13 landmarks field.csv", header=TRUE)
#make data fram
dtry13<-data.frame(try13)
#scale into mm
dtry13[,8:20]<-(dtry13[,8:20]/306.1)
#make matrix
mtry13<-as.matrix(dtry13)

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


#### all that above is your old stuff. I'll propose the following:
#### 

setwd("C:/Users/vmc04/Dropbox/R code")
#read file
try13<- read.csv("13 landmarks field.csv", header=TRUE)
#make data fram
dtry13<-data.frame(try13)
#scale into mm
dtry13[,8:20]<-(dtry13[,8:20]/306.1)


rowvar <- function(row) {
  # takes a row of the data frame and compute variance of the L1-L13 components
  var(row[8:20])
}

dtry13$vars = apply(dtry13,1,rowvar) # compute variance for each row

# now create table with centroid size for each Code
# (so take sum, then multiple by dim-1 (i.e., 13-1=12), then square root)

centtab = tapply(dtry13$vars,dtry13$Code,function(x) {sqrt(sum(x)*12)})

# now need to put this centroid table data back onto the data frame!

dtry13$cent_size = centtab[dtry13$Code]

x<-aggregate(dtry13$cent_size, list(loc=dtry13$Locality),length)
x1<-aggregate(dtry13$cent_size, list(loc=dtry13$Locality),mean)
x2<-aggregate(dtry13$cent_size, list(loc=dtry13$Locality),sd)

dtry13$Locality=factor(dtry13$Locality, c("ARS","APR","RPV","RMO","TLC","TPN","SJU"))

bartlett.test(cent_size~Biome, data=dtry13)

wing.morf=aov(cent_size ~ Locality, data=dtry13)
summary(wing.morf)

wingmorf<-TukeyHSD(wing.morf)
plot(wingmorf)

print(HSD.test(wing.morf, 'Locality'))

# if you want, you could also throw on the scaled columns, but that's up to you. Or you could (if you are careful) just modify the original columns by dividing by the cent_size column!

# The moral of the story is that tables (and tapply) can be miracle workers :)

try18<- read.csv("18 landmarks field.csv", header=TRUE)
#make data fram
dtry18<-data.frame(try18)
#scale into mm
dtry18[,7:19]<-(dtry18[,7:19]/306.1)


rowvar <- function(row) {
  # takes a row of the data frame and compute variance of the L1-L13 components
  var(row[7:19])
}

dtry18$vars = apply(dtry18,1,rowvar) # compute variance for each row

# now create table with centroid size for each Code
# (so take sum, then multiple by dim-1 (i.e., 13-1=12), then square root)

centtab = tapply(dtry18$vars,dtry18$Code,function(x) {sqrt(sum(x)*12)})

# now need to put this centroid table data back onto the data frame!

dtry18$cent_size = centtab[dtry18$Code]

x8<-aggregate(dtry18$cent_size, list(loc=dtry18$Locality),length)
x81<-aggregate(dtry18$cent_size, list(loc=dtry18$Locality),mean)
x82<-aggregate(dtry18$cent_size, list(loc=dtry18$Locality),sd)

dtry18$Locality=factor(dtry18$Locality, c("ARS","APR","RPV","RMO","TLC","TPN","SJU"))

bartlett.test(cent_size~Biome, data=dtry18)

wing.morf1=aov(cent_size ~ Locality, data=dtry18)
summary(wing.morf1)

wingmorf1<-TukeyHSD(wing.morf1)
plot(wingmorf1)

print(HSD.test(wing.morf1, 'Locality'))
