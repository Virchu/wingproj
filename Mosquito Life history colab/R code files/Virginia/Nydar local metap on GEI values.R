# metaP testing of GEI values

#resources
#https://cran.r-project.org/web/packages/metap/metap.pdf
#https://cran.r-project.org/web/packages/metap/vignettes/compare.pdf
#https://cran.r-project.org/web/packages/metap/vignettes/metap.pdf

library(metap)

#created dataframe
locality<-c('ARS','APR','RPV','RMO','TLC','TPN','SJU')
larv<-c(0.11,.00641,.108,.1079,.00666,.438,.115)
adult<-c(.019,.000000000656, .209,.708,.85,.8287,.246)
wing<-c(.0816,.07758,.0624,.035,.306,.0934,.0205)
GEI.data<-data.frame(locality,larv,adult,wing)

#looking at Fischers result (sumlog)
allmetap(GEI.data$larv, method="all")
#sumlog=0.00031417
allmetap(GEI.data$adult, method="all")
#sumlog=3.14e-7
allmetap(GEI.data$wing, method="all")
#sumlog=0.00067311