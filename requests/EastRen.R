# 
ER<-readRDS("C:/Users/mcherrie/Google Drive/ProjectFiles/CRESHwebmap/alcoholtobacco/geography/DZ/la/East Renfrewshire.rds")

ERdata<-read.csv("C:/Users/mcherrie/Downloads/ER.csv")

colnames(ERdata)[7]<-"Density"
colnames(ERdata)[13]<-"Deprivation"
colnames(ERdata)[9]<-"Mortality"
colnames(ERdata)[10]<-"Hospitalisations"
colnames(ERdata)[14]<-"Crime"

ERdata$Deprivation<-6-(ERdata$Deprivation)

`East Ren.`<-merge(ER, ERdata, by.x="datazone", by.y="DZ_2011_code")


library(mapview)

mapviewOptions( basemaps = c("OpenStreetMap.Mapnik"))

m1<-mapview(`East Ren.`, zcol="Density")

m2<-mapview(`East Ren.`, zcol="Hospitalisations")

m3<-mapview(`East Ren.`, zcol="Mortality")

m4<-mapview(`East Ren.`, zcol="Deprivation")

m5<-sync(m1,m2,m3,m4)