Scotland<-readRDS("C:/Users/mcherrie/Downloads/testOGR/ScotlandOLD.rds")

object.size(Scotland)

#library(rgeos)
#Scotland2<-gSimplify(Scotland, 20, topologyPreserve = T)

Scotland2 <- rmapshaper::ms_simplify(Scotland, keep = 0.2, keep_shapes=T)

subset <- Scotland@data

spdf <- SpatialPolygonsDataFrame(Scotland2, data =subset)

object.size(spdf)

spdf@data[4:10]<-NA
spdf@data[12:15]<-NA


saveRDS(spdf,"C:/Users/mcherrie/Downloads/testOGR/Scotland.rds")



writeOGR(spdf, "C:/Users/mcherrie/Downloads/testOGR", "Scotland3", driver="ESRI Shapefile")