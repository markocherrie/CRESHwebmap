Scotland<-readRDS("C:/Users/mcherrie/Downloads/testOGR/ScotlandOLD3.rds")

object.size(Scotland)

#library(rgeos)
#Scotland2<-gSimplify(Scotland, 20, topologyPreserve = T)

Scotland2 <- rmapshaper::ms_simplify(Scotland, keep = 0.01, keep_shapes=T)

object.size(Scotland2)

subset <- Scotland@data

spdf <- SpatialPolygonsDataFrame(Scotland2, data =subset)

spdf@data[4:9]<-NA
spdf@data[11:15]<-NA


saveRDS(spdf,"C:/Users/mcherrie/Downloads/testOGR/Scotland3.rds")



writeOGR(spdf, "C:/Users/mcherrie/Downloads/testOGR", "Scotland2", driver="ESRI Shapefile")