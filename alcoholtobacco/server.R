library(shiny)
library(leaflet)
# geocoding shiny
require(RCurl)
require(RJSONIO)
require(plyr)
library(BAMMtools)
library(rgdal)
library(rgeos)
library(dplyr)
library(gtools)
library(jsonlite)
library(mongolite)
library(rgdal)
library(DT)
library(leaflet.extras)

# devtools::install_github("hrbrmstr/ipapi")
library(ipapi)

#source_gist("524eade46135f6348140")

### Load the data
#mydata<-read.csv("//home1.geos.ed.ac.uk/mcherrie/CRESH_MC/WEBMAP/CRESHMAP/data/webmapdata_testscript_Dec2016.csv")
#mydata<-readRDS("data/mydata.RDS")
#mydata[is.na(mydata)]<-"MISSING"


### sort out data- numeric issues
#locationgeog<-readRDS("//home1.geos.ed.ac.uk/mcherrie/CRESH_MC/WEBMAP/CRESHMAP/geography/datazone.rds")
#IG<-readOGR("//home1.geos.ed.ac.uk/mcherrie/CRESH_MC/WEBMAP/CRESHMAP/geography", "Councils_from_IG")
#locationgeog<-merge(locationgeog, mydata, by.x="zonecode", by.y="datazone")
#r.vals <- extract(IG, coordinates(locationgeog), small=TRUE)
#locationgeog@data <- data.frame(locationgeog@data, r.vals$NAME)
#list<-levels(locationgeog@data$laname)
#for (i in list){
#  output<-locationgeog[locationgeog@data$laname==i,]
#  saveRDS(output, paste0("geography/la/", i, ".rds"))
#}

### Load the geodata
#IG<-readOGR("//home1.geos.ed.ac.uk/mcherrie/CRESH_MC/WEBMAP/CRESHMAP/geography", "Councils_from_IG")
#SP<-readOGR("//home1.geos.ed.ac.uk/mcherrie/CRESH_MC/WEBMAP/CRESHMAP/geography", "Scotparlconst2011_fromDZ")
#geog<-readRDS("//home1.geos.ed.ac.uk/mcherrie/CRESH_MC/WEBMAP/CRESHMAP/geography/datazone.rds")

# Remember that I have simplified the polygons to reduce file size! Could have used rgeos but I am using QGISs

## load local authority shapefile
geog<-readRDS("geography/councilareas.rds")
#geog3 <- spTransform(geog, CRS("+proj=longlat +datum=WGS84"))
#geog2<-readOGR("geography", "LA")

# functions

####
BING <- function(str){
  u <- URLencode(paste0("http://dev.virtualearth.net/REST/v1/Locations?q=", str, "&maxResults=1&key=Apo4HssxpmYvVbDEUA464pmX5Y30xsQNlJ4pES6Z6D056puS63D90MLZlQ1yVeTG"))
  d <- getURL(u)
  j <- RJSONIO::fromJSON(d,simplify = FALSE) 
  if (j$resourceSets[[1]]$estimatedTotal > 0) {
    lat <- j$resourceSets[[1]]$resources[[1]]$point$coordinates[[1]]
    lng <- j$resourceSets[[1]]$resources[[1]]$point$coordinates[[2]]
  }
  else {    
    lat <- lng <- NA
  }
  data<-c(lat,lng)
  data[3]<-"BING"
  return(data)
}  


simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


sp.na.omit <- function(x, margin=2) {
  if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
    stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT") 
  na.index <- unique(as.data.frame(which(is.na(x@data),arr.ind=TRUE))[,margin])
  if(margin == 1) {  
    cat("DELETING ROWS: ", na.index, "\n") 
    return( x[-na.index,]  ) 
  }
  if(margin == 2) {  
    cat("DELETING COLUMNS: ", na.index, "\n") 
    return( x[,-na.index]  ) 
  }
}

# Sort out data type


shinyServer(function(input, output) {
  
  output$test=renderUI({ 
    if(input$checkbox_1==F){  
      list(
        selectInput("year", "Year",
                    list("2012" = "2012",
                         "2016" = "2016"),
                    selected="2016"),
        bsTooltip("buffer", "Here you can toggle the buffer size, that is, the distance you would expect people to travel to visit these retailers. Larger buffer sizes may be more appropriate for rural areas and smaller ones for urban areas.", "top"
        ),
        
        selectInput("buffer", "Buffer Size (m)",
                    list("400" = "400",
                         "800" = "800",
                         "1,000"  = "1000",
                         "1,200"  = "1200",
                         "1,600" = "1600",
                         "2,000" = "2000",
                         "3,000" = "3000",
                         "4,000" = "4000",
                         "5,000" = "5000"), selected="800"),
        bsTooltip("comparison", "Datazones can be ranked in comparison to the local authority and Scottish average. In addition you can see each datazone ranked in comparison to other datazones of similar urban/rural or deprivation status.", "top"
        ),
        selectInput("comparison", "Rank Colour Options",
                    list("Compared to Local Authority Average" = "LA",
                         "Compared to Scottish Average" = "SCO",
                         "Compared to Urban/Rural Average"  = "URBRUR",
                         "Compared to Deprivation Average"  = "SIMD"
                    ), selected="SCO"))}
    if(input$checkbox_1==T){  
      list(
        selectInput("year", "Year",
                    list("2012" = "2012",
                         "2016" = "2016"),
                    selected="2016"),
        bsTooltip("buffer", "Here you can toggle the buffer size, that is, the distance you would expect people to travel to visit these retailers. Larger buffer sizes may be more appropriate for rural areas and smaller ones for urban areas.", "top"
        ),
        selectInput("buffer", "Buffer Size (m)",
                    list("400" = "400",
                         "800" = "800",
                         "1,000"  = "1000",
                         "1,200"  = "1200",
                         "1,600" = "1600",
                         "2,000" = "2000",
                         "3,000" = "3000",
                         "4,000" = "4000",
                         "5,000" = "5000"), selected="800"),
        bsTooltip("comparison", "Datazones can be ranked in comparison to the local authority and Scottish average. In addition you can see each datazone ranked in comparison to other datazones of similar urban/rural or deprivation status.", "top"
        ),
        selectInput("comparison", "Rank Colour Options",
                    list("Compared to Local Authority Average" = "LA",
                         "Compared to Scottish Average" = "SCO",
                         "Compared to Urban/Rural Average"  = "URBRUR",
                         "Compared to Deprivation Average"  = "SIMD"
                    ), selected="SCO"))}
  }
  ) 
  
  
  ###### Change to say Lowest, Average and Highest in the legend
  
  pal <- colorNumeric(c("#d73027", "#ffffe5", "#5d8bba"), 1:7) 
  
  #### stick in geog2 below, so that people can see the local authority boundaries always??
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB Positron") %>%
      addProviderTiles("Stamen.Toner", group = "Toner") %>%
      addProviderTiles("OpenStreetMap.BlackAndWhite", group = "OSM") %>%
      setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
      addLayersControl(
        baseGroups = c("CartoDB Positron", "Toner", "OSM"),
        options = layersControlOptions(collapsed = TRUE)) %>%
      addLegend("bottomright", pal = pal, values=c(1:7),
                title = "Rank",
                labels= c("Lowest", "","","Average","","", "Highest"),
                opacity = 1
      ) %>%
      addScaleBar(position = c("bottomleft"))%>%
      addFullscreenControl() 
  })
  
  
  ##### LA input
  
  observe({
    
    if(!is.null(input$LAinput)){
      mapit <- leafletProxy("map") 
      mapit  %>% clearShapes() %>% clearMarkers() #%>%
      foo<-length(input$LAinput)
      if (foo==1 && input$LAinput!="Scotland"){
        datalist = list()
        Datazone<-readRDS(paste0("geography/DZ/la/", trimws(input$LAinput[1]), ".rds"))
        zoom<-read.csv("data/zoomlevels.csv")
        Datazone<-merge(Datazone, zoom, by="Council")
        zoomlevel<-unique(Datazone$zoomy)
        Datazone <- spTransform(Datazone, CRS("+proj=longlat +datum=WGS84"))
        long<- (Datazone@bbox[1,1]+Datazone@bbox[1,2])/2
        lat <-(Datazone@bbox[2,1]+Datazone@bbox[2,2])/2
        dat<-cbind(lat, long)  
        datalist[[1]] <- dat
        datalist<-data.frame(datalist)
        ### put in if statement for the larger LA's??????
        mapit %>% 
          setView(lng =  mean(as.numeric(as.character((datalist[,2])))), lat = mean(as.numeric(as.character((datalist[,1])))), zoom = zoomlevel)
        
      } else {
        mapit  %>%  clearMarkers()
      }
      
      ######### Getting proper names for graphs
      #names<-as.data.frame(matrix(c("Alcohol On Sales",
      #                              "Alcohol Off Sales",
      #                              "Alcohol Both On and Off Sales","Alcohol Total Sales", 
      #                              "Tobacco Total Sales", "alcoholOn", "alcoholOff", "alcoholBoth","alcoholTOTAL",
      #                              "tobaccoTOTAL"), ncol=2, nrow=5))
      #names(names)<-c("new", "old")
      #data1<-as.data.frame(input$dataype)
      #names(data1)<-"old"
      #names2<-merge(names, data1, by="old", all=F)
      #names3<-names2 
      ##################################
      
      
      #### to have default as advanced options hidden
      #if (!is.null(input$buffer)){
      #if (input$checkbox_1==T){
      #  data <- as.character(paste0(input$buffer, input$datatype, input$year, ".csv"))
      #} else { 
      #  data <- as.character(paste0("800", input$datatype, "2016", ".csv")) 
      #}
      
      for (i in input$LAinput){
        Datazone<-readRDS(paste0("geography/DZ/la/", trimws(i), ".rds"))
        Datazone <- spTransform(Datazone, CRS("+proj=longlat +datum=WGS84"))
        Datazone@data$respop2011<-NULL
        Datazone@data$stdareakm2<-NULL
        Datazone@data$shape_leng<-NULL
        data <- as.character(paste0(input$buffer, input$datatype, input$year, ".csv"))
        add<-read.csv(paste0("data/output/",data))
        Scotlandmean<-mean(add[,3])
        Datazone$Scottishaverage<-Scotlandmean
        Scottish90th<-quantile(add[,3], c(.90)) 
        add$CODE<-trimws(add$CODE)
        Datazone<-merge(Datazone, add, by.x="datazone", by.y="CODE")
        
        ############################# have to make the categories for SCOTTISH AVERAGE #############################
        
        Datazone@data$SCOcat[Datazone@data[,16]>=(Scotlandmean-0.2*Scotlandmean) & Datazone@data[,16]<=Scotlandmean+(0.15*Scotlandmean)]<-4
        Datazone2<-subset(Datazone, Datazone@data[,16]<(Scotlandmean-0.2*Scotlandmean))
        breaks1<-unique(quantile(Datazone2@data[,16], probs=0:3/3))
        Datazone@data$SCOcat2<-ifelse(Datazone@data[,16]<(Scotlandmean-0.2*Scotlandmean), cut(Datazone@data[,16], unique(breaks1), include.lowest=TRUE, labels=FALSE), NA)
        Datazone3<-subset(Datazone, Datazone@data[,16]>Scotlandmean+(0.15*Scotlandmean))
        breaks2<-unique(quantile(Datazone3@data[,16], probs=0:3/3))
        Datazone@data$SCOcat3<-ifelse(Datazone@data[,16]>Scotlandmean+(0.15*Scotlandmean), cut(Datazone@data[,16], unique(breaks2), include.lowest=TRUE, labels=FALSE), NA)
        Datazone@data$SCOcat3[Datazone@data$SCOcat3==1]<-5
        Datazone@data$SCOcat3[Datazone@data$SCOcat3==2]<-6
        Datazone@data$SCOcat3[Datazone@data$SCOcat3==3]<-7
        Datazone@data$SCOcat4<-paste(Datazone@data$SCOcat, Datazone@data$SCOcat2, Datazone@data$SCOcat3)
        Datazone@data$SCOcat4<-trimws(Datazone@data$SCOcat4)
        Datazone@data$SCOcat4<-sub("NA NA", "", Datazone@data$SCOcat4)
        Datazone@data$SCOcat4<-sub(" NA", "", Datazone@data$SCOcat4)
        Datazone@data$SCOcat4<-sub("NA ", "", Datazone@data$SCOcat4)
        Datazone@data$SCOcat4<-trimws(Datazone@data$SCOcat4)
        Datazone@data$SCOcat4<-as.numeric(Datazone@data$SCOcat4)
        
        ## too slow load up from file- simplify-
        # Datazone@data$LAid<-99
        # LAoutline <- gUnaryUnion(Datazone, id = Datazone@data$LAid)
        
        ###################################################################################################################
        ################## Urban Rural
        add2<-read.csv(paste0("data/urbanrural.csv"))
        UrbRur<-merge(add, add2, by.x="CODE", by.y="Datazone2011")
        UrbRur$UR6_2013_2014<-as.numeric(as.character(UrbRur$UR6_2013_2014))
        UrbRurmean <-aggregate(UrbRur[,3], by=list(UrbRur$UR6_2013_2014), FUN=mean, na.rm=TRUE)
        names(UrbRurmean)<-c("UR6_2013_2014", "UR6_2013_2014mean")
        UrbRurCalc<-merge(UrbRurmean, UrbRur, by="UR6_2013_2014")
        UrbRurCalc90<-aggregate(UrbRur[,3], by = list(UrbRur$UR6_2013_2014), FUN = function(x) quantile(x, probs = 0.90))
        names(UrbRurCalc90)<-c("UR6_2013_2014", "UR6_2013_201490")
        UrbRurCalc<-merge(UrbRurCalc, UrbRurCalc90, by="UR6_2013_2014")
        
        
        ###############
        UrbRurCalc$UrbRurcat[UrbRurCalc[,5]>=(UrbRurCalc$UR6_2013_2014mean-0.2*UrbRurCalc$UR6_2013_2014mean) & UrbRurCalc[,5]<=UrbRurCalc$UR6_2013_2014mean+(0.15*UrbRurCalc$UR6_2013_2014mean)]<-4
        UrbRurCalc2<-subset(UrbRurCalc, UrbRurCalc[,5]<(UrbRurCalc$UR6_2013_2014mean-0.2*UrbRurCalc$UR6_2013_2014mean))
        breaks5<-unique(quantile(UrbRurCalc2[,5], probs=0:3/3))
        UrbRurCalc$UrbRurcat2<-ifelse(UrbRurCalc[,5]<(UrbRurCalc$UR6_2013_2014mean-0.2*UrbRurCalc$UR6_2013_2014mean), cut(UrbRurCalc[,5], unique(breaks5), include.lowest=TRUE, labels=FALSE), NA)
        UrbRurCalc3<-subset(UrbRurCalc, UrbRurCalc[,5]>UrbRurCalc$UR6_2013_2014mean+(0.15*UrbRurCalc$UR6_2013_2014mean))
        breaks6<-unique(quantile(UrbRurCalc3[,5], probs=0:3/3))
        UrbRurCalc$UrbRurcat3<-ifelse(UrbRurCalc[,5]>UrbRurCalc$UR6_2013_2014mean+(0.15*UrbRurCalc$UR6_2013_2014mean), cut(UrbRurCalc[,5], unique(breaks6), include.lowest=TRUE, labels=FALSE), NA)
        UrbRurCalc$UrbRurcat3[UrbRurCalc$UrbRurcat3==1]<-5
        UrbRurCalc$UrbRurcat3[UrbRurCalc$UrbRurcat3==2]<-6
        UrbRurCalc$UrbRurcat3[UrbRurCalc$UrbRurcat3==3]<-7
        UrbRurCalc$UrbRurcat4<-paste(UrbRurCalc$UrbRurcat, UrbRurCalc$UrbRurcat2, UrbRurCalc$UrbRurcat3)
        UrbRurCalc$UrbRurcat4<-trimws(UrbRurCalc$UrbRurcat4)
        UrbRurCalc$UrbRurcat4<-sub("NA NA", "", UrbRurCalc$UrbRurcat4)
        UrbRurCalc$UrbRurcat4<-sub(" NA", "", UrbRurCalc$UrbRurcat4)
        UrbRurCalc$UrbRurcat4<-sub("NA ", "", UrbRurCalc$UrbRurcat4)
        UrbRurCalc$UrbRurcat4<-trimws(UrbRurCalc$UrbRurcat4)
        UrbRurCalc$UrbRurcat4<-as.numeric(UrbRurCalc$UrbRurcat4)
        UrbRurCalcAdd<-subset(UrbRurCalc, select=c("CODE", "UrbRurcat4", "UR6_2013_2014mean", "UR6_2013_201490", "UR6_2013_2014"))
        Datazone<-merge(Datazone, UrbRurCalcAdd,by.x="code", by.y="CODE")
        
        
        ###################################################################################################################
        ######################   SIMD
        add4<-read.csv(paste0("data/SIMD.csv"))
        SIMD<-merge(add, add4, by.x="CODE", by.y="Data_Zone")
        SIMD$SIMDrank5<-as.numeric(quantcut(as.numeric(SIMD$Income_domain_2016_rank), 5))
        SIMDmean <-aggregate(SIMD[,3], by=list(SIMD$SIMDrank5), FUN=mean, na.rm=TRUE)
        names(SIMDmean)<-c("SIMDrank5", "SIMDmean")
        SIMDCalc<-merge(SIMDmean, SIMD, by="SIMDrank5")
        SIMDCalc90<-aggregate(SIMD[,3], by = list(SIMD$SIMDrank5), FUN = function(x) quantile(x, probs = 0.90))
        names(SIMDCalc90)<-c("SIMDrank5", "SIMD90")
        SIMDCalc<-merge(SIMDCalc, SIMDCalc90, by="SIMDrank5")
        
        
        SIMDCalc$SIMDcat[SIMDCalc[,5]>=(SIMDCalc$SIMDmean-0.2*SIMDCalc$SIMDmean) & SIMDCalc[,5]<=SIMDCalc$SIMDmean+(0.15*SIMDCalc$SIMDmean)]<-4
        SIMDCalc2<-subset(SIMDCalc, SIMDCalc[,5]<(SIMDCalc$SIMDmean-0.2*SIMDCalc$SIMDmean))
        breaks5<-unique(quantile(SIMDCalc2[,5], probs=0:3/3))
        SIMDCalc$SIMDcat2<-ifelse(SIMDCalc[,5]<(SIMDCalc$SIMDmean-0.2*SIMDCalc$SIMDmean), cut(SIMDCalc[,5], unique(breaks5), include.lowest=TRUE, labels=FALSE), NA)
        SIMDCalc3<-subset(SIMDCalc, SIMDCalc[,5]>SIMDCalc$SIMDmean+(0.15*SIMDCalc$SIMDmean))
        breaks6<-unique(quantile(SIMDCalc3[,5], probs=0:3/3))
        SIMDCalc$SIMDcat3<-ifelse(SIMDCalc[,5]>SIMDCalc$SIMDmean+(0.15*SIMDCalc$SIMDmean), cut(SIMDCalc[,5], unique(breaks6), include.lowest=TRUE, labels=FALSE), NA)
        SIMDCalc$SIMDcat3[SIMDCalc$SIMDcat3==1]<-5
        SIMDCalc$SIMDcat3[SIMDCalc$SIMDcat3==2]<-6
        SIMDCalc$SIMDcat3[SIMDCalc$SIMDcat3==3]<-7
        SIMDCalc$SIMDcat4<-paste(SIMDCalc$SIMDcat, SIMDCalc$SIMDcat2, SIMDCalc$SIMDcat3)
        SIMDCalc$SIMDcat4<-trimws(SIMDCalc$SIMDcat4)
        SIMDCalc$SIMDcat4<-sub("NA NA", "", SIMDCalc$SIMDcat4)
        SIMDCalc$SIMDcat4<-sub(" NA", "", SIMDCalc$SIMDcat4)
        SIMDCalc$SIMDcat4<-sub("NA ", "", SIMDCalc$SIMDcat4)
        SIMDCalc$SIMDcat4<-trimws(SIMDCalc$SIMDcat4)
        SIMDCalc$SIMDcat4<-as.numeric(SIMDCalc$SIMDcat4)
        SIMDCalcAdd<-subset(SIMDCalc, select=c("CODE", "SIMDcat4", "SIMDmean", "SIMD90", "SIMDrank5"))
        Datazone<-merge(Datazone, SIMDCalcAdd,by.x="code", by.y="CODE")
        
        
        
        ############################# have to make the categories for LA AVERAGE #############################
        LAmean<-mean(Datazone@data[,16])
        LA90th<-quantile(Datazone@data[,16], c(.90)) 
        # have to make the categories
        Datazone@data$LAcat[Datazone@data[,16]>=(LAmean-0.2*LAmean) & Datazone@data[,16]<=LAmean+(0.15*LAmean)]<-4
        Datazone2<-subset(Datazone, Datazone@data[,16]<(LAmean-0.2*LAmean))
        breaks3<-unique(quantile(Datazone2@data[,16], probs=0:3/3))
        if (length(breaks3)>1 ){
          Datazone@data$LAcat2<-ifelse(Datazone@data[,16]<(LAmean-0.2*LAmean), cut(Datazone@data[,16], unique(breaks3), include.lowest=TRUE, labels=FALSE), NA)
        } else {
          Datazone@data$LAcat2[Datazone@data[,16]<(LAmean-0.2*LAmean)]<-1
        }
        Datazone3<-subset(Datazone, Datazone@data[,16]>LAmean+(0.15*LAmean))
        breaks4<-unique(quantile(Datazone3@data[,16], probs=0:3/3))
        if (length(breaks4)>1){
          Datazone@data$LAcat3<-ifelse(Datazone@data[,16]>LAmean+(0.15*LAmean), cut(Datazone@data[,16], unique(breaks4), include.lowest=TRUE, labels=FALSE), NA)
        } else {   Datazone@data$LAcat3<-1
        }
        Datazone@data$LAcat3[Datazone@data$LAcat3==1]<-5
        Datazone@data$LAcat3[Datazone@data$LAcat3==2]<-6
        Datazone@data$LAcat3[Datazone@data$LAcat3==3]<-7
        Datazone@data$LAcat4<-paste(Datazone@data$LAcat, Datazone@data$LAcat2, Datazone@data$LAcat3)
        Datazone@data$LAcat4<-trimws(Datazone@data$LAcat4)
        Datazone@data$LAcat4<-sub("NA NA", "", Datazone@data$LAcat4)
        Datazone@data$LAcat4<-sub(" NA", "", Datazone@data$LAcat4)
        Datazone@data$LAcat4<-sub("NA ", "", Datazone@data$LAcat4)
        Datazone@data$LAcat4<-trimws(Datazone@data$LAcat4)
        Datazone@data$LAcat4<-as.numeric(Datazone@data$LAcat4)
        ###################################################################################################################    
        
        ################## Hospital Admissions
        HospAdd<-read.csv(paste0("data/Hosp.csv"))
        Datazone<-merge(Datazone, HospAdd,by="code")
        
        ## Choices
        Bufferchoice<-input$buffer
        Datatypechoice<-input$datatype
        Yearchoice<-input$year
        Rankchoice<-input$comparison
        
        ## ugly but needed to get nicely formatted stuff for description in popup
        Datatypechoice<-sub("alcoholOff",  "Off Alcohol Sales", Datatypechoice)        
        Datatypechoice<-sub("alcoholBoth", "Both On and Off Alcohol Sales", Datatypechoice)    
        Datatypechoice<-sub("alcoholTOTAL", "Total Alcohol Sales", Datatypechoice)    
        Datatypechoice<-sub("tobaccoTOTAL", "Total Tobacco Sales", Datatypechoice)    
        
        Rankchoice<-sub("LA", "in comparison to the Local Authority average.", Rankchoice)        
        Rankchoice<-sub("SCO", "in comparison to the Scottish average.", Rankchoice)    
        Rankchoice<-sub("URBRUR", "in comparison to the Urban/Rural average.", Rankchoice)    
        Rankchoice<-sub("SIMD", "in comparison to the Deprivation average.", Rankchoice)    
        
        ###
        
        if(input$comparison=="SCO"){
          pal <- colorNumeric(c("#5d8bba", "#ffffe5", "#d73027"), 1:7)   
          
          ### superscript in leaflet
          
          popup <- paste0("<h3>", Datazone$name, "</h3><br>",
                          "<b> Description </b> </br>",
                          "This datazone is within the local authority of ", Datazone@data$Councilname,
                          ". You have selected to display data for ", Datatypechoice, " for ", Yearchoice, ", with the buffer size of ", Bufferchoice," m,"," and colours ", Rankchoice,
                          "</br></br><b>",
                          substr(Datatypechoice,7,14), " Outlet Density </b></br>",
                          "<ul><li>Density around the population centre is ", round(Datazone@data[,16], 2)," per km<sup>2</sup>","</li>",
                          "<br/>",
                          "<li>This is ", round((Datazone@data[,16]/Datazone$Scottishaverage*100),0), ifelse(round(Datazone@data[,16], 2)>round(Datazone$Scottishaverage, 2) ,"% higher than", "% of")," the Scottish average.</li>",
                          "<br/><li>",
                          ifelse(Datazone@data[,16]>Scottish90th, "<font color='#EE2C2C'>This datazone is in the top 10% of neighbourhoods in Scotland</font></li></ul>", "This datazone is not in the top 10% of neighbourhoods in Scotland</li></ul>"),
                          "<b> Health </b></br>",
                          "<ul><li>Hospital stays related to alcohol misuse: standardised ratio: ", Datazone@data$ALCOHOL, "</li></br>",
                          "<li>Proportion of population being prescribed drugs for anxiety, depression or psychosis: ", Datazone@data$DEPRESS,"</li></br>",
                          "<li>Standardised mortality ratio: ", Datazone@data$SMR,"</li>",
                          "<br/>",
                          "<b><a target='_blank' href='http://statistics.gov.scot/doc/statistical-geography/", Datazone$code,"'> Click here for more information available on this datazone</a></b>")
          
          #################
          mapit  %>%
            
            addPolygons(data=Datazone,
                        stroke=T,
                        weight=0.3,
                        smoothFactor = 0.2,
                        fillOpacity = 0.62,
                        popup=popup,
                        color= ~pal(SCOcat4),
                        highlightOptions = highlightOptions(color = "black", weight = 3,
                                                            bringToFront = TRUE)) 
        }
        else if(input$comparison=="LA"){
          pal <- colorNumeric(c("#5d8bba", "#ffffe5", "#d73027"), 1:7)   
          
          popup <- paste0("<h3>", Datazone$name, "</h3><br>",
                          "Density around the population centre is ", "<strong>", round(Datazone@data[,16], 2)," per km2","</strong>",
                          "<br/>",
                          "This is <strong>", round((Datazone@data[,16]/LAmean*100),0), ifelse(round(Datazone@data[,16], 2)>round(LAmean, 2) ,"% higher than</strong>", "%</strong> of")," the ", input$LAinput, " average",
                          "<br/>",
                          ifelse(Datazone@data[,16]>LA90th, paste0("<strong><font color='#EE2C2C'>This datazone is in the top 10% of neighbourhoods in ", input$LAinput, "</font></strong>"), ""),
                          "<br/>",
                          "<br/>",
                          " More information available on this datazone ", "<b><a target='_blank' href='http://statistics.gov.scot/doc/statistical-geography/", Datazone$code,"'>here</a></b>")
          
          mapit  %>%     
            addPolygons(data=Datazone,
                        stroke=TRUE,
                        weight=0.1,
                        smoothFactor = 0.2,
                        fillOpacity = 0.62,
                        popup=popup,
                        color= ~pal(LAcat4),
                        highlightOptions = highlightOptions(color = "black", weight = 3,
                                                            bringToFront = TRUE)) 
          
        }
        else if(input$comparison=="URBRUR"){
          pal <- colorNumeric(c("#5d8bba", "#ffffe5", "#d73027"), 1:7)   
          
          popup <- paste0("<h3>", Datazone$name, "</h3><br>",
                          "Density around the population centre is ", "<strong>", round(Datazone@data[,16], 2)," per km2","</strong>",
                          "<br/>",
                          "This is <strong>", round((Datazone@data[,16]/Datazone$UR6_2013_2014mean*100),0), ifelse(round(Datazone@data[,16], 2)>round(Datazone$UR6_2013_2014mean, 2) ,"% higher than</strong>", "%</strong> of")," the Urban Rural Classification (6-Fold) Group ", Datazone$UR6_2013_2014, " average",
                          "<br/>",
                          ifelse(Datazone@data[,16]>Datazone$UR6_2013_201490, paste0("<strong><font color='#EE2C2C'>This datazone is in the top 10% of neighbourhoods in the Urban Rural Classification (6-Fold) Group ", Datazone$UR6_2013_2014, "</font></strong>"), ""),
                          "<br/>",
                          "<br/>",
                          " More information available on this datazone ", "<b><a target='_blank' href='http://statistics.gov.scot/doc/statistical-geography/", Datazone$code,"'>here</a></b>")
          
          
          mapit %>% 
            #addPolygons(data=LAoutline,
            #            stroke=T,
            #            weight=3,
            #            color= "black",
            #            fillOpacity = 0) #%>%
            
            addPolygons(data=Datazone,
                        stroke=TRUE,
                        weight=0.1,
                        smoothFactor = 0.2,
                        fillOpacity = 0.62,
                        popup=popup,
                        color= ~pal(UrbRurcat4),
                        highlightOptions = highlightOptions(color = "black", weight = 3,
                                                            bringToFront = TRUE)) 
          
        }
        else if(input$comparison=="SIMD"){
          pal <- colorNumeric(c("#5d8bba", "#ffffe5", "#d73027"), 1:7)   
          
          ### superscript in leaflet
          popup <- paste0("<h3>", Datazone$name, "</h3><br>",
                          "Density around the population centre is ", "<strong>", round(Datazone@data[,16], 2)," per km2","</strong>",
                          "<br/>",
                          "This is <strong>", round((Datazone@data[,16]/Datazone$SIMDmean*100),0), ifelse(round(Datazone@data[,16], 2)>round(Datazone$SIMDmean, 2) ,"% higher than</strong>", "% of</strong>")," the Scottish Index of Multiple Deprivation Quintile Group ", Datazone$SIMDrank5, " average",
                          "<br/>",
                          ifelse(Datazone@data[,16]>Datazone$SIMD90, paste0("<strong><font color='#EE2C2C'>This datazone is in the top 10% of neighbourhoods in the Scottish Index of Multiple Deprivation Quintile Group ", Datazone$SIMDrank5, "</font></strong>"), ""),
                          "<br/>",
                          "<br/>",
                          " More information available on this datazone ", "<b><a target='_blank' href='http://statistics.gov.scot/doc/statistical-geography/", Datazone$code,"'>here</a></b>")
          
          #####################
          mapit  %>%
            #addPolygons(data=LAoutline,
            #            stroke=T,
            #            weight=3,
            #            color= "black",
            #            fillOpacity = 0) #%>%
            
            addPolygons(data=Datazone,
                        stroke=TRUE,
                        weight=0.1,
                        smoothFactor = 0.2,
                        fillOpacity = 0.62,
                        popup=popup,
                        color= ~pal(SIMDcat4),
                        highlightOptions = highlightOptions(color = "black", weight = 3,
                                                            bringToFront = TRUE)) 
          
        }
      }
      #  for(i in input$LAinput[1]){
      #    Datazone<-readRDS(paste0("geography/DZ/la/", trimws(i), ".rds"))
      #    Datazone <- spTransform(Datazone, CRS("+proj=longlat +datum=WGS84"))
      #    data <- as.character(paste0(input$buffer, input$datatype, 2012, ".csv"))
      #   data2 <- as.character(paste0(input$buffer, input$datatype, 2016, ".csv"))
      ##   add<-read.csv(paste0("data/output/",data))
      #    add$CODE<-trimws(add$CODE)
      #    add2<-read.csv(paste0("data/output/",data2))
      #    add2$CODE<-trimws(add2$CODE)
      #    Datazone<-merge(Datazone, add, by.x="datazone", by.y="CODE")
      #   Datazone<-merge(Datazone, add2, by.x="datazone", by.y="CODE")
      #    
      #    r <- raster(ncol=400, nrow=400)
      #    extent(r) <- extent(Datazone)
      #    rp <- rasterize(Datazone, r, Datazone@data[,16])
      #    rp2 <- rasterize(Datazone, r, Datazone@data[,18])
      #    m<-slideView(rp, rp2)
      #    htmlwidgets::saveWidget(m@map, "m.html")
      #   output$change <- renderMapview(m)
      # }
    }
    
    else{
      mapit <- leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7) 
    }
  })
  
  # Zoom in on user location if given
  
  
  
  ## need to clear it with a button!
  location2<-NA
  
  ### gets geolocation manually
  ### To DO: make it so that it zooms in, change up location of fitBounds
  ### Check this out:  http://www.r-graph-gallery.com/2017/03/14/4-tricks-for-working-with-r-leaflet-and-shiny/ 
  observe({
    observeEvent(input$goButton, {
      mapit <- leafletProxy("map") 
      mapit  %>% clearShapes() %>% clearMarkers()
      str <- as.character(paste0(input$str, ", Scotland"))
      map<-BING(str)
      
      if (!is.null(str)){
        lat<-map[1]
        long<-map[2]
        
        if (!is.na(lat)){
          mapit %>% 
            setView(lng =  long, lat = lat, zoom = 14)
          
        } else {
          mapit  %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7) 
        } 
        
        ## map 2 is the red dot location
        map2<-as.data.frame(cbind(long=as.numeric(long), lat=as.numeric(lat)))
        map<-SpatialPoints(cbind(as.numeric(long),as.numeric(lat)))
        proj4string(map) <- CRS("+proj=longlat +datum=WGS84")
        map<-spTransform(map, proj4string(geog))
        location<-over(map, geog , fn = NULL)
        location2<-location[1,2]
        
        observe({
          if (!is.na(location2)){
            mapit  %>% clearShapes() %>%  setView(lng =  long, lat = lat, zoom = 14)
            Datazone<-readRDS(paste0("geography/DZ/la/", trimws(location2), ".rds"))
            Datazone <- spTransform(Datazone, CRS("+proj=longlat +datum=WGS84"))
            # Datazone2 is the datazone location
            Datazone2 <- Datazone[location[1,2], ]
            buffer<-gBuffer(map, width = 3000)
            buffer<-spTransform(buffer, proj4string(Datazone))
            Datazone3 <-gIntersects(Datazone, buffer, byid = T)
            Datazone3<- as.data.frame(t(Datazone3))
            Datazone@data <-cbind(Datazone@data, Datazone3)
            Datazone<-Datazone[Datazone@data$buffer==T, ]
            
            
            ### get the data in
            data <- as.character(paste0(input$buffer, input$datatype, input$year, ".csv"))
            add<-read.csv(paste0("data/output/",data))
            Scotlandmean<-mean(add[,3])
            Datazone$Scottishaverage<-Scotlandmean
            Scottish90th<-quantile(add[,3], c(.90)) 
            add$CODE<-trimws(add$CODE)
            Datazone<-merge(Datazone, add, by.x="datazone", by.y="CODE")
            Datazone$stdareaha<-NULL
            Datazone$stdareakm2<-NULL
            Datazone$shape_leng<-NULL
            
            #### Mulitple options- generate variable that is scottish average
            ############################# have to make the categories for SCOTTISH AVERAGE #############################
            
            Datazone@data$SCOcat[Datazone@data[,16]>=(Scotlandmean-0.2*Scotlandmean) & Datazone@data[,16]<=Scotlandmean+(0.15*Scotlandmean)]<-4
            Datazone2<-subset(Datazone, Datazone@data[,16]<(Scotlandmean-0.2*Scotlandmean))
            breaks1<-unique(quantile(Datazone2@data[,16], probs=0:3/3))
            Datazone@data$SCOcat2<-ifelse(Datazone@data[,16]<(Scotlandmean-0.2*Scotlandmean), cut(Datazone@data[,16], unique(breaks1), include.lowest=TRUE, labels=FALSE), NA)
            Datazone3<-subset(Datazone, Datazone@data[,16]>Scotlandmean+(0.15*Scotlandmean))
            breaks2<-unique(quantile(Datazone3@data[,16], probs=0:3/3))
            Datazone@data$SCOcat3<-ifelse(Datazone@data[,16]>Scotlandmean+(0.15*Scotlandmean), cut(Datazone@data[,16], unique(breaks2), include.lowest=TRUE, labels=FALSE), NA)
            Datazone@data$SCOcat3[Datazone@data$SCOcat3==1]<-5
            Datazone@data$SCOcat3[Datazone@data$SCOcat3==2]<-6
            Datazone@data$SCOcat3[Datazone@data$SCOcat3==3]<-7
            Datazone@data$SCOcat4<-paste(Datazone@data$SCOcat, Datazone@data$SCOcat2, Datazone@data$SCOcat3)
            Datazone@data$SCOcat4<-trimws(Datazone@data$SCOcat4)
            Datazone@data$SCOcat4<-sub("NA NA", "", Datazone@data$SCOcat4)
            Datazone@data$SCOcat4<-sub(" NA", "", Datazone@data$SCOcat4)
            Datazone@data$SCOcat4<-sub("NA ", "", Datazone@data$SCOcat4)
            Datazone@data$SCOcat4<-trimws(Datazone@data$SCOcat4)
            Datazone@data$SCOcat4<-as.numeric(Datazone@data$SCOcat4)
            
            ## too slow load up from file- simplify-
            # Datazone@data$LAid<-99
            # LAoutline <- gUnaryUnion(Datazone, id = Datazone@data$LAid)
            
            ###################################################################################################################
            ################## Urban Rural
            add2<-read.csv(paste0("data/urbanrural.csv"))
            UrbRur<-merge(add, add2, by.x="CODE", by.y="Datazone2011")
            UrbRur$UR6_2013_2014<-as.numeric(as.character(UrbRur$UR6_2013_2014))
            UrbRurmean <-aggregate(UrbRur[,3], by=list(UrbRur$UR6_2013_2014), FUN=mean, na.rm=TRUE)
            names(UrbRurmean)<-c("UR6_2013_2014", "UR6_2013_2014mean")
            UrbRurCalc<-merge(UrbRurmean, UrbRur, by="UR6_2013_2014")
            UrbRurCalc90<-aggregate(UrbRur[,3], by = list(UrbRur$UR6_2013_2014), FUN = function(x) quantile(x, probs = 0.90))
            names(UrbRurCalc90)<-c("UR6_2013_2014", "UR6_2013_201490")
            UrbRurCalc<-merge(UrbRurCalc, UrbRurCalc90, by="UR6_2013_2014")
            
            
            ###############
            UrbRurCalc$UrbRurcat[UrbRurCalc[,5]>=(UrbRurCalc$UR6_2013_2014mean-0.2*UrbRurCalc$UR6_2013_2014mean) & UrbRurCalc[,5]<=UrbRurCalc$UR6_2013_2014mean+(0.15*UrbRurCalc$UR6_2013_2014mean)]<-4
            UrbRurCalc2<-subset(UrbRurCalc, UrbRurCalc[,5]<(UrbRurCalc$UR6_2013_2014mean-0.2*UrbRurCalc$UR6_2013_2014mean))
            breaks5<-unique(quantile(UrbRurCalc2[,5], probs=0:3/3))
            UrbRurCalc$UrbRurcat2<-ifelse(UrbRurCalc[,5]<(UrbRurCalc$UR6_2013_2014mean-0.2*UrbRurCalc$UR6_2013_2014mean), cut(UrbRurCalc[,5], unique(breaks5), include.lowest=TRUE, labels=FALSE), NA)
            UrbRurCalc3<-subset(UrbRurCalc, UrbRurCalc[,5]>UrbRurCalc$UR6_2013_2014mean+(0.15*UrbRurCalc$UR6_2013_2014mean))
            breaks6<-unique(quantile(UrbRurCalc3[,5], probs=0:3/3))
            UrbRurCalc$UrbRurcat3<-ifelse(UrbRurCalc[,5]>UrbRurCalc$UR6_2013_2014mean+(0.15*UrbRurCalc$UR6_2013_2014mean), cut(UrbRurCalc[,5], unique(breaks6), include.lowest=TRUE, labels=FALSE), NA)
            UrbRurCalc$UrbRurcat3[UrbRurCalc$UrbRurcat3==1]<-5
            UrbRurCalc$UrbRurcat3[UrbRurCalc$UrbRurcat3==2]<-6
            UrbRurCalc$UrbRurcat3[UrbRurCalc$UrbRurcat3==3]<-7
            UrbRurCalc$UrbRurcat4<-paste(UrbRurCalc$UrbRurcat, UrbRurCalc$UrbRurcat2, UrbRurCalc$UrbRurcat3)
            UrbRurCalc$UrbRurcat4<-trimws(UrbRurCalc$UrbRurcat4)
            UrbRurCalc$UrbRurcat4<-sub("NA NA", "", UrbRurCalc$UrbRurcat4)
            UrbRurCalc$UrbRurcat4<-sub(" NA", "", UrbRurCalc$UrbRurcat4)
            UrbRurCalc$UrbRurcat4<-sub("NA ", "", UrbRurCalc$UrbRurcat4)
            UrbRurCalc$UrbRurcat4<-trimws(UrbRurCalc$UrbRurcat4)
            UrbRurCalc$UrbRurcat4<-as.numeric(UrbRurCalc$UrbRurcat4)
            UrbRurCalcAdd<-subset(UrbRurCalc, select=c("CODE", "UrbRurcat4", "UR6_2013_2014mean", "UR6_2013_201490", "UR6_2013_2014"))
            Datazone<-merge(Datazone, UrbRurCalcAdd,by.x="code", by.y="CODE")
            
            
            ###################################################################################################################
            ######################   SIMD
            add4<-read.csv(paste0("data/SIMD.csv"))
            SIMD<-merge(add, add4, by.x="CODE", by.y="Data_Zone")
            SIMD$SIMDrank5<-as.numeric(quantcut(as.numeric(SIMD$Income_domain_2016_rank), 5))
            SIMDmean <-aggregate(SIMD[,3], by=list(SIMD$SIMDrank5), FUN=mean, na.rm=TRUE)
            names(SIMDmean)<-c("SIMDrank5", "SIMDmean")
            SIMDCalc<-merge(SIMDmean, SIMD, by="SIMDrank5")
            SIMDCalc90<-aggregate(SIMD[,3], by = list(SIMD$SIMDrank5), FUN = function(x) quantile(x, probs = 0.90))
            names(SIMDCalc90)<-c("SIMDrank5", "SIMD90")
            SIMDCalc<-merge(SIMDCalc, SIMDCalc90, by="SIMDrank5")
            
            
            SIMDCalc$SIMDcat[SIMDCalc[,5]>=(SIMDCalc$SIMDmean-0.2*SIMDCalc$SIMDmean) & SIMDCalc[,5]<=SIMDCalc$SIMDmean+(0.15*SIMDCalc$SIMDmean)]<-4
            SIMDCalc2<-subset(SIMDCalc, SIMDCalc[,5]<(SIMDCalc$SIMDmean-0.2*SIMDCalc$SIMDmean))
            breaks5<-unique(quantile(SIMDCalc2[,5], probs=0:3/3))
            SIMDCalc$SIMDcat2<-ifelse(SIMDCalc[,5]<(SIMDCalc$SIMDmean-0.2*SIMDCalc$SIMDmean), cut(SIMDCalc[,5], unique(breaks5), include.lowest=TRUE, labels=FALSE), NA)
            SIMDCalc3<-subset(SIMDCalc, SIMDCalc[,5]>SIMDCalc$SIMDmean+(0.15*SIMDCalc$SIMDmean))
            breaks6<-unique(quantile(SIMDCalc3[,5], probs=0:3/3))
            SIMDCalc$SIMDcat3<-ifelse(SIMDCalc[,5]>SIMDCalc$SIMDmean+(0.15*SIMDCalc$SIMDmean), cut(SIMDCalc[,5], unique(breaks6), include.lowest=TRUE, labels=FALSE), NA)
            SIMDCalc$SIMDcat3[SIMDCalc$SIMDcat3==1]<-5
            SIMDCalc$SIMDcat3[SIMDCalc$SIMDcat3==2]<-6
            SIMDCalc$SIMDcat3[SIMDCalc$SIMDcat3==3]<-7
            SIMDCalc$SIMDcat4<-paste(SIMDCalc$SIMDcat, SIMDCalc$SIMDcat2, SIMDCalc$SIMDcat3)
            SIMDCalc$SIMDcat4<-trimws(SIMDCalc$SIMDcat4)
            SIMDCalc$SIMDcat4<-sub("NA NA", "", SIMDCalc$SIMDcat4)
            SIMDCalc$SIMDcat4<-sub(" NA", "", SIMDCalc$SIMDcat4)
            SIMDCalc$SIMDcat4<-sub("NA ", "", SIMDCalc$SIMDcat4)
            SIMDCalc$SIMDcat4<-trimws(SIMDCalc$SIMDcat4)
            SIMDCalc$SIMDcat4<-as.numeric(SIMDCalc$SIMDcat4)
            SIMDCalcAdd<-subset(SIMDCalc, select=c("CODE", "SIMDcat4", "SIMDmean", "SIMD90", "SIMDrank5"))
            Datazone<-merge(Datazone, SIMDCalcAdd,by.x="code", by.y="CODE")
            
            
            
            ############################# have to make the categories for LA AVERAGE #############################
            LAmean<-mean(Datazone@data[,16])
            LA90th<-quantile(Datazone@data[,16], c(.90)) 
            # have to make the categories
            Datazone@data$LAcat[Datazone@data[,16]>=(LAmean-0.2*LAmean) & Datazone@data[,16]<=LAmean+(0.15*LAmean)]<-4
            Datazone2<-subset(Datazone, Datazone@data[,16]<(LAmean-0.2*LAmean))
            breaks3<-unique(quantile(Datazone2@data[,16], probs=0:3/3))
            if (length(breaks3)>1 ){
              Datazone@data$LAcat2<-ifelse(Datazone@data[,16]<(LAmean-0.2*LAmean), cut(Datazone@data[,16], unique(breaks3), include.lowest=TRUE, labels=FALSE), NA)
            } else {
              Datazone@data$LAcat2[Datazone@data[,16]<(LAmean-0.2*LAmean)]<-1
            }
            Datazone3<-subset(Datazone, Datazone@data[,16]>LAmean+(0.15*LAmean))
            breaks4<-unique(quantile(Datazone3@data[,16], probs=0:3/3))
            if (length(breaks4)>1){
              Datazone@data$LAcat3<-ifelse(Datazone@data[,16]>LAmean+(0.15*LAmean), cut(Datazone@data[,16], unique(breaks4), include.lowest=TRUE, labels=FALSE), NA)
            } else {   Datazone@data$LAcat3<-1
            }
            Datazone@data$LAcat3[Datazone@data$LAcat3==1]<-5
            Datazone@data$LAcat3[Datazone@data$LAcat3==2]<-6
            Datazone@data$LAcat3[Datazone@data$LAcat3==3]<-7
            Datazone@data$LAcat4<-paste(Datazone@data$LAcat, Datazone@data$LAcat2, Datazone@data$LAcat3)
            Datazone@data$LAcat4<-trimws(Datazone@data$LAcat4)
            Datazone@data$LAcat4<-sub("NA NA", "", Datazone@data$LAcat4)
            Datazone@data$LAcat4<-sub(" NA", "", Datazone@data$LAcat4)
            Datazone@data$LAcat4<-sub("NA ", "", Datazone@data$LAcat4)
            Datazone@data$LAcat4<-trimws(Datazone@data$LAcat4)
            Datazone@data$LAcat4<-as.numeric(Datazone@data$LAcat4)
            ###################################################################################################################    
            
            
            
            if(input$comparison=="SCO"){
              pal <- colorNumeric(c("#5d8bba", "#ffffe5", "#d73027"), 1:7)   
              
              ### superscript in leaflet
              
              popup <- paste0("<h3>", Datazone$name, "</h3><br>",
                              "Density around the population centre is ", "<strong>", round(Datazone@data[,16], 2)," per km2","</strong>",
                              "<br/>",
                              "This is <strong>", round((Datazone@data[,16]/Datazone$Scottishaverage*100),0), ifelse(round(Datazone@data[,16], 2)>round(Datazone$Scottishaverage, 2) ,"% higher</strong> than", "%</strong> of")," the Scottish average",
                              "<br/>",
                              ifelse(Datazone@data[,16]>Scottish90th, "<strong><font color='#EE2C2C'>This datazone is in the top 10% of neighbourhoods in Scotland</font></strong>", ""),
                              "<br/>",
                              "<br/>",
                              " More information available on this datazone ", "<b><a target='_blank' href='http://statistics.gov.scot/doc/statistical-geography/", Datazone$code,"'>here</a></b>")
              
              #################
              mapit  %>%
                
                #addPolygons(data=LAoutline,
                #            stroke=T,
                #            weight=3,
                #            color= "black",
                #            fillOpacity = 0) #%>%
                
                addPolygons(data=Datazone,
                            stroke=T,
                            weight=0.3,
                            smoothFactor = 0.2,
                            fillOpacity = 0.62,
                            popup=popup,
                            color= ~pal(SCOcat4),
                            highlightOptions = highlightOptions(color = "black", weight = 3,
                                                                bringToFront = TRUE)) %>%
                addMarkers(data=map2, ~long, ~lat)
            }
            else if(input$comparison=="LA"){
              pal <- colorNumeric(c("#5d8bba", "#ffffe5", "#d73027"), 1:7)   
              
              popup <- paste0("<h3>", Datazone$name, "</h3><br>",
                              "Density around the population centre is ", "<strong>", round(Datazone@data[,16], 2)," per km2","</strong>",
                              "<br/>",
                              "This is <strong>", round((Datazone@data[,16]/LAmean*100),0), ifelse(round(Datazone@data[,16], 2)>round(LAmean, 2) ,"% higher than</strong>", "%</strong> of")," the ", input$LAinput, " average",
                              "<br/>",
                              ifelse(Datazone@data[,16]>LA90th, paste0("<strong><font color='#EE2C2C'>This datazone is in the top 10% of neighbourhoods in ", input$LAinput, "</font></strong>"), ""),
                              "<br/>",
                              "<br/>",
                              " More information available on this datazone ", "<b><a target='_blank' href='http://statistics.gov.scot/doc/statistical-geography/", Datazone$code,"'>here</a></b>")
              
              mapit  %>%     
                addPolygons(data=Datazone,
                            stroke=TRUE,
                            weight=0.1,
                            smoothFactor = 0.2,
                            fillOpacity = 0.62,
                            popup=popup,
                            color= ~pal(LAcat4),
                            highlightOptions = highlightOptions(color = "black", weight = 3,
                                                                bringToFront = TRUE)) 
              
            }
            else if(input$comparison=="URBRUR"){
              pal <- colorNumeric(c("#5d8bba", "#ffffe5", "#d73027"), 1:7)   
              
              popup <- paste0("<h3>", Datazone$name, "</h3><br>",
                              "Density around the population centre is ", "<strong>", round(Datazone@data[,16], 2)," per km2","</strong>",
                              "<br/>",
                              "This is <strong>", round((Datazone@data[,16]/Datazone$UR6_2013_2014mean*100),0), ifelse(round(Datazone@data[,16], 2)>round(Datazone$UR6_2013_2014mean, 2) ,"% higher than</strong>", "%</strong> of")," the Urban Rural Classification (6-Fold) Group ", Datazone$UR6_2013_2014, " average",
                              "<br/>",
                              ifelse(Datazone@data[,16]>Datazone$UR6_2013_201490, paste0("<strong><font color='#EE2C2C'>This datazone is in the top 10% of neighbourhoods in the Urban Rural Classification (6-Fold) Group ", Datazone$UR6_2013_2014, "</font></strong>"), ""),
                              "<br/>",
                              "<br/>",
                              " More information available on this datazone ", "<b><a target='_blank' href='http://statistics.gov.scot/doc/statistical-geography/", Datazone$code,"'>here</a></b>")
              
              
              mapit %>% 
                #addPolygons(data=LAoutline,
                #            stroke=T,
                #            weight=3,
                #            color= "black",
                #            fillOpacity = 0) #%>%
                
                addPolygons(data=Datazone,
                            stroke=TRUE,
                            weight=0.1,
                            smoothFactor = 0.2,
                            fillOpacity = 0.62,
                            popup=popup,
                            color= ~pal(UrbRurcat4),
                            highlightOptions = highlightOptions(color = "black", weight = 3,
                                                                bringToFront = TRUE)) 
              
            }
            else if(input$comparison=="SIMD"){
              pal <- colorNumeric(c("#5d8bba", "#ffffe5", "#d73027"), 1:7)   
              
              ### superscript in leaflet
              popup <- paste0("<h3>", Datazone$name, "</h3><br>",
                              "Density around the population centre is ", "<strong>", round(Datazone@data[,16], 2)," per km2","</strong>",
                              "<br/>",
                              "This is <strong>", round((Datazone@data[,16]/Datazone$SIMDmean*100),0), ifelse(round(Datazone@data[,16], 2)>round(Datazone$SIMDmean, 2) ,"% higher than</strong>", "% of</strong>")," the Scottish Index of Multiple Deprivation Quintile Group ", Datazone$SIMDrank5, " average",
                              "<br/>",
                              ifelse(Datazone@data[,16]>Datazone$SIMD90, paste0("<strong><font color='#EE2C2C'>This datazone is in the top 10% of neighbourhoods in the Scottish Index of Multiple Deprivation Quintile Group ", Datazone$SIMDrank5, "</font></strong>"), ""),
                              "<br/>",
                              "<br/>",
                              " More information available on this datazone ", "<b><a target='_blank' href='http://statistics.gov.scot/doc/statistical-geography/", Datazone$code,"'>here</a></b>")
              
              #####################
              mapit  %>%
                #addPolygons(data=LAoutline,
                #            stroke=T,
                #            weight=3,
                #            color= "black",
                #            fillOpacity = 0) #%>%
                
                addPolygons(data=Datazone,
                            stroke=TRUE,
                            weight=0.1,
                            smoothFactor = 0.2,
                            fillOpacity = 0.62,
                            popup=popup,
                            color= ~pal(SIMDcat4),
                            highlightOptions = highlightOptions(color = "black", weight = 3,
                                                                bringToFront = TRUE)) 
              
            }
          }
        }
        )}
      
      
      else{
        mapit <- leafletProxy("map") %>% clearShapes() %>%clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7) 
      }
    })  
  })
  
  observe({
    if (!is.na(location2)){
      mapit  %>% clearShapes() 
      Datazone<-readRDS(paste0("geography/DZ/la/", trimws(location2), ".rds"))
      Datazone <- spTransform(Datazone, CRS("+proj=longlat +datum=WGS84"))
      # Datazone2 is the datazone location
      Datazone2 <- Datazone[location[1,2], ]
      buffer<-gBuffer(map, width = 3000)
      buffer<-spTransform(buffer, proj4string(Datazone))
      Datazone3 <-gIntersects(Datazone, buffer, byid = T)
      Datazone3<- as.data.frame(t(Datazone3))
      Datazone@data <-cbind(Datazone@data, Datazone3)
      Datazone<-Datazone[Datazone@data$buffer==T, ]
      
      
      ### get the data in
      data <- as.character(paste0(input$buffer, input$datatype, input$year, ".csv"))
      add<-read.csv(paste0("data/output/",data))
      Scotlandmean<-mean(add[,3])
      Datazone$Scottishaverage<-Scotlandmean
      Scottish90th<-quantile(add[,3], c(.90)) 
      add$CODE<-trimws(add$CODE)
      Datazone<-merge(Datazone, add, by.x="datazone", by.y="CODE")
      Datazone$stdareaha<-NULL
      Datazone$stdareakm2<-NULL
      Datazone$shape_leng<-NULL
      
      #### Mulitple options- generate variable that is scottish average
      ############################# have to make the categories for SCOTTISH AVERAGE #############################
      
      Datazone@data$SCOcat[Datazone@data[,16]>=(Scotlandmean-0.2*Scotlandmean) & Datazone@data[,16]<=Scotlandmean+(0.15*Scotlandmean)]<-4
      Datazone2<-subset(Datazone, Datazone@data[,16]<(Scotlandmean-0.2*Scotlandmean))
      breaks1<-unique(quantile(Datazone2@data[,16], probs=0:3/3))
      Datazone@data$SCOcat2<-ifelse(Datazone@data[,16]<(Scotlandmean-0.2*Scotlandmean), cut(Datazone@data[,16], unique(breaks1), include.lowest=TRUE, labels=FALSE), NA)
      Datazone3<-subset(Datazone, Datazone@data[,16]>Scotlandmean+(0.15*Scotlandmean))
      breaks2<-unique(quantile(Datazone3@data[,16], probs=0:3/3))
      Datazone@data$SCOcat3<-ifelse(Datazone@data[,16]>Scotlandmean+(0.15*Scotlandmean), cut(Datazone@data[,16], unique(breaks2), include.lowest=TRUE, labels=FALSE), NA)
      Datazone@data$SCOcat3[Datazone@data$SCOcat3==1]<-5
      Datazone@data$SCOcat3[Datazone@data$SCOcat3==2]<-6
      Datazone@data$SCOcat3[Datazone@data$SCOcat3==3]<-7
      Datazone@data$SCOcat4<-paste(Datazone@data$SCOcat, Datazone@data$SCOcat2, Datazone@data$SCOcat3)
      Datazone@data$SCOcat4<-trimws(Datazone@data$SCOcat4)
      Datazone@data$SCOcat4<-sub("NA NA", "", Datazone@data$SCOcat4)
      Datazone@data$SCOcat4<-sub(" NA", "", Datazone@data$SCOcat4)
      Datazone@data$SCOcat4<-sub("NA ", "", Datazone@data$SCOcat4)
      Datazone@data$SCOcat4<-trimws(Datazone@data$SCOcat4)
      Datazone@data$SCOcat4<-as.numeric(Datazone@data$SCOcat4)
      
      ## too slow load up from file- simplify-
      # Datazone@data$LAid<-99
      # LAoutline <- gUnaryUnion(Datazone, id = Datazone@data$LAid)
      
      ###################################################################################################################
      ################## Urban Rural
      add2<-read.csv(paste0("data/urbanrural.csv"))
      UrbRur<-merge(add, add2, by.x="CODE", by.y="Datazone2011")
      UrbRur$UR6_2013_2014<-as.numeric(as.character(UrbRur$UR6_2013_2014))
      UrbRurmean <-aggregate(UrbRur[,3], by=list(UrbRur$UR6_2013_2014), FUN=mean, na.rm=TRUE)
      names(UrbRurmean)<-c("UR6_2013_2014", "UR6_2013_2014mean")
      UrbRurCalc<-merge(UrbRurmean, UrbRur, by="UR6_2013_2014")
      UrbRurCalc90<-aggregate(UrbRur[,3], by = list(UrbRur$UR6_2013_2014), FUN = function(x) quantile(x, probs = 0.90))
      names(UrbRurCalc90)<-c("UR6_2013_2014", "UR6_2013_201490")
      UrbRurCalc<-merge(UrbRurCalc, UrbRurCalc90, by="UR6_2013_2014")
      
      
      ###############
      UrbRurCalc$UrbRurcat[UrbRurCalc[,5]>=(UrbRurCalc$UR6_2013_2014mean-0.2*UrbRurCalc$UR6_2013_2014mean) & UrbRurCalc[,5]<=UrbRurCalc$UR6_2013_2014mean+(0.15*UrbRurCalc$UR6_2013_2014mean)]<-4
      UrbRurCalc2<-subset(UrbRurCalc, UrbRurCalc[,5]<(UrbRurCalc$UR6_2013_2014mean-0.2*UrbRurCalc$UR6_2013_2014mean))
      breaks5<-unique(quantile(UrbRurCalc2[,5], probs=0:3/3))
      UrbRurCalc$UrbRurcat2<-ifelse(UrbRurCalc[,5]<(UrbRurCalc$UR6_2013_2014mean-0.2*UrbRurCalc$UR6_2013_2014mean), cut(UrbRurCalc[,5], unique(breaks5), include.lowest=TRUE, labels=FALSE), NA)
      UrbRurCalc3<-subset(UrbRurCalc, UrbRurCalc[,5]>UrbRurCalc$UR6_2013_2014mean+(0.15*UrbRurCalc$UR6_2013_2014mean))
      breaks6<-unique(quantile(UrbRurCalc3[,5], probs=0:3/3))
      UrbRurCalc$UrbRurcat3<-ifelse(UrbRurCalc[,5]>UrbRurCalc$UR6_2013_2014mean+(0.15*UrbRurCalc$UR6_2013_2014mean), cut(UrbRurCalc[,5], unique(breaks6), include.lowest=TRUE, labels=FALSE), NA)
      UrbRurCalc$UrbRurcat3[UrbRurCalc$UrbRurcat3==1]<-5
      UrbRurCalc$UrbRurcat3[UrbRurCalc$UrbRurcat3==2]<-6
      UrbRurCalc$UrbRurcat3[UrbRurCalc$UrbRurcat3==3]<-7
      UrbRurCalc$UrbRurcat4<-paste(UrbRurCalc$UrbRurcat, UrbRurCalc$UrbRurcat2, UrbRurCalc$UrbRurcat3)
      UrbRurCalc$UrbRurcat4<-trimws(UrbRurCalc$UrbRurcat4)
      UrbRurCalc$UrbRurcat4<-sub("NA NA", "", UrbRurCalc$UrbRurcat4)
      UrbRurCalc$UrbRurcat4<-sub(" NA", "", UrbRurCalc$UrbRurcat4)
      UrbRurCalc$UrbRurcat4<-sub("NA ", "", UrbRurCalc$UrbRurcat4)
      UrbRurCalc$UrbRurcat4<-trimws(UrbRurCalc$UrbRurcat4)
      UrbRurCalc$UrbRurcat4<-as.numeric(UrbRurCalc$UrbRurcat4)
      UrbRurCalcAdd<-subset(UrbRurCalc, select=c("CODE", "UrbRurcat4", "UR6_2013_2014mean", "UR6_2013_201490", "UR6_2013_2014"))
      Datazone<-merge(Datazone, UrbRurCalcAdd,by.x="code", by.y="CODE")
      
      
      ###################################################################################################################
      ######################   SIMD
      add4<-read.csv(paste0("data/SIMD.csv"))
      SIMD<-merge(add, add4, by.x="CODE", by.y="Data_Zone")
      SIMD$SIMDrank5<-as.numeric(quantcut(as.numeric(SIMD$Income_domain_2016_rank), 5))
      SIMDmean <-aggregate(SIMD[,3], by=list(SIMD$SIMDrank5), FUN=mean, na.rm=TRUE)
      names(SIMDmean)<-c("SIMDrank5", "SIMDmean")
      SIMDCalc<-merge(SIMDmean, SIMD, by="SIMDrank5")
      SIMDCalc90<-aggregate(SIMD[,3], by = list(SIMD$SIMDrank5), FUN = function(x) quantile(x, probs = 0.90))
      names(SIMDCalc90)<-c("SIMDrank5", "SIMD90")
      SIMDCalc<-merge(SIMDCalc, SIMDCalc90, by="SIMDrank5")
      
      
      SIMDCalc$SIMDcat[SIMDCalc[,5]>=(SIMDCalc$SIMDmean-0.2*SIMDCalc$SIMDmean) & SIMDCalc[,5]<=SIMDCalc$SIMDmean+(0.15*SIMDCalc$SIMDmean)]<-4
      SIMDCalc2<-subset(SIMDCalc, SIMDCalc[,5]<(SIMDCalc$SIMDmean-0.2*SIMDCalc$SIMDmean))
      breaks5<-unique(quantile(SIMDCalc2[,5], probs=0:3/3))
      SIMDCalc$SIMDcat2<-ifelse(SIMDCalc[,5]<(SIMDCalc$SIMDmean-0.2*SIMDCalc$SIMDmean), cut(SIMDCalc[,5], unique(breaks5), include.lowest=TRUE, labels=FALSE), NA)
      SIMDCalc3<-subset(SIMDCalc, SIMDCalc[,5]>SIMDCalc$SIMDmean+(0.15*SIMDCalc$SIMDmean))
      breaks6<-unique(quantile(SIMDCalc3[,5], probs=0:3/3))
      SIMDCalc$SIMDcat3<-ifelse(SIMDCalc[,5]>SIMDCalc$SIMDmean+(0.15*SIMDCalc$SIMDmean), cut(SIMDCalc[,5], unique(breaks6), include.lowest=TRUE, labels=FALSE), NA)
      SIMDCalc$SIMDcat3[SIMDCalc$SIMDcat3==1]<-5
      SIMDCalc$SIMDcat3[SIMDCalc$SIMDcat3==2]<-6
      SIMDCalc$SIMDcat3[SIMDCalc$SIMDcat3==3]<-7
      SIMDCalc$SIMDcat4<-paste(SIMDCalc$SIMDcat, SIMDCalc$SIMDcat2, SIMDCalc$SIMDcat3)
      SIMDCalc$SIMDcat4<-trimws(SIMDCalc$SIMDcat4)
      SIMDCalc$SIMDcat4<-sub("NA NA", "", SIMDCalc$SIMDcat4)
      SIMDCalc$SIMDcat4<-sub(" NA", "", SIMDCalc$SIMDcat4)
      SIMDCalc$SIMDcat4<-sub("NA ", "", SIMDCalc$SIMDcat4)
      SIMDCalc$SIMDcat4<-trimws(SIMDCalc$SIMDcat4)
      SIMDCalc$SIMDcat4<-as.numeric(SIMDCalc$SIMDcat4)
      SIMDCalcAdd<-subset(SIMDCalc, select=c("CODE", "SIMDcat4", "SIMDmean", "SIMD90", "SIMDrank5"))
      Datazone<-merge(Datazone, SIMDCalcAdd,by.x="code", by.y="CODE")
      
      
      
      ############################# have to make the categories for LA AVERAGE #############################
      LAmean<-mean(Datazone@data[,16])
      LA90th<-quantile(Datazone@data[,16], c(.90)) 
      # have to make the categories
      Datazone@data$LAcat[Datazone@data[,16]>=(LAmean-0.2*LAmean) & Datazone@data[,16]<=LAmean+(0.15*LAmean)]<-4
      Datazone2<-subset(Datazone, Datazone@data[,16]<(LAmean-0.2*LAmean))
      breaks3<-unique(quantile(Datazone2@data[,16], probs=0:3/3))
      if (length(breaks3)>1 ){
        Datazone@data$LAcat2<-ifelse(Datazone@data[,16]<(LAmean-0.2*LAmean), cut(Datazone@data[,16], unique(breaks3), include.lowest=TRUE, labels=FALSE), NA)
      } else {
        Datazone@data$LAcat2[Datazone@data[,16]<(LAmean-0.2*LAmean)]<-1
      }
      Datazone3<-subset(Datazone, Datazone@data[,16]>LAmean+(0.15*LAmean))
      breaks4<-unique(quantile(Datazone3@data[,16], probs=0:3/3))
      if (length(breaks4)>1){
        Datazone@data$LAcat3<-ifelse(Datazone@data[,16]>LAmean+(0.15*LAmean), cut(Datazone@data[,16], unique(breaks4), include.lowest=TRUE, labels=FALSE), NA)
      } else {   Datazone@data$LAcat3<-1
      }
      Datazone@data$LAcat3[Datazone@data$LAcat3==1]<-5
      Datazone@data$LAcat3[Datazone@data$LAcat3==2]<-6
      Datazone@data$LAcat3[Datazone@data$LAcat3==3]<-7
      Datazone@data$LAcat4<-paste(Datazone@data$LAcat, Datazone@data$LAcat2, Datazone@data$LAcat3)
      Datazone@data$LAcat4<-trimws(Datazone@data$LAcat4)
      Datazone@data$LAcat4<-sub("NA NA", "", Datazone@data$LAcat4)
      Datazone@data$LAcat4<-sub(" NA", "", Datazone@data$LAcat4)
      Datazone@data$LAcat4<-sub("NA ", "", Datazone@data$LAcat4)
      Datazone@data$LAcat4<-trimws(Datazone@data$LAcat4)
      Datazone@data$LAcat4<-as.numeric(Datazone@data$LAcat4)
      ###################################################################################################################    
      
      
      
      if(input$comparison=="SCO"){
        pal <- colorNumeric(c("#5d8bba", "#ffffe5", "#d73027"), 1:7)   
        
        ### superscript in leaflet
        
        popup <- paste0("<h3>", Datazone$name, "</h3><br>",
                        "Density around the population centre is ", "<strong>", round(Datazone@data[,16], 2)," per km2","</strong>",
                        "<br/>",
                        "This is <strong>", round((Datazone@data[,16]/Datazone$Scottishaverage*100),0), ifelse(round(Datazone@data[,16], 2)>round(Datazone$Scottishaverage, 2) ,"% higher</strong> than", "%</strong> of")," the Scottish average",
                        "<br/>",
                        ifelse(Datazone@data[,16]>Scottish90th, "<strong><font color='#EE2C2C'>This datazone is in the top 10% of neighbourhoods in Scotland</font></strong>", ""),
                        "<br/>",
                        "<br/>",
                        " More information available on this datazone ", "<b><a target='_blank' href='http://statistics.gov.scot/doc/statistical-geography/", Datazone$code,"'>here</a></b>")
        
        #################
        mapit  %>%
          
          #addPolygons(data=LAoutline,
          #            stroke=T,
          #            weight=3,
          #            color= "black",
          #            fillOpacity = 0) #%>%
          
          addPolygons(data=Datazone,
                      stroke=T,
                      weight=0.3,
                      smoothFactor = 0.2,
                      fillOpacity = 0.62,
                      popup=popup,
                      color= ~pal(SCOcat4),
                      highlightOptions = highlightOptions(color = "black", weight = 3,
                                                          bringToFront = TRUE)) %>%
          addMarkers(data=map2, ~long, ~lat)
      }
      else if(input$comparison=="LA"){
        pal <- colorNumeric(c("#5d8bba", "#ffffe5", "#d73027"), 1:7)   
        
        popup <- paste0("<h3>", Datazone$name, "</h3><br>",
                        "Density around the population centre is ", "<strong>", round(Datazone@data[,16], 2)," per km2","</strong>",
                        "<br/>",
                        "This is <strong>", round((Datazone@data[,16]/LAmean*100),0), ifelse(round(Datazone@data[,16], 2)>round(LAmean, 2) ,"% higher than</strong>", "%</strong> of")," the ", input$LAinput, " average",
                        "<br/>",
                        ifelse(Datazone@data[,16]>LA90th, paste0("<strong><font color='#EE2C2C'>This datazone is in the top 10% of neighbourhoods in ", input$LAinput, "</font></strong>"), ""),
                        "<br/>",
                        "<br/>",
                        " More information available on this datazone ", "<b><a target='_blank' href='http://statistics.gov.scot/doc/statistical-geography/", Datazone$code,"'>here</a></b>")
        
        mapit  %>%     
          addPolygons(data=Datazone,
                      stroke=TRUE,
                      weight=0.1,
                      smoothFactor = 0.2,
                      fillOpacity = 0.62,
                      popup=popup,
                      color= ~pal(LAcat4),
                      highlightOptions = highlightOptions(color = "black", weight = 3,
                                                          bringToFront = TRUE)) 
        
      }
      else if(input$comparison=="URBRUR"){
        pal <- colorNumeric(c("#5d8bba", "#ffffe5", "#d73027"), 1:7)   
        
        popup <- paste0("<h3>", Datazone$name, "</h3><br>",
                        "Density around the population centre is ", "<strong>", round(Datazone@data[,16], 2)," per km2","</strong>",
                        "<br/>",
                        "This is <strong>", round((Datazone@data[,16]/Datazone$UR6_2013_2014mean*100),0), ifelse(round(Datazone@data[,16], 2)>round(Datazone$UR6_2013_2014mean, 2) ,"% higher than</strong>", "%</strong> of")," the Urban Rural Classification (6-Fold) Group ", Datazone$UR6_2013_2014, " average",
                        "<br/>",
                        ifelse(Datazone@data[,16]>Datazone$UR6_2013_201490, paste0("<strong><font color='#EE2C2C'>This datazone is in the top 10% of neighbourhoods in the Urban Rural Classification (6-Fold) Group ", Datazone$UR6_2013_2014, "</font></strong>"), ""),
                        "<br/>",
                        "<br/>",
                        " More information available on this datazone ", "<b><a target='_blank' href='http://statistics.gov.scot/doc/statistical-geography/", Datazone$code,"'>here</a></b>")
        
        
        mapit %>% 
          #addPolygons(data=LAoutline,
          #            stroke=T,
          #            weight=3,
          #            color= "black",
          #            fillOpacity = 0) #%>%
          
          addPolygons(data=Datazone,
                      stroke=TRUE,
                      weight=0.1,
                      smoothFactor = 0.2,
                      fillOpacity = 0.62,
                      popup=popup,
                      color= ~pal(UrbRurcat4),
                      highlightOptions = highlightOptions(color = "black", weight = 3,
                                                          bringToFront = TRUE)) 
        
      }
      else if(input$comparison=="SIMD"){
        pal <- colorNumeric(c("#5d8bba", "#ffffe5", "#d73027"), 1:7)   
        
        ### superscript in leaflet
        popup <- paste0("<h3>", Datazone$name, "</h3><br>",
                        "Density around the population centre is ", "<strong>", round(Datazone@data[,16], 2)," per km2","</strong>",
                        "<br/>",
                        "This is <strong>", round((Datazone@data[,16]/Datazone$SIMDmean*100),0), ifelse(round(Datazone@data[,16], 2)>round(Datazone$SIMDmean, 2) ,"% higher than</strong>", "% of</strong>")," the Scottish Index of Multiple Deprivation Quintile Group ", Datazone$SIMDrank5, " average",
                        "<br/>",
                        ifelse(Datazone@data[,16]>Datazone$SIMD90, paste0("<strong><font color='#EE2C2C'>This datazone is in the top 10% of neighbourhoods in the Scottish Index of Multiple Deprivation Quintile Group ", Datazone$SIMDrank5, "</font></strong>"), ""),
                        "<br/>",
                        "<br/>",
                        " More information available on this datazone ", "<b><a target='_blank' href='http://statistics.gov.scot/doc/statistical-geography/", Datazone$code,"'>here</a></b>")
        
        #####################
        mapit  %>%
          #addPolygons(data=LAoutline,
          #            stroke=T,
          #            weight=3,
          #            color= "black",
          #            fillOpacity = 0) #%>%
          
          addPolygons(data=Datazone,
                      stroke=TRUE,
                      weight=0.1,
                      smoothFactor = 0.2,
                      fillOpacity = 0.62,
                      popup=popup,
                      color= ~pal(SIMDcat4),
                      highlightOptions = highlightOptions(color = "black", weight = 3,
                                                          bringToFront = TRUE)) 
        
      }
      
    }
    else{
      mapit <- leafletProxy("map") %>% clearShapes() %>%clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7) 
    }
  })
  
  
  ##################################### DOWNLOAD FACILITY
  Dataset <- reactive({
    datatypeOUT<-input$datatype
    strOUT<-input$str
    bufferOUT<-input$buffer
    yearOUT<-input$year
    if (!is.null(input$LAinput)){
      Datazone<-readRDS(paste0("geography/DZ/la/",input$LAinput ,".rds"))
    } else (Datazone<-readRDS(paste0("geography/DZ/la/Scotland.rds")))
    Datazone <- spTransform(Datazone, CRS("+proj=longlat +datum=WGS84"))
    data <- as.character(paste0(bufferOUT, datatypeOUT, yearOUT, ".csv"))
    add<-read.csv(paste0("data/output/",data))
    add$CODE<-trimws(add$CODE)
    Datazone<-merge(Datazone, add, by.x="datazone", by.y="CODE")
    v<-Datazone@data
    return(v)
  })
  
  output$dt <- 
    DT::renderDataTable(
      datatable(Dataset()),
      server = FALSE
    )
  
  #output$filtered_row <- 
  # renderPrint({
  #    input[["dt_rows_all"]]
  #  })
  
  
  output$download_filtered <- 
    downloadHandler(
      filename = "CRESH_ALCTOBDATA.csv",
      content = function(file){
        write.csv(Dataset()[input[["dt_rows_all"]], ],
                  file)
      }
    )
  
  
  
  #############################################
  output$download=renderUI({ 
    list(
      HTML("<br>"),
      helpText("To find out how the data are being used, and to report back to the project funders (ESRC), we are collecting contact information for anyone wanting to download the data. We will not release these details to anyone and only the project researchers will see this information. We may contact you to ask how we can improve the website or to find out if you found the data helpful."),
      textInput("email", "Your Email", ""),
      textInput("name", "Full Name"),
      selectInput("reason", "Reason for Download",
                  list("Academic" = "Academic",
                       "Health Sector" = "Health Sector",
                       "Member of the public"  = "Member of the public",
                       "Community council"  = "Community council",
                       "Retail trade" = "Retail trade",
                       "Journalist/blogger" = "Journalist/blogger",
                       "Industry" = "Industry",
                       "Government/policy related" = "Government/policy related",
                       "Other" = "Other")),
      textInput("other", "If other, please provide detail", ""),
      textInput("use", "Data will be used for", ""),
      actionButton("submit", "Submit", class = "btn-primary")
    ) })
  
  ### Not doing anything at the moment!  
  # add an asterisk to an input label
  labelMandatory <- function(label) {
    tagList(
      label,
      span("*", class = "mandatory_star")
    )
  }
  ######
  
  ######## Remember time stamp is NOT corrected for BST
  
  # get current Epoch time
  epochTime <- function() {
    return(as.integer(Sys.time()))
  }
  
  # get a formatted string of the timestamp (exclude colons as they are invalid
  # characters in Windows filenames)
  humanTime <- function() {
    format(Sys.time(), "%Y%m%d-%H%M%OS")
  }
  
  # which fields are mandatory
  fieldsMandatory <- c("email", "name", "reason", "use")
  
  fieldsAll <- c("email", "name", "reason", "other","use")
  epochTime <- function() {
    as.integer(Sys.time())
  }
  #
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data
  }) 
  
  
  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    shinyjs::toggleState(id = "download_filtered", condition = mandatoryFilled)
  })
  
  
  
  # save the results to a file
  options(mongodb = list(
    "host" = "ds157187.mlab.com:57187",
    "username" = "marko",
    "password" = "Shiny4life"
  ))
  
  databaseName <- "creshmap"
  
  saveData <- function(data, collectionName) {
    # Connect to the database
    db <- mongo(collection = collectionName,
                url = sprintf(
                  "mongodb://%s:%s@%s/%s",
                  options()$mongodb$username,
                  options()$mongodb$password,
                  options()$mongodb$host,
                  databaseName))
    # Insert the data into the mongo collection as a data.frame
    data <- as.data.frame(t(data))
    db$insert(data)
  }
  
  ###### WHAT ARE THE INPUTS
  AllInputs <- reactive({
    datatypeOUT<-input$datatype
    if (is.null(input$LAinput)){
      LAinputOUT<-""
    } else {
      LAinputOUT<-input$LAinput
    }
    strOUT<-input$str
    bufferOUT<-input$buffer
    yearOUT<-input$year
    comparisonOUT<-input$comparison
    epochTime <- function() {
      as.integer(Sys.time())
    }
    timestamp<-epochTime()
    masterOUT<-cbind(datatypeOUT, LAinputOUT, strOUT, bufferOUT, yearOUT, comparisonOUT, timestamp)
    data <- as.data.frame(t(masterOUT))
  })
  
  
  # When the Submit button is clicked, save the form data and the user inputs
  observeEvent(input$submit, {
    saveData(formData(), "responses")
    ## We need to start the download  
    saveData(AllInputs(), "inputs")
    
  })
  ################## who are the people using this!
  
  ## create zipped file
  
  
  #output$downloadData <- downloadHandler(
  #  filename = function() {
  #    paste(input$select,input$Date, input$Type,".zip",sep='') 
  #  },
  #  content = function(file) {
  #   file.copy(paste(input$select,input$Date, input$Type,".zip",sep=''), file)
  # },
  # contentType = "application/zip"
  #)
  
  
  
  
})



