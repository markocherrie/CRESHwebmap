## load packages
library(shiny)
library(leaflet)
require(RCurl)
require(RJSONIO)
require(plyr)
library(rgdal)
library(rgeos)
library(dplyr)
library(gtools)
library(jsonlite)
library(mongolite)
library(rgdal)
library(DT)
library(leaflet.extras)

## load local authority shapefile
geog<-readRDS("geography/councilareas.rds")

# load functions
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

# server file
shinyServer(function(input, output) {
  
  observe({
    if(input$Change=="LorG"){
      
      #### stick in geog2 below, so that people can see the local authority boundaries always??
      
      # Create the map
      output$map <- renderLeaflet({
        leaflet() %>%
          addProviderTiles("CartoDB.Positron", group = "CartoDB Positron") %>%
          addProviderTiles("Stamen.Toner", group = "Stamen Toner") %>%
          addProviderTiles("OpenStreetMap.BlackAndWhite", group = "OSM") %>%
          addLayersControl(
            baseGroups = c("CartoDB Positron", "OSM", "Toner"),
            options = layersControlOptions(collapsed = TRUE)) %>%
          addLegend("bottomright", 
                    colors=c("#a40025", "" ,"#006837"),
                    title = "Change",
                    labels= c("Loss","No change", "Gain"),
                    opacity =  0.7
          ) %>%
          addScaleBar(position = c("bottomleft"))%>%
          addFullscreenControl() 
      })} else if(input$Change=="HorL" & input$Feature=="Buildings"){
      
      output$map <- renderLeaflet({
        leaflet() %>%
          addProviderTiles("CartoDB.Positron", group = "CartoDB Positron") %>%
          addProviderTiles("Stamen.Toner", group = "Stamen Toner") %>%
          addProviderTiles("OpenStreetMap.BlackAndWhite", group = "OSM") %>%
          addLayersControl(
            baseGroups = c("CartoDB Positron", "OSM", "Toner"),
            options = layersControlOptions(collapsed = TRUE)) %>%
          addLegend("bottomright", 
                    colors=c("#a40025", "#d62f27" ,"#f46c43", "#fcad60", "", "#a6d96a" ,"#66bc62", "#1a9750", "#006837"),
                    title = "Level of Change (number)",
                    labels= c("Loss: >20", "Loss: 10-19", "Loss: 5-9", "Loss: <5","No change",  "Gain: <5", "Gain: 5-9", "Gain: 10-19", "Gain: >20"),
                    opacity =  0.7
          ) %>%
          addScaleBar(position = c("bottomleft"))%>%
          addFullscreenControl() 
      }) } else if(input$Change=="HorL" & input$Feature=="Roads"){
      
      output$map <- renderLeaflet({
        leaflet() %>%
          addProviderTiles("CartoDB.Positron", group = "CartoDB Positron") %>%
          addProviderTiles("Stamen.Toner", group = "Stamen Toner") %>%
          addProviderTiles("OpenStreetMap.BlackAndWhite", group = "OSM") %>%
          addLayersControl(
            baseGroups = c("CartoDB Positron", "OSM", "Toner"),
            options = layersControlOptions(collapsed = TRUE)) %>%
          addLegend("bottomright", 
                    colors=c("#a40025", "#d62f27" ,"#f46c43", "#fcad60", "", "#a6d96a" ,"#66bc62", "#1a9750", "#006837"),
                    title = "Level of Change (m)",
                    labels= c("Loss: 1,000+", "Loss: 500-999", "Loss: 100-499", "Loss: <100","No change",  "Gain: <100", "Gain: 100-499", "Gain: 499-999", "Gain: >1,000"),
                    opacity =  0.7
          ) %>%
          addScaleBar(position = c("bottomleft"))%>%
          addFullscreenControl()
      })} else if(input$Change=="HorL" & input$Feature=="Woodland"){
        
        output$map <- renderLeaflet({
          leaflet() %>%
            addProviderTiles("CartoDB.Positron", group = "CartoDB Positron") %>%
            addProviderTiles("Stamen.Toner", group = "Stamen Toner") %>%
            addProviderTiles("OpenStreetMap.BlackAndWhite", group = "OSM") %>%
            addLayersControl(
              baseGroups = c("CartoDB Positron", "OSM", "Toner"),
              options = layersControlOptions(collapsed = TRUE)) %>%
            addLegend("bottomright", 
                      colors=c("#a40025", "#d62f27" ,"#f46c43", "#fcad60", "", "#a6d96a" ,"#66bc62", "#1a9750", "#006837"),
                      title = "Level of Change (m)",
                      labels= c("Loss: 100,000+", "Loss: 1,000-99,999", "Loss: 100-999", "Loss: <100","No change",  "Gain: <100", "Gain: 100-999", "Gain: 1,000-99,999", "Gain: 100,000+"),
                      opacity =  0.7
            ) %>%
            addScaleBar(position = c("bottomleft"))%>%
            addFullscreenControl() 
        })
      }
  
  
  
  ##### LA input
  
  observe({
    
    if(!is.null(input$LAinput) & input$Change=="LorG"){
      mapit <- leafletProxy("map") 
      mapit  %>% clearShapes() %>% clearMarkers() #%>%
      foo<-length(input$LAinput)
      if (foo==1 && input$LAinput!="Scotland"){
        datalist = list()
        Datazone<-readRDS(paste0("geography/DZ/la/", input$LAinput[1], ".rds"))
        zoom<-read.csv("data/zoomlevels.csv")
        Datazone<-merge(Datazone, zoom, by="Council")
        zoomlevel<-unique(Datazone$zoomy)
        Datazone <- spTransform(Datazone, CRS("+proj=longlat +datum=WGS84"))
        long<- (Datazone@bbox[1,1]+Datazone@bbox[1,2])/2
        lat <-(Datazone@bbox[2,1]+Datazone@bbox[2,2])/2
        dat<-cbind(lat, long)  
        datalist[[1]] <- dat
        datalist<-data.frame(datalist)
        mapit %>% 
          setView(lng =  mean(as.numeric(as.character((datalist[,2])))), lat = mean(as.numeric(as.character((datalist[,1])))), zoom = zoomlevel)
        
      } else {
        mapit  %>%  clearMarkers()
      }
      for (i in input$LAinput){
        Datazone<-readRDS(paste0("geog/la/", i, "_",tolower(input$Feature),".rds"))
        Datazone <- spTransform(Datazone, CRS("+proj=longlat +datum=WGS84"))
        Datazone@data$Grps2<-as.numeric(paste0(Datazone@data$LossGain, Datazone@data$Grps))
        Datazone<-Datazone[Datazone@data$LossGain!=0,]
        
      
        ###################################################################################################################    
        
        #### POP UP BOX OUTPUT
        
        pal <- colorNumeric(c("#a40025","#006837"), c(-1,1))   
        
        ### superscript in leaflet
        popup <- 
          ifelse(Datazone@data$LGLabel=="No change", "There has been no change in this grid cell", paste0("There has been a ", Datazone@data$LGLabel, " in this cell"))
        
        
        #################
        mapit  %>%
          
          addPolygons(data=Datazone,
                      stroke=T,
                      weight=0.3,
                      smoothFactor = 0.2,
                      fillOpacity = 0.7,
                      popup=popup,
                      color= ~pal(LossGain),
                      highlightOptions = highlightOptions(color = "black", weight = 3,
                                                          bringToFront = TRUE)) 
        
      }
      
      
      
      
    } else if(!is.null(input$LAinput) & input$Change=="HorL"){
      
      mapit <- leafletProxy("map") 
      mapit  %>% clearShapes() %>% clearMarkers() #%>%
      foo<-length(input$LAinput)
      if (foo==1 && input$LAinput!="Scotland"){
        datalist = list()
        Datazone<-readRDS(paste0("geography/DZ/la/", input$LAinput[1], ".rds"))
        zoom<-read.csv("data/zoomlevels.csv")
        Datazone<-merge(Datazone, zoom, by="Council")
        zoomlevel<-unique(Datazone$zoomy)
        Datazone <- spTransform(Datazone, CRS("+proj=longlat +datum=WGS84"))
        long<- (Datazone@bbox[1,1]+Datazone@bbox[1,2])/2
        lat <-(Datazone@bbox[2,1]+Datazone@bbox[2,2])/2
        dat<-cbind(lat, long)  
        datalist[[1]] <- dat
        datalist<-data.frame(datalist)
        mapit %>% 
          setView(lng =  mean(as.numeric(as.character((datalist[,2])))), lat = mean(as.numeric(as.character((datalist[,1])))), zoom = zoomlevel)
        
      } else {
        mapit  %>%  clearMarkers()
      }
      for (i in input$LAinput){
        Datazone<-readRDS(paste0("geog/la/", i, "_",tolower(input$Feature),".rds"))
        Datazone <- spTransform(Datazone, CRS("+proj=longlat +datum=WGS84"))
        Datazone@data$Grps2<-as.numeric(paste0(Datazone@data$LossGain, Datazone@data$Grps))
        Datazone<-Datazone[Datazone@data$LossGain!=0,]
        
        #Datazone@data$Grps2[Datazone@data$Grps2==0]<-5
        Datazone@data$Grps2[Datazone@data$Grps2==-14]<-1
        Datazone@data$Grps2[Datazone@data$Grps2==-13]<-2
        Datazone@data$Grps2[Datazone@data$Grps2==-12]<-3
        Datazone@data$Grps2[Datazone@data$Grps2==-11]<-4
        Datazone@data$Grps2[Datazone@data$Grps2==11]<-5
        Datazone@data$Grps2[Datazone@data$Grps2==12]<-6
        Datazone@data$Grps2[Datazone@data$Grps2==13]<-7
        Datazone@data$Grps2[Datazone@data$Grps2==14]<-8
        
        ###################################################################################################################    
        
        #### POP UP BOX OUTPUT
        
        pal <- colorNumeric(c("#a40025", "#d62f27" ,"#f46c43", "#fcad60", "#a6d96a" ,"#66bc62", "#1a9750", "#006837"), c(1:8))   
        
        ### superscript in leaflet
        popup <- 
          ifelse(Datazone@data$LGLabel=="No change", "There has been no change in this datazone", paste0("There has been a ", Datazone@data$LGLabel, " of", gsub("Loss/gain: ", " ", Datazone@data$GrpsLabel), " in this cell"))
        
        
        #################
        mapit  %>%
          
          addPolygons(data=Datazone,
                      stroke=T,
                      weight=0.3,
                      smoothFactor = 0.2,
                      fillOpacity = 0.7,
                      popup=popup,
                      color= ~pal(Grps2),
                      highlightOptions = highlightOptions(color = "black", weight = 3,
                                                          bringToFront = TRUE)) 
        
      }
      
    } else{
      mapit <- leafletProxy("map") %>% clearShapes() %>% clearMarkers() %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7) 
    }
  })
  
  
 ###################################################################################################################    

# This gets geocodes the search string on the click of the button
observeEvent(input$goButton,{
      mapit <- leafletProxy("map") 
      mapit  %>% clearShapes() %>% clearMarkers()
      str <- as.character(paste0(input$str, ", Scotland"))
      map<-BING(str)

# If the search is not null then set the lat and long coordinates
if (!is.null(str)){
    lat<-map[1]
    long<-map[2]
    
# This is important... it sets the zoom to the geocoded location
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
## Here we are getting the local authority location
        location<-over(map, geog , fn = NULL)
        location2<-location[1,2]

###################################################################################################################    
    
observe({    
if(!is.na(location2) & input$Change=="LorG"){
  mapit  %>% clearShapes() %>% clearMarkers()
  Datazone<-readRDS(paste0("geog/la/", location2, "_",tolower(input$Feature),".rds"))
  Datazone <- spTransform(Datazone, CRS("+proj=longlat +datum=WGS84"))
  Datazone@data$Grps2<-as.numeric(paste0(Datazone@data$LossGain, Datazone@data$Grps))
  Datazone<-Datazone[Datazone@data$LossGain!=0,]
  
        
        #### POP UP BOX OUTPUT
        
        pal <- colorNumeric(c("#a40025","#006837"), c(-1,1))   
        
        ### superscript in leaflet
        popup <- 
          ifelse(Datazone@data$LGLabel=="No change", "There has been no change in this grid cell", paste0("There has been a ", Datazone@data$LGLabel, " in this cell"))
        
        
        #################
        mapit  %>%
          
          addPolygons(data=Datazone,
                      stroke=T,
                      weight=0.3,
                      smoothFactor = 0.2,
                      fillOpacity = 0.7,
                      popup=popup,
                      color= ~pal(LossGain),
                      highlightOptions = highlightOptions(color = "black", weight = 3,
                                                          bringToFront = TRUE)) 
        
}else if(!is.na(location2) & input$Change=="HorL"){
        # this means that it clears when you change the option 
        mapit  %>% clearShapes() %>% clearMarkers()
        Datazone<-readRDS(paste0("geog/la/", location2, "_",tolower(input$Feature),".rds"))
        Datazone <- spTransform(Datazone, CRS("+proj=longlat +datum=WGS84"))
        Datazone@data$Grps2<-as.numeric(paste0(Datazone@data$LossGain, Datazone@data$Grps))
        Datazone<-Datazone[Datazone@data$LossGain!=0,]
        
      #Datazone@data$Grps2[Datazone@data$Grps2==0]<-5
      Datazone@data$Grps2[Datazone@data$Grps2==-14]<-1
      Datazone@data$Grps2[Datazone@data$Grps2==-13]<-2
      Datazone@data$Grps2[Datazone@data$Grps2==-12]<-3
      Datazone@data$Grps2[Datazone@data$Grps2==-11]<-4
      Datazone@data$Grps2[Datazone@data$Grps2==11]<-5
      Datazone@data$Grps2[Datazone@data$Grps2==12]<-6
      Datazone@data$Grps2[Datazone@data$Grps2==13]<-7
      Datazone@data$Grps2[Datazone@data$Grps2==14]<-8
      
      ###################################################################################################################    
      
      #### POP UP BOX OUTPUT
      
      pal <- colorNumeric(c("#a40025", "#d62f27" ,"#f46c43", "#fcad60", "#a6d96a" ,"#66bc62", "#1a9750", "#006837"), c(1:8))   
      
      ### superscript in leaflet
      popup <- 
        ifelse(Datazone@data$LGLabel=="No change", "There has been no change in this datazone", paste0("There has been a ", Datazone@data$LGLabel, " of", gsub("Loss/gain: ", " ", Datazone@data$GrpsLabel), " in this cell"))
      
      
      #################
      mapit  %>%
        
        addPolygons(data=Datazone,
                    stroke=T,
                    weight=0.3,
                    smoothFactor = 0.2,
                    fillOpacity = 0.7,
                    popup=popup,
                    color= ~pal(Grps2),
                    highlightOptions = highlightOptions(color = "black", weight = 3,
                                                        bringToFront = TRUE)) 
}
}
)

# If there is no proper geocode then clear the map 
}else{
  mapit <- leafletProxy("map") %>% clearShapes() %>%clearMarkers()  
}

#This ones for the observe 2 - geocoded location    
})  
 
  ###################################################################################################################    
  
# this ones for the Shinyserver  
})

# this ones for the map view options
})
