library(shiny)
library(shinyjs)
library(leaflet)
library(rgdal)
library(shinyBS)
library(gtools)
library(ggplot2)
###updates--- to get datazones that have been clicked
### http://stackoverflow.com/questions/28938642/marker-mouse-click-event-in-r-leaflet-for-shiny


list<-read.csv("data/list.csv", header=F)
list<-list[1:nrow(list),]

##
textInputRow<-function (inputId, label, value = "") 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value,class="input-small"))
}


shinyUI(fluidPage(
  useShinyjs(),  # Include shinyjs
  
# geolocation
tags$style(type = "text/css", "#map {height: calc(100vh - 160px) !important;}"),

tags$head(
  tags$style(HTML("
                  @import url('//fonts.googleapis.com/css?family=Roboto+Slab');
                  "))
  ),

headerPanel(
  fluidRow(
  column(11, h1("Built Environment Change Across Scotland 2016 to 2017", 
     style = "font-family: 'Roboto Slab', cursive;
     font-weight: bold; font-size: 39px")))
, windowTitle = "Built Environment Change Across Scotland 2016 to 2017"),


  ### CRESH favicon
  tags$head(tags$link(rel = "shortcut icon", href="http://www.iconj.com/ico/g/g/ggtzbwew2b.ico", type="image/x-icon")),
  tags$head(tags$style("#summary{
                      position: relative;
                      display: inline-block;
                       width: 20%;
                       height: 10%;
                       top: 10px;
                       padding: 10% 0;
                       border-radius:50%;
                       line-height:0;
                       /* further display options */
                       @shadow: rgba(0, 0, 0, .1);
                       @shadow-length: 4px;
                       -webkit-box-shadow: 0 @shadow-length 0 0 @shadow;
                       box-shadow: 0 @shadow-length 0 0 @shadow;
                       text-shadow: 0 @shadow-length 0 @shadow;
                       background: #428bca;
                       color: white;
                       font-family: Helvetica, Arial Black, sans;
                       font-size: 24px;
                       text-align: center;
                       }"
                       
  )),
  sidebarPanel( 
    strong("Description"),
    helpText("This application allows you to view change in features in the built environment across small areas in Scotland, between two time points."),
    # adding the new div tag to the sidebsar            
    bsTooltip("LAinput", "Tip: Type in the local authority you wish to view", "top"
    ),
    
    selectizeInput('LAinput', 'Local Authorities', choices = list, multiple = TRUE, options = list(maxItems = 5)),
    #div(style="display:inline-block", textInput("str", label =("Enter an Area of Interest"), value = "")),
    #bsTooltip("str", "Tip: Type in an address, postcode or point of interest and click Enter. The location will be shown as a blue marker with data for the surrounding area local area presented on the map.", "top"
    #),
    #div(style="display:inline-block",actionButton("goButton", "Enter")),
    #tags$br(),
    #tags$br(),
    uiOutput("Feature"),
    bsTooltip("Feature", "Tip: Choose the feature you wish to view change for", "top"
    ),
    selectInput("Feature", "Feature:",
                c("Buildings" = "Buildings",
                  "Roads" = "Roads",
                  "Woodland" = "Woodland"), selected="Buildings"),
    bsTooltip("Change", "Tip: Choose how you wish change to be represented on the map", "top"
    ),
    selectInput("Change", "Change",
                list("Loss or Gain" = "LorG",
                     "High or low level change" = "HorL"),
                selected="LorG")),
  tags$br(),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Map", leafletOutput("map")),
                tabPanel("About", includeHTML("about.html")),
                tabPanel("How to use", includeHTML("howtouse.html")),
                #tabPanel("Change over time", leafletOutput("change")),
                tabPanel("Acknowledgements", includeHTML("acknowledgements.html"))
    ))
))


