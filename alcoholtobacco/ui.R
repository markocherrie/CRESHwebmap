library(shiny)
library(shinyjs)
library(leaflet)
library(rgdal)
library(shinyBS)
library(gtools)
library(ggplot2)
###updates--- to get datazones that have been clicked
### http://stackoverflow.com/questions/28938642/marker-mouse-click-event-in-r-leaflet-for-shiny


#list<-readRDS("data/list.rds")
#list<-unique(list[,1])
list<-read.csv("data/list.csv", header=F)

## Somehow take out V1 in the selection box!!!

# log scale
JScode <-
  "$(function() {
    setTimeout(function(){
      var vals = [0];
      var powStart = 1;
      var powStop = 7;
      for (i = powStart; i <= powStop; i++) {
        var val = Math.pow(10, i);
        val = parseFloat(val.toFixed(8));
        vals.push(val);
      }
      $('#range').data('ionRangeSlider').update({'values':vals})
    }, 5)})"


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
  tags$script('
              $(document).ready(function () {
              navigator.geolocation.getCurrentPosition(onSuccess, onError);
              
              function onError (err) {
              Shiny.onInputChange("geolocation", false);
              }
              
              function onSuccess (position) {
              setTimeout(function () {
              var coords = position.coords;
              console.log(coords.latitude + ", " + coords.longitude);
              Shiny.onInputChange("geolocation", true);
              Shiny.onInputChange("lat", coords.latitude);
              Shiny.onInputChange("long", coords.longitude);
              }, 1100)
              }
              });
              '),
  
  tags$style(type = "text/css", "#map {height: calc(100vh - 110px) !important;}"),

  headerPanel("Alcohol and Tobacco Environments in Scotland"),
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
    helpText("This application allows you to map alcohol and tobacco outlet density and related harm for small neighbourhoods across Scotland.
            Use the options below to choose the geographical area and data you are interested in.
 Once you have defined data for a single area, use the tabbed menu to get: an impression on how provision has changed over time (2012-2016), a text and visual summary and instructions on how to download the underlying data."),
    tags$head(tags$script(HTML(JScode))),
    bsTooltip("datatype", "There are different types of alcohol retailers. On-sales are when alcohol is consumed on the premises, off-sales are when it is bought at the retailer but consumed elsewhere. Some retailers offer both on and off sales.", "top"
    ),
    selectInput("datatype", "Data Type:",
                c("Alcohol On Sales" = "alcoholOn",
                  "Alcohol Off Sales" = "alcoholOff",
                  "Alcohol Both On and Off Sales" = "alcoholBoth",
                  "Alcohol Total Sales" = "alcoholTOTAL",
                  "Tobacco Total Sales" = "tobaccoTOTAL"), selected="alcoholTOTAL"),
    # adding the new div tag to the sidebsar            
    bsTooltip("LAinput", "We have used 2011 Scottish datazones as our smallest neighbourhood units. As there nearly 7,000, choose to display a subset by selecting up to 5 local authorities. If you do wish to view the whole of Scotland, select the Scotland option, however please be patient, this will result in much longer loading times", "top"
    ),
    selectizeInput('LAinput', 'Local Authorities', choices = list, multiple = TRUE, options = list(maxItems = 5)),
    div(style="display:inline-block", textInput("str", label =("Enter an Area of interest"), value = "")),
    bsTooltip("str", "If you would prefer to view a specific area, please type in an address, postcode or point of interest and click Enter. The location will be shown as a blue marker with data for the surrounding area local area presented on the map.", "top"
    ),
    div(style="display:inline-block",actionButton("goButton", "Enter")),
    tags$br(),
    uiOutput("Variable"),
    checkboxInput("checkbox_1", label = "Advanced Options", value = T),
    uiOutput('test'),
  tags$br(),
    helpText("Application built in Rstudio (0.98.507) using data...acknowledgements...")),
  tags$br(),
  mainPanel(
    tabsetPanel(type = "tabs", 
                tabPanel("Map", leafletOutput("map")),
                #tabPanel("Change over time", leafletOutput("change")),
                tabPanel("Register to Download", htmlOutput("download")),
                tabPanel("Download", 
                         helpText("The data presented here are based on your choices to the left, once you have registered, click download below the table to get a .csv file which can be opened in Excel. If you want a specific datazone, perhaps ones that was of interest in the map tab, copy and paste the name into the search box in the top right hand corner to filter the data to this single datazone."),
                         DT::dataTableOutput("dt"),
                         verbatimTextOutput("filtered_row"),
                         downloadButton(outputId = "download_filtered",
                                        label = "Download Filtered Data"))
    ))
))


