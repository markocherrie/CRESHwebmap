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

  headerPanel("Alcohol and Tobacco Environments in Scotland"
  , windowTitle = "Alcohol and Tobacco Environments in Scotland"),


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
    helpText("This application allows you to map alcohol and tobacco outlet density and related harm for small neighbourhoods across Scotland. Further information is available from the 'About' and 'How to Use' tabs on the right hand side."),
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
    bsTooltip("datatype", "There are different types of alcohol retailers. On-sales are when alcohol is consumed on the premises, off-sales are when it is bought at the retailer but consumed elsewhere. Some retailers offer both on and off sales.", "top"
    ),
    selectInput("datatype", "Data Type:",
                c("Alcohol On Sales" = "alcoholOn",
                  "Alcohol Off Sales" = "alcoholOff",
                  "Alcohol Both On and Off Sales" = "alcoholBoth",
                  "Alcohol Total Sales" = "alcoholTOTAL",
                  "Tobacco Total Sales" = "tobaccoTOTAL"), selected="alcoholTOTAL"),
    selectInput("year", "Year",
                list("2012" = "2012",
                     "2016" = "2016"),
                selected="2016"),
    bsTooltip("buffer", "In the density calculation this is also known as the buffer size, that is, the distance you would expect people to travel to visit these outlets. Larger buffer sizes may be more appropriate for rural areas and smaller ones for urban areas.", "top"
    ),
    selectInput("buffer", "Typical Distance to Outlet (m)",
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
                ), selected="SCO"),
  tags$br(),
    helpText("Application built in Rstudio (0.98.507)")),
  tags$br(),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Map", leafletOutput("map")),
                tabPanel("About", includeHTML("about.html")),
                tabPanel("How to use", includeHTML("howtouse.html")),
                #tabPanel("Change over time", leafletOutput("change")),
                tabPanel("Register to Download", htmlOutput("download")),
                tabPanel("Download", 
                         helpText("The data presented here are based on your choices to the left, once you have registered, click download below the table to get a .csv file which can be opened in Excel. If you want a specific datazone, perhaps ones that was of interest in the map tab, copy and paste the name into the search box in the top right hand corner to filter the data to this single datazone."),
                         DT::dataTableOutput("dt"),
                         verbatimTextOutput("filtered_row"),
                         downloadButton(outputId = "download_filtered",
                                        label = "Download Filtered Data")),
                tabPanel("Acknowledgements", includeHTML("acknowledgements.html"))
    ))
))


