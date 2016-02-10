library(shiny)
library(leaflet)
source("performance/helper.R")

# Choices for drop-downs

vars <<- list(
  #"StreamId" = "uuid",
  "Age" = "age",
  "Frequency" = "frequency",
  "Latency" = "latency",
  "Completeness" = "completeness",
  "Correctness" = "correctness"
)

analys <- list(
  "Average" = "avg",
  "Minimum" = "min",
  "Maximum" = "max"
)

timeframe = list(
  "Current"="current",
  "Hourly"="hourly",
  "Daily"="daily"  
  )

sourceconfig <- c(
  "Live View" = "live"
)

colorconfig <- c(
  "Full Scale" = "fullscale",
  "Adopted Scale" = "adoptedscale"
  )
colorchoice <- c(
  "Red-Yellow-Green" = "RdYlGn",
  "Spektral" = "Spectral"
)
vertsize=135;

shinyUI(navbarPage("CityPulse QoI Explorer", id="nav",
  tabPanel("Interactive map",
    div(class="outer",
      tags$head( # Include our custom CSS
        includeCSS("styles.css")#,
        #includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 500, height = "auto",
        fluidPage(
          fluidRow(
            column(12,
                   selectInput("qoimetric", "Qoi Metric", vars)
            )),
            fluidRow(
              column(6,selectInput("aggregation", "Time/Aggregation", timeframe)),
              column(6,selectInput("analysis", "Analysis", analys))
              ),
          fluidRow(
            column(12,
                   textOutput('qoitext'),                   
            plotOutput("qoiHistFrequency", height = 1.2*vertsize),
            plotOutput("qoiHistAge", height = vertsize),
            plotOutput("qoiHistLatency", height = vertsize),
            plotOutput("qoiHistCorrectness", height = vertsize),
            plotOutput("qoiHistCompleteness", height = vertsize)
            #plotOutput("scatter", height = 2*vertsize)
            )
          )
        )
      ),
      
       tags$div(id="cite",
         'Data compiled from CityPulse ', tags$em('Open Datasets available at'), ' http://www.ict-citypulse.eu/'
       )
    )
  ),
  tabPanel("Wrapper Time Measurement",
        div(class="outer",
            tags$head( # Include our custom CSS
              includeCSS("styles.css")#,
            ),
            
            sidebarLayout(position = "right",
                          sidebarPanel(
                            selectInput("category", "Select category", getServiceCategories()),
                            selectInput("histName", "Select histogram", NULL),
                            plotOutput("histplot_functions"),
                            selectInput("uuid", "Select UUID", NULL),
                            htmlOutput("info")
                          ),
                          mainPanel(
                            uiOutput("header_category"),
                            plotOutput("pieplots_category"),
                            uiOutput("header_uuid"),
                            plotOutput("pieplots_uuid")   
                          )
            )
            
        )        
    ),
  tabPanel("Configuration",
           selectInput("colorscale", "Color Scale", colorconfig, selected = 'fullscale'), 
           selectInput("colorchooser", "Color Choice", colorchoice, selected = 'Red-Yellow-Green'),          
           selectInput("offlineMode", "Offline mode", c(FALSE, TRUE), selected = offline),
           selectInput("sourceselect", "Data Source", sourceconfig) 
  ),
  
  conditionalPanel("false", icon("crosshair"))
))

