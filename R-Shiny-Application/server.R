print("!!! - Server")
shinyServer(function(input, output, session) {  
 
  updateSensorUuids(con)
  #print(g_sensor_uuids)
  print("End GeoDbConnection")
  
  print('Initialising Aggregated Quality Data')
  print("Update Base Data")
  updateBaseData()
  print('--- OK')
  print("Update Live Data")
  updateLiveData()
  print('--- OK')
  
  
  updateBaseData()
  
  # When map is clicked, show a popup with city info
  obsMap<-observe({
    print("ObserveMap")
    leafletProxy("map") %>% clearPopups()
    leafletProxy("map2") %>% clearPopups()
    leafletProxy("map3") %>% clearPopups()    
    event <- input$map_shape_click
    if (is.null(event))
      return()
    isolate({
      showQoiPopup(event$id, event$lat, event$lng)
    })
  })
  
  obsRoute<-observe({
    print("ObserveRoute")
    leafletProxy("map3") %>% clearPopups()  
    event <- input$map3_geojson_click
    if (is.null(event))
      return()
    isolate({
      print(event)
      showRoutePopup(event$properties$request_id, event$properties$route_id, event$properties$cost, event$lat, event$lng)
      })  
  })
  
  ## Interactive Map ###########################################  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      #addProviderTiles("CartoDB.Positron")%>% 
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 10.21, lat = 56.1575, zoom = 14)
  })
 

  qoiInBounds <- reactive({
    print("!!!in the subset")
    if (is.null(input$map_bounds)){
      #print("bounds=null")
      return(qoidatalist[[input$sourceselect]][FALSE,])
    }
    bounds <- input$map_bounds
    print(bounds)
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)     
    #print(qoidatalist[[input$sourceselect]])
    set<<-subset(qoidatalist[[input$sourceselect]],
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
    print(paste(nrow(set),"/",nrow(qoidatalist[[input$sourceselect]])))
    #set
    #TODO joined filter
    #set <- subset(set, !is.na(age))
    #set <- subset(set, !is.na(frequency))
    #set <- subset(set, !is.na(completeness))
    #set <- subset(set, !is.na(latency))
    return(set)
  })
  
  output$qoitext <- renderText({ 
    'QoI Score (visible sensors)'
  })
  
  getRenderHistPlot <- function(parameterName, aggregation, analysis, maintitle=NULL){#Function to create histograms
    if (aggregation=="current"){
      colorBy <- paste0(parameterName,".",aggregation,".rated")      
    } else{
      colorBy <- paste0(parameterName,".",aggregation,".rated.",analysis)
    }
    #print("!!!HIST")
    #print(colorBy)
    #print(colnames(qoiInBounds()))
    data<-qoiInBounds()[,colorBy]
    #print(nrow(data))
    #histdebugdata <<- data
    #print('!')
    if (length(data) == 0)##SHOULDNT THIS BE AVOIDED?? Recalc!!!
      return(NULL)
    qoiBreaks <- seq(0,1,0.1)
    #par(mar=c(4,3,0,0)+0.1)
    par(mar=c(4,5,0,0)+0.1)
    h <- hist(as.numeric(data),
              breaks = qoiBreaks,
              #main = "QoI Score (all visible sensors)",
              main = maintitle,
              xlab = parameterName,
              ylab = "#",
              xlim = range(qoiBreaks),#range(data[,1]),
              col = histcol,
              border = 'white')
    return(h) 
  }
  
#   getRenderHistPlot <- function(parameterName, aggregation, analysis, maintitle=NULL){#Function to create histograms
#     if (aggregation=="current"){
#       colorBy <- paste0(parameterName,".",aggregation,".rated")      
#     } else{
#       colorBy <- paste0(parameterName,".",aggregation,".rated.",analysis)
#     }
#     return(ecdf(qoiInBounds()[,colorBy]))
#   }
  output$qoiHistFrequency <- renderPlot({
    aggregation <- input$aggregation
    analysis <- input$analysis
    getRenderHistPlot('frequency', aggregation, analysis)
  })
    
  output$qoiHistFrequency <- renderPlot({
    aggregation <- input$aggregation
    analysis <- input$analysis
    getRenderHistPlot('frequency', aggregation, analysis)
  })
  
  output$qoiHistAge <- renderPlot({
    aggregation <- input$aggregation
    analysis <- input$analysis
    getRenderHistPlot('age', aggregation, analysis)
  })
  
  output$qoiHistCorrectness <- renderPlot({
    aggregation <- input$aggregation
    analysis <- input$analysis
    getRenderHistPlot('correctness', aggregation, analysis)
  })
  
  output$qoiHistCompleteness <- renderPlot({
    aggregation <- input$aggregation
    analysis <- input$analysis
    getRenderHistPlot('completeness', aggregation, analysis)
  })
  
  
  output$qoiHistLatency <- renderPlot({
    aggregation <- input$aggregation
    analysis <- input$analysis
    getRenderHistPlot('latency', aggregation, analysis)
  })
  output$scatter <- renderPlot({
    print(paste("lastuuid ", lastuuid)) 
    event <- input$map_shape_click
    if(!is.null(lastuuid)){
      colorBy <- input$color
      #inbounds<-qoiInBounds()
      inbounds<-qoidatalist[[input$sourceselect]][qoidatalist[[input$sourceselect]]$uuid == lastuuid,]
      #print(xyplot(age ~ timestamp, data = qoiInBounds(), xlim = range(qoidata$timestamp), ylim = range(qoidata$age),main=lastuuid, strip = FALSE))
      ggplot(inbounds) + geom_line(aes(timestamp, age, color="age")) + 
        geom_line(aes(timestamp, frequency, color="freq.")) + 
        geom_line(aes(timestamp, completeness, color="comp.")) + 
        geom_line(aes(timestamp, latency, color="lat.")) + 
        guides(fill=guide_legend(title=NULL))+
        theme(legend.title=element_blank(), legend.position = "top")+
        scale_x_datetime() + xlab("Date") + ylab("QoI Score")
        #ggtitle(lastuuid)
    }else { 
      return(NULL)     
    }
  })
  
  showQoiPopup <- function(uuid, lat, lng) {
    print(paste("Showing Popup for ", uuid, " at: ", lng,"/",lat))
    selectedSensor <<- qoidatalist[[input$sourceselect]][qoidatalist[[input$sourceselect]]$uuid == uuid,]
    values <- list()
    for (metric in vars){
      cols1 <- paste0(metric,".current.rated")      
      cols2 <- paste(metric,".",'daily',".rated.",c('min','max','avg'), sep='')
      cols3 <- paste(metric,".",'hourly',".rated.",c('min','max','avg'), sep='')
      valuecols <- c(cols1,cols2,cols3)
      values[[metric]] <- as.numeric(selectedSensor[1,c(valuecols)])
    }
    s.df <<- data.frame(values)
    colnames(s.df)<-c('Age', 'Freq.', 'Lat.','Compl.','Correct.')
    rownames(s.df)<-c('Current','Daily Min.', 'Daily Max.','Daily Avg.','Hourly Min.', 'Hourly Max.','Hourly Avg.')
    xtab<-xtable(s.df, caption="Quality Metrics")
    digits(xtab) <- 2
    content <- as.character(
      tagList(
        tags$h4("Stream:", uuid),
        tags$h5("Service Category: ",selectedSensor[1,'service_category']),        
        #HTML(kable(s.df, format='html', digits=2, pad=10))   
        HTML(print(xtab, type="html", include.rownames = TRUE,scalebox = 0.7, html.table.attributes = list('border="1" bgcolor="#FFCC00"')))
      )
    )
    lastuuid<<-uuid
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = uuid)
  }

  showRoutePopup <- function(request_id, route_id, cost, lat, lng) {
    content <- as.character(
      tagList(
        tags$h4("Route:", route_id),
        tags$strong(HTML(sprintf("Cost: %.2f",cost))), tags$br(),
        paste("from Request: ", request_id )
      )
    )
    leafletProxy("map3") %>% addPopups(lng, lat, content)
  }

  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  obsColor<-observe({
    print("Observe Color") 
    qoimetric <- input$qoimetric
    aggregation <- input$aggregation
    analysis <- input$analysis
    if (aggregation=="current"){
      colorBy <- paste0(qoimetric,".",aggregation,".rated")      
    } else{
      colorBy <- paste0(qoimetric,".",aggregation,".rated.",analysis)
    }
    #print(colorBy)
    event <- input$nav ##has to b here to reload
    print("Updating Live Date")
    
    #TODO
    if(!offline){
    print("update Live Data")
    updateLiveData()
    }
    print("--- OK(Update)")
    #Color    
    debugqoiuinbounds <<- qoiInBounds()    
    print("Subsetting Data")
    #print(colnames(qoiInBounds()))   
    #print(paste("Looking for Column", colorBy))
    datasubset <- qoiInBounds()[c('longitude','latitude','uuid', colorBy)]
    #print("--- QoI in Bounds")
    #print(nrow(datasubset))
    print("--- OK(Subset)")
    colnames(datasubset) <- c('longitude','latitude','uuid','val')
    datasubset <- subset(datasubset, !is.na(val))
    
   aggreg <- subset(datasubset, !is.na(val)) # datasubset#aggregated in the interface

    
   print("Coloring")
   aggreg[['val']] <- as.numeric(aggreg[['val']])
   colorData <- aggreg[['val']]

   if((length(unique(colorData))>1) && abs(max(colorData)-min(colorData))>0.01 && (input$colorscale!='fullscale')){
        pal <- colorBin(input$colorchooser, colorData, 7, pretty = FALSE)
      }else{
        pal <- colorBin(input$colorchooser, c(1,0), 10, pretty = FALSE)#if we just have 1 color we cant do a scale
      }
  
  print(paste("Should now print ",nrow(aggreg)))
   map <<- leafletProxy("map", data = aggreg, deferUntilFlush=FALSE) %>%
      clearShapes() %>% #clearLegends()%>%
     addCircles(~longitude, ~latitude,  radius=70, layerId=~uuid, stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      #addGeoJSON(geoj, stroke=TRUE, fill=FALSE, fillOpacity=0.4) %>%
      #addCircles(~longitude, ~latitude,  radius=70, layerId=~uuid, stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      #addCircleMarkers(~longitude, ~latitude, radius=10, layerId=~uuid, stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%         
      addLegend(position="bottomleft", pal=pal, values=colorData, title=colorBy, layerId="map")
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  obsAnno<-observe({
    print("ObserveAnnotation") 
    event <- input$nav ##has to b here to reload   
    #datasubset <- qoiInBounds()[c('longitude','latitude','streamid', colorBy)]    
    map2 <<- leafletProxy("map2",  deferUntilFlush=FALSE) %>%
      clearShapes() 
    print("--- OK(ObserveAnnotation)")
    })
 


  obsColor<-observe({
    event <- input$nav ##has to b he
    if(event=='Routing'){
      print(paste ("EVENT:",event))     
      updateRequestIds(con)
      updateSensorUuids(con)
      print(paste("g_sensor_uuids: ",g_sensor_uuids))
    }
  })


  #Performance
  options = reactive({
    input$category
  })
  
  offlineMode = reactive({
    input$offlineMode
  })
  
  performanceOfflineMode = reactive({
    input$performanceOfflineMode
  })
  
  observe({
    offline <<- as.logical(offlineMode())
    performance_offline <<- as.logical(performanceOfflineMode())
    tryCatch({
      updateSelectInput(session, inputId = "category", choices = getServiceCategories())
    },
    error=function(cond){
      updateSelectInput(session, inputId = "category", choices = "NA")
      message("error") 
    })
    reinitDatabaseConnection()
  })
  
  observe({
    tryCatch({
      uuids = getWrapperUUIDsForCategory(options())
      updateSelectInput(session, inputId = "uuid", choices = uuids)
    },
    error=function(cond){
      updateSelectInput(session, inputId = "uuid", choices = "NA")
      message("error")
    })
  })
  
  observe({
    tryCatch({
      options = getNames(category=input$category)
      updateSelectInput(session, inputId = "histName", choices = options)
    },
    error=function(cond){
      updateSelectInput(session, inputId = "histName", choices = "NA")
    })
  })
  
  output$histplot_functions <- renderPlot({
    if (input$histName !="" && !is.na(input$histName)){
      try(getHist(category=input$category)[input$histName])
    }
  })
  
  output$pieplots_category <- renderPlot({
    try(getPiePlots(category=input$category))
  })
  
  output$pieplots_uuid <- renderPlot({
    try(getPiePlots(uuid=input$uuid))
  })
  
  output$info <- renderUI({
    tryCatch({
      wData <- getWrapperInformation(input$uuid)
      HTML(paste(wData$sensorName, wData$sensorType[1], wData$streetName[1], paste(wData$postalCode[1], wData$cityName[1], sep=" "), sep=" <br/>"))
    },
    error=function(cond){
      HTML(paste("NA", sep="<br/>"))
    })
  })
  
  output$header_category <- renderUI({
    headerPanel(paste("Average times for", input$category, sep=" "))
  })
  
  output$header_uuid <- renderUI({
    headerPanel(paste("Average times for", input$uuid, sep=" "))
  })

})


 
