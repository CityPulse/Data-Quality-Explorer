getPiePlots <- function(uuid=NULL, category=NULL){
  #print("p1")

  #library(rjson)
  #library(ggplot2)
  #library(rCharts)
  #library(RColorBrewer)
  #source('config.R')
  
  if (!is.null(category) && category !=""){
    # category="Aarhus_Road_Traffic"
    # category="Romanian_Weather"
    # print(category)
    select = category
    if (performance_offline == TRUE){
      filename = paste('./data/categories.json', sep='')
    }else{
      api='/stat_api/avg_processing_time?category='
      filename=paste('http://', server, ':', port, api, category, sep='')
    }
  }else if (!is.null(uuid) && uuid !=""){
    # uuid="51f0f28c-0909-5a83-a310-b6bd686bf57b"
    # print(uuid)
    select = uuid
    if (performance_offline == TRUE){
      filename = paste('./data/uuids.json', sep='')
    }else{
      api='/stat_api/avg_processing_time?uuid='
      filename=paste('http://', server, ':', port, api, uuid, sep='')
    }
  }else{
    return (NULL)
  }
  
  
  
  
  
  data_for_uuid = getJSONData(filename)
  if (isNaCheck(data_for_uuid)){
    return(NA)
  }
  data_for_uuid = data_for_uuid[select] 
  data = data_for_uuid[[1]][[1]] 
  
  
  #create data structure for graph
  test <- function(e, layer=""){
    name = paste(layer, e$name)
    value = e$value
    df = data.frame(name, value, layer)
    if("values" %in% names(e)){
      for(v in e$values){
        if(!is.null(v)){
          df = rbind(df, test(v, name))
        }
      }
    }
    return (df)
  }
  
  graphData = test(data)
  
  #change values from seconds to ms
  graphData$value = graphData$value * 1000
  
  i = 0
  
  graphData$ymin=0
  graphData$ymax=0
  graphData$xmin=0
  graphData$xmax=0
  
  for (layer in unique(graphData$layer)){
    ymin=0
    ymax=0
    
    for(line in 1:nrow(graphData[graphData$layer==layer,])){
      if(line==1){
        #look for parent ymin
        ymin = graphData[graphData$name==layer,]$ymin
        if(length(ymin)>0){
          graphData[graphData$layer==layer,][line,]$ymin=ymin
          ymax=ymin
        }
        ymax=ymax + graphData[graphData$layer==layer,][line,]$value
        graphData[graphData$layer==layer,][line,]$ymax = ymax
      }else{
        ymin=ymax
        graphData[graphData$layer==layer,][line,]$ymin=ymax
        ymax=ymax + graphData[graphData$layer==layer,][line,]$value
        graphData[graphData$layer==layer,][line,]$ymax=ymax
      }
      #       graphData[graphData$layer==layer,][line,]$xmax=i+1
      #       graphData[graphData$layer==layer,][line,]$xmin=i
      graphData[graphData$layer==layer,][line,]$xmax=min(graphData[graphData$name==layer,]$xmax+1, i+1)
      graphData[graphData$layer==layer,][line,]$xmin=min(graphData[graphData$name==layer,]$xmax, i)
    }
    
    #caluclate rest time for layer
    parentsum = sum(graphData[graphData$name==layer,]$value)
    if(parentsum > 0){
      ownsum = sum(graphData[graphData$layer==layer,]$value)
      name=paste(layer,"Rest")
      value=parentsum-ownsum
      graphData = rbind(graphData, data.frame(name, value, layer, ymin=ymax, ymax=ymax+value, xmin=min(graphData[graphData$name==layer,]$xmax, i), xmax=min(graphData[graphData$name==layer,]$xmax+1, i+1)))
    }
    i = i+1
  }
  
  graphData$name = paste(graphData$name, round(graphData$value, 3), "ms", sep = " ")
  layers = length(unique(graphData$layer))
  
  colourCount = nrow(graphData)
  getPalette = colorRampPalette(brewer.pal(9, "Set1"))
  
  g = ggplot(data=NULL) + xlim(c(0, layers)) + theme(aspect.ratio=1) + 
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.background=element_blank()) + ggtitle("Time consumption per function in ms") +
    scale_fill_manual(values = getPalette(colourCount))
  
  g <- g + 
    geom_rect(data=graphData, aes(fill=name, ymin=ymin, ymax=ymax, xmin=xmin, xmax=xmax))
  
  #change barplot to pieplot and return
  # print("p2")
  return(g + coord_polar(theta='y'))
}