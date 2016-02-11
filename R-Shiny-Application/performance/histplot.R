getHist <- function(category=NULL){
#   print("h1")
#   print(category)
  #library(rjson)
  #library(ggplot2)
  #library(rCharts)
  #library(RColorBrewer)
  #library(plyr)
  #source('config.R')
  #source('helper.R')
  
  #get UUIDs for given category
  # category="Aarhus_Road_Traffic"
  # category="Aarhus_Road_Parking"
  # category="Brasov_Air_Pollution"
  # category="Romanian_Weather"
  uuids = getWrapperUUIDsForCategory(category)
  uuidString = paste(uuids, collapse=",")
  
  if (performance_offline == TRUE){
    filename = paste('./data/uuids.json', sep='')
  }else{
    api='/stat_api/avg_processing_time?uuid='
    filename=paste('http://', server, ':', port, api, uuidString, sep='')
  }
  
  #get data from resource management
  all_stats_json = getJSONData(filename)
  if(isNaCheck(all_stats_json)){
    return(NA)
  }
  
  
  #create data structure for graph
  test <- function(e, layer=""){
    if(layer != ""){
      name = paste(layer, e$name, sep="_")
    }else{
      name = e$name
    }
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
  
  histData = NULL
  for(uuid in uuids){
    tmp = test(all_stats_json[uuid][[1]][[1]])
    if(is.null(histData)){
      histData = data.frame(t(data.frame(tmp$value, row.names=tmp$name)))
    }else{
      histData = rbind.fill(histData, data.frame(t(data.frame(tmp$value, row.names=tmp$name))))
    }
  }
  
  #change values from seconds to ms
  histData = histData * 1000
  
  plotList = list()
  
  rownames(histData) <- NULL
  for(name in names(histData)){
    g = ggplot(histData, aes_string(x =name)) + geom_histogram(binwidth=0.1) + labs(title=name) + labs(x=paste(name, " duration (ms)"), y="Count")
    plotList[[name]] = g
  }
  # print("h2")
  return(plotList)  
}