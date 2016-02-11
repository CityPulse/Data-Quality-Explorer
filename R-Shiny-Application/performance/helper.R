getWrapperUUIDs <- function(){
  if (performance_offline == TRUE){
    filename = paste('./data/listwrapper.json', sep='')
  }else{
    api='/api/listwrapper'
    filename=paste('http://', server, ':', port, api, sep='')
  }
  jsonWrappers = getJSONData(filename)
  if (isNaCheck(jsonWrappers)){
    return(NA)
  }
  wrappers = jsonWrappers$wrappers
  uuids = lapply(wrappers, function(x) x$uuid)
  return(uuids)
}

getWrapperUUIDsForCategory <- function(category){
  # print(paste("cat:", category))
  if (performance_offline == TRUE){
    filename = paste('./data/listwrapper.json', sep='')
  }else{
    api='/api/listwrapper'
    filename=paste('http://', server, ':', port, api, sep='')
  }
  jsonWrappers = getJSONData(filename)
  if (isNaCheck(jsonWrappers)){
    return(NA)
  }
  wrappers = jsonWrappers$wrappers
  uuids = lapply(wrappers, function(x) if(x$sensorType==category) {x$uuid})
  uuids = uuids[!sapply(uuids, is.null)]
  return(uuids)
}

getWrapperInformation <- function(uuid){
  if(!is.null(uuid) && uuid !=""){
    if (performance_offline == TRUE){
      filename = paste('./data/descriptions/', uuid, '.json', sep='')
    }else{
      api='/api/get_description?uuid='
      filename=paste('http://', server, ':', port, api, uuid, sep='')
    }
    wInfo = getJSONData(filename)
    if (isNaCheck(wInfo)){
      return(NA)
    }
    wData = wInfo$data
    return(wData)
  }
  return(NULL)
}

getServiceCategories <- function(){
  if (performance_offline == TRUE){
    filename = paste('./data/listwrapper.json', sep='')
  }else{
    api='/api/listwrapper'
    filename=paste('http://', server, ':', port, api, sep='')
  }
  jsonWrappers = getJSONData(filename)
  if (isNaCheck(jsonWrappers)){
    return(NA)
  }
  wrappers = jsonWrappers$wrappers
  categories = unique(lapply(wrappers, function(x) x$sensorType))
  sort_cat = sort(unlist(categories))
  return(sort_cat)
}


getNames <- function(category=NULL){
  if (performance_offline == TRUE){
    filename = paste('./data/categories.json', sep='')
  }else{
    api='/stat_api/avg_processing_time?category='
    filename=paste('http://', server, ':', port, api, category, sep='')
  }
  
  #get data from resource management
  all_stats_json = getJSONData(filename)
  if (isNaCheck(all_stats_json)){
    return(NA)
  }
  
  test <- function(e, layer=""){
    if(layer != ""){
      name = paste(layer, e$name, sep="_")
    }else{
      name = e$name
    }
    df = cbind(name)
    if("values" %in% names(e)){
      for(v in e$values){
        if(!is.null(v)){
          df = cbind(df, test(v, name))
        }
      }
    }
    return (df)
  }
  return(test(all_stats_json[category][[1]][[1]]))  
}

getJSONData <- function(filename){
  data <- tryCatch(
    {
      #get data from resource management
      rjson::fromJSON(file=filename, method='C')
    },
    error = function(cond){
      message(paste("Filename does not seem to exist:", filename))
      message("Error message:")
      message(cond)
      return(NA)
    }
  )
  return(data)
}

#na check without warning that data parameter might be list
isNaCheck <- function(data){
  if (!is.list(data)){
    if (is.na(data)){
      return(TRUE)
    }
  }  
  return(FALSE)
}