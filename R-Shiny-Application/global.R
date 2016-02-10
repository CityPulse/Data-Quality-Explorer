print("!!! - Global")
source('config.R')
library(RPostgreSQL)## if the package is not already installed, use install.packages('RPostgreSQL') and install.packages(DBI)  
library(plyr)
library(dplyr)
library(RCurl)
library(sp)
library(rgeos)
library(maptools)
library(gdata)
library(knitr, ,quietly=T)
library(xtable)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(rgdal)
library(rgeos)
library(ggplot2)
library(rjson)
library(jsonlite)

print('Loading Performance Component')
source("performance/pieplots.R")
source("performance/helper.R")
source("performance/histplot.R")
print('--- OK')
print('Initialising GeoDbConnection')
source(file="initSensors/GeoDbConnection.R")
reinitDatabaseConnection()
print('--- OK')

histcol <- '#00ee00'


options(scipen=999)

switchXyLive <- TRUE
crs=CRS("+proj=longlat +datum=WGS84") #EPSG:4360
crsS70=CRS("+proj=sterea +lat_0=46 +lon_0=25 +k=0.99975 +x_0=500000 +y_0=500000 +ellps=krass +units=m +no_defs ") #EPSG:31700

lastuuid <- NULL
map <- NULL
spatialdataLines <- list()
spatialdataPoints <- list()
multiwkt <- list()
lastStreamPool <- list()
qoidatalist <- list()

getDataset <- function (filename=NULL, url=NULL, switched){
  if(!is.null(filename)){
    qoidataimport <- read.csv(file=filename,sep=',',header=TRUE, quote='\"',stringsAsFactors=FALSE)
    print(paste("importing data: ", filename))
  }else if(!is.null(url)){
    print(paste("importing data: ", url))
    x <- getURL(url)
    qoidataimport <- read.csv(text = x,sep=',',header=TRUE, quote='\"',stringsAsFactors=FALSE)
  }else{
    print("ERROR!")
    return (NULL)
  }
  print(" importing data: csv read")
  qoidata <- qoidataimport %>%
    select(
      streamid = streamId,
      location = location,
      timestamp = timestamp,
      frequency=Frequency,
      age=Age,
      latency=Latency,
      completeness=Completeness
    )
  
  ##SatialLines/PointsDataFrame
  distinct <- unique(qoidata[c("streamid","location")])
  rownames(distinct)<-distinct$streamid
  distinctPoints <- subset(distinct, startsWith(distinct$location, "POINT"))
  distinctLines <- subset(distinct, startsWith(distinct$location, "LINE")| startsWith(distinct$location, " LINE"))
  geomstringLines = paste(distinctLines$location, sep=', ', collapse=', ')
  geomstringPoints = paste(distinctPoints$location, sep=', ', collapse=', ') 
  if(nchar(geomstringLines)>1){
    spatialLines= readWKT(paste('GEOMETRYCOLLECTION(',geomstringLines,')'))
    if(switched) {
      spatialLines <- elide(spatialLines, flip=TRUE)
    }
    SLDF = SpatialLinesDataFrame(spatialLines,data=distinctLines,match.ID=FALSE)
      spatialdataLines[filename] <<- SLDF  
  }    
  if(nchar(geomstringPoints)>1){
    spatialPoints= readWKT(paste('GEOMETRYCOLLECTION(',geomstringPoints,')')) 
    if(switched) {
      spatialPoints <- elide(spatialPoints, flip=TRUE)
    }
    SPDF = SpatialPointsDataFrame(spatialPoints,data=distinctPoints,match.ID=FALSE)
    spatialdataPoints[filename] <<- SPDF     
  }
  
  #Individual SpatialLines as Part of data.frame
  distinct$wkt <- unlist(sapply(distinct$location, "readWKT", p4s=crs))
  multiwkt[filename] <<- distinct

  ##Simple XY Position
  loc1 <- sapply(strsplit(qoidata$location, c("(\\(|\\)|,)") ), "[[", 2)
  loc2 <- unlist(strsplit(loc1,"\\ "))
  if (switched){
    latpos=1
    longpos=2
  } else{
    latpos=2
    longpos=1
  } 
  qoidata$longitude <- as.numeric(loc2[seq(longpos, length(loc2), 2)])
  qoidata$latitude <- as.numeric(loc2[seq(latpos, length(loc2), 2)])
  
  
  
  qoidata$timestamp <- as.POSIXct(qoidata$timestamp, origin = "1970-01-01", tz = "GMT")
  print(" finished import")
  return(qoidata)
}

nullToNA <- function(x) {
  x[x=='inf'] <- NA
  x[sapply(x, is.null)] <- NaN
  return(x)
}

df_from_json_last <- function(all_qual, json_object){
  df1 <- allQualDf #DEBUG All Qual to be Updated
  json_object <- all_last_qual #DEBUG
  
  uuid = unlist(nullToNA(as.character(sapply(json_object, "[[", 'uuid'))))
  frequency = lapply(json_object, "[[", 'Frequency')
  age =  lapply(json_object, "[[", 'Age')
  completeness =  lapply(json_object, "[[", 'Completeness')
  latency =  lapply(json_object, "[[", 'Latency')
  correctness =  lapply(json_object, "[[", 'Correctness')
  
  frequency.unit<-unlist(nullToNA(lapply(frequency, "[[", 'unit')))
  age.unit<-unlist(nullToNA(lapply(age, "[[", 'unit')))
  completeness.unit<-unlist(nullToNA(lapply(completeness, "[[", 'unit')))
  latency.unit<-unlist(nullToNA(lapply(latency, "[[", 'unit')))
  correctness.unit<-unlist(nullToNA(lapply(correctness, "[[", 'unit')))
  #frequency.unit, age.unit, completeness.unit, latency.unit, correctness.unit
  
  frequency.current<-lapply(frequency, "[[", 'CURRENT')
  age.current<-lapply(age, "[[", 'CURRENT')
  completeness.current<-lapply(completeness, "[[", 'CURRENT')
  latency.current<-lapply(latency, "[[", 'CURRENT')
  correctness.current<-lapply(correctness, "[[", 'CURRENT')
  
  frequency.current.rated<-unlist(nullToNA(lapply(frequency.current, "[[", 'ratedValue')))
  age.current.rated<-unlist(nullToNA(lapply(age.current, "[[", 'ratedValue')))
  completeness.current.rated<-unlist(nullToNA(lapply(completeness.current, "[[", 'ratedValue')))
  latency.current.rated<-unlist(nullToNA(lapply(latency.current, "[[", 'ratedValue')))
  correctness.current.rated<-unlist(nullToNA(lapply(correctness.current, "[[", 'ratedValue')))
  
  df2 = data.frame(uuid, 
                  frequency.current.rated, age.current.rated, completeness.current.rated, latency.current.rated, correctness.current.rated, 
                  stringsAsFactors=FALSE)
  
  for (i in 1:nrow(df1)){
    for(col in colnames(df2)){
      if(!is.null(df2[i,col]) && !is.na(df2[i,col])){
        df1row <- which(df1[,'uuid']==df2[i,'uuid'])
        df1[df1row,col] = df2[i,col]
      }
    }
  }
    
  return(df1)
  }



df_from_json_all <- function(sensor_uuids_df, json_object){
  uuid = unlist(nullToNA(as.character(sapply(json_object, "[[", 'uuid'))))
  frequency = lapply(json_object, "[[", 'Frequency')
  age =  lapply(json_object, "[[", 'Age')
  completeness =  lapply(json_object, "[[", 'Completeness')
  latency =  lapply(json_object, "[[", 'Latency')
  correctness =  lapply(json_object, "[[", 'Correctness')
  
  frequency.unit<-unlist(nullToNA(lapply(frequency, "[[", 'unit')))
  age.unit<-unlist(nullToNA(lapply(age, "[[", 'unit')))
  completeness.unit<-unlist(nullToNA(lapply(completeness, "[[", 'unit')))
  latency.unit<-unlist(nullToNA(lapply(latency, "[[", 'unit')))
  correctness.unit<-unlist(nullToNA(lapply(correctness, "[[", 'unit')))
  #frequency.unit, age.unit, completeness.unit, latency.unit, correctness.unit
  
  frequency.current<-lapply(frequency, "[[", 'CURRENT')
  age.current<-lapply(age, "[[", 'CURRENT')
  completeness.current<-lapply(completeness, "[[", 'CURRENT')
  latency.current<-lapply(latency, "[[", 'CURRENT')
  correctness.current<-lapply(correctness, "[[", 'CURRENT')
 
  frequency.current.rated<-unlist(nullToNA(lapply(frequency.current, "[[", 'ratedValue')))
  age.current.rated<-unlist(nullToNA(lapply(age.current, "[[", 'ratedValue')))
  completeness.current.rated<-unlist(nullToNA(lapply(completeness.current, "[[", 'ratedValue')))
  latency.current.rated<-unlist(nullToNA(lapply(latency.current, "[[", 'ratedValue')))
  correctness.current.rated<-unlist(nullToNA(lapply(correctness.current, "[[", 'ratedValue')))
  
  #Hourly
  frequency.hourly<-lapply(frequency, "[[", 'HOURLY')
  age.hourly<-lapply(age, "[[", 'HOURLY')
  completeness.hourly<-lapply(completeness, "[[", 'HOURLY')
  latency.hourly<-lapply(latency, "[[", 'HOURLY')
  correctness.hourly<-lapply(correctness, "[[", 'HOURLY')
  
  frequency.hourly.rated.min<-unlist(nullToNA(lapply(frequency.hourly, "[[", 'ratedMin')))
  age.hourly.rated.min<-unlist(nullToNA(lapply(age.hourly, "[[", 'ratedMin')))
  completeness.hourly.rated.min<-unlist(nullToNA(lapply(completeness.hourly, "[[", 'ratedMin')))
  latency.hourly.rated.min<-unlist(nullToNA(lapply(latency.hourly, "[[", 'ratedMin')))
  correctness.hourly.rated.min<-unlist(nullToNA(lapply(correctness.hourly, "[[", 'ratedMin')))  
  
  frequency.hourly.rated.max<-unlist(nullToNA(lapply(frequency.hourly, "[[", 'ratedMax')))
  age.hourly.rated.max<-unlist(nullToNA(lapply(age.hourly, "[[", 'ratedMax')))
  completeness.hourly.rated.max<-unlist(nullToNA(lapply(completeness.hourly, "[[", 'ratedMax')))
  latency.hourly.rated.max<-unlist(nullToNA(lapply(latency.hourly, "[[", 'ratedMax')))
  correctness.hourly.rated.max<-unlist(nullToNA(lapply(correctness.hourly, "[[", 'ratedMax')))
  
  frequency.hourly.rated.avg<-unlist(nullToNA(lapply(frequency.hourly, "[[", 'ratedAvg')))
  age.hourly.rated.avg<-unlist(nullToNA(lapply(age.hourly, "[[", 'ratedAvg')))
  completeness.hourly.rated.avg<-unlist(nullToNA(lapply(completeness.hourly, "[[", 'ratedAvg')))
  latency.hourly.rated.avg<-unlist(nullToNA(lapply(latency.hourly, "[[", 'ratedAvg')))
  correctness.hourly.rated.avg<-unlist(nullToNA(lapply(correctness.hourly, "[[", 'ratedAvg')))
  
  #Daily
  frequency.daily<-lapply(frequency, "[[", 'DAILY')
  age.daily<-lapply(age, "[[", 'DAILY')
  completeness.daily<-lapply(completeness, "[[", 'DAILY')
  latency.daily<-lapply(latency, "[[", 'DAILY')
  correctness.daily<-lapply(correctness, "[[", 'DAILY')
  
  frequency.daily.rated.min<-unlist(nullToNA(lapply(frequency.daily, "[[", 'ratedMin')))
  age.daily.rated.min<-unlist(nullToNA(lapply(age.daily, "[[", 'ratedMin')))
  completeness.daily.rated.min<-unlist(nullToNA(lapply(completeness.daily, "[[", 'ratedMin')))
  latency.daily.rated.min<-unlist(nullToNA(lapply(latency.daily, "[[", 'ratedMin')))
  correctness.daily.rated.min<-unlist(nullToNA(lapply(correctness.daily, "[[", 'ratedMin')))
  
  frequency.daily.rated.max<-unlist(nullToNA(lapply(frequency.daily, "[[", 'ratedMax')))
  age.daily.rated.max<-unlist(nullToNA(lapply(age.daily, "[[", 'ratedMax')))
  completeness.daily.rated.max<-unlist(nullToNA(lapply(completeness.daily, "[[", 'ratedMax')))
  latency.daily.rated.max<-unlist(nullToNA(lapply(latency.daily, "[[", 'ratedMax')))
  correctness.daily.rated.max<-unlist(nullToNA(lapply(correctness.daily, "[[", 'ratedMax')))
  
  frequency.daily.rated.avg<-unlist(nullToNA(lapply(frequency.daily, "[[", 'ratedAvg')))
  age.daily.rated.avg<-unlist(nullToNA(lapply(age.daily, "[[", 'ratedAvg')))
  completeness.daily.rated.avg<-unlist(nullToNA(lapply(completeness.daily, "[[", 'ratedAvg')))
  latency.daily.rated.avg<-unlist(nullToNA(lapply(latency.daily, "[[", 'ratedAvg')))
  correctness.daily.rated.avg<-unlist(nullToNA(lapply(correctness.daily, "[[", 'ratedAvg')))
  
  df = data.frame(uuid, frequency.unit, age.unit, completeness.unit, latency.unit, correctness.unit,
        frequency.current.rated, age.current.rated, completeness.current.rated, latency.current.rated, correctness.current.rated, 
        frequency.hourly.rated.min, age.hourly.rated.min, completeness.hourly.rated.min, latency.hourly.rated.min, correctness.hourly.rated.min,
        frequency.hourly.rated.max, age.hourly.rated.max, completeness.hourly.rated.max, latency.hourly.rated.max, correctness.hourly.rated.max,
        frequency.hourly.rated.avg, age.hourly.rated.avg, completeness.hourly.rated.avg, latency.hourly.rated.avg, correctness.hourly.rated.avg,
        frequency.daily.rated.min, age.daily.rated.min, completeness.daily.rated.min, latency.daily.rated.min, correctness.daily.rated.min,
        frequency.daily.rated.max, age.daily.rated.max, completeness.daily.rated.max, latency.daily.rated.max, correctness.daily.rated.max,
        frequency.daily.rated.avg, age.daily.rated.avg, completeness.daily.rated.avg, latency.daily.rated.avg, correctness.daily.rated.avg,
        stringsAsFactors=FALSE)
 
  all_qual_df_full <<- join(sensor_uuids_df,df,by='uuid')
  return(all_qual_df_full)
}


updateLiveData <- function(){
  result = tryCatch({
    if(offline){
      print("offline")
      allQualDf=readRDS('OfflineData/allQualDf')
      print(paste("OfflineDataCount: ",nrow(allQualDf)))      
    }else{
    print('Initialising First Quality Data')
      last_qual_url = paste0('http://',server,':',port,'/api/getAllLastQualities')
      all_last_qual <<- rjson::fromJSON(file=last_qual_url, method='C')
      allQualDf <<- df_from_json_last(g_sensor_uuids, all_last_qual) 
      print(paste("LiveDataCount: ",nrow(allQualDf)))    
    }
    qoidatalist[['live']] <<- allQualDf
  }, warning = function(w) {
    print(paste("Warning getting ", url, w))
  }, error = function(e) {
    print(paste("Warning getting ", url, e))
  }, finally = {    
  })  
}

updateBaseData <- function(){
  if(offline){
    allQualDf=readRDS('OfflineData/allQualDf')
  }else{
    all_qual_url = paste0('http://',server,':',port,'/api/getQualityValues?types=DAILY,HOURLY&avg=True&minimum=True&maximum=True')
    all_qual = rjson::fromJSON(file=all_qual_url)
    allQualDf <<- df_from_json_all(g_sensor_uuids, all_qual)
  }
  qoidatalist[['live']] <<- allQualDf#qoidatalist <<- list('live'=allQualDf)
}



