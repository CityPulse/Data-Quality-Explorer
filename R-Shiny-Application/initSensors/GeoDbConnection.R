
drv <- dbDriver("PostgreSQL")


reinitDatabaseConnection <- function(){
  if (exists('con')){
    dbDisconnect(con)
  }
  if(!offline){
    con <<- dbConnect(PostgreSQL(), user= dbuser, password=dbpasswd, dbname=dbname, port=dbport, host=dbhost)
    print(con)
  }  
}
updateRequestIds <- function(con){
  if(offine){
    request_ids <- readRDS('OfflineData/request_ids')
  }else{
  #sql <- paste("select request_id from  cp_route_requests;")
    sql <- "select request_id, st_x(from_geom) as from_x,st_y(from_geom) as from_y, st_x(to_geom) as to_x, st_y(to_geom) as to_y from  cp_route_requests;"
    request_ids_geos <<- dbGetQuery(con,sql)
    request_ids <<- request_ids_geos$request_id  
  }
  return(request_ids)
}

updateSensorUuids <- function(con){
  if(offline){
    g_sensor_uuids <- readRDS('OfflineData/g_sensor_uuids')
  }else{
    sql = "select sensor_uuid::varchar as uuid, sercvice_category as service_category, st_astext(geom)  as location, st_x(st_centroid(geom)) as longitude, st_y(st_centroid(geom)) as latitude from cp_sensors"
    response = dbSendQuery(con,sql)
    g_sensor_uuids <<-  dbFetch(response,n=-1)#[,'sensor_uuid']
  }
  g_service_categories <<- unique(g_sensor_uuids[,'service_category'])
  return(g_sensor_uuids)
}



