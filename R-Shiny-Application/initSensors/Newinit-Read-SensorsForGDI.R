library(raster)
library(rgdal)
require(RPostgreSQL)
library(ggplot2)   
library(ggmap)          # for ggmap(...) and get_map(...)
library(plyr) 
library(Rmisc)
#install.packages('Rmisc')

#dsn="PG:dbname='gis' host='localhost' user='wp4' password='wp4natss!' port=5439 table='cp_sensor_corr_view'"
#ras <- rgdal::readGDAL(dsn) # Get your file as SpatialGridDataFrame

#ras2 <- raster(ras,1) # Convert the first Band to Raster
#plot(ras2)

#dsn="OCI:wp4/wp4natss!@localhost:5439"
#ogrListLayers(dsn)

dbport=5439#5433
drv <- dbDriver("PostgreSQL")
if (exists('con')){
  dbDisconnect(con)
}
con <- dbConnect(PostgreSQL(), user= "wp4", password='wp4natss!', dbname="gis", port=dbport, host="localhost")
#sp = dbReadSpatial(con, tablename="cp_sensor_corr_view")

dbReadSpatial <- function(con, schemaname="public", tablename, geomcol="geom", idcol=NULL, whereClause=NULL) { 
  # con:          A PostgreSQL connection (from RPostgreSQL)
  # spatial.df:   A Spatial Data Frame object
  # schemaname:   Target schema name
  # tablename:    Target table name
  # geomcol:      Name of the geometry column in the target table (target table may not have more than one geometry column!)
  # idcol:        Name of the column with unique IDs to be used in the ID slot of the spatial objects (not relevant for point data)
  #schemaname="public"
  #tablename='cp_sensors'
  #geomcol="geom"
  #idcol="sensor_uuid"
  ## Build query and fetch the target table
  # Get column names
  q.res <- dbSendQuery(con, statement=paste("SELECT column_name FROM information_schema.columns WHERE table_name ='", tablename, "' AND table_schema ='", schemaname, "';", sep=""))
  schema.table = paste(schemaname, ".", tablename, sep="")
  q.df <- fetch(q.res, -1)
  
  # Some safe programming
  if (!(geomcol %in% q.df[,1])) {stop(paste("No", geomcol, "column in specified table."))}
  if (!is.null(idcol)) {
    if (!(idcol %in% q.df[,1])) {stop(paste("Specified idname '", idcol, "' not found.", sep=""))}
  }
  
  whereAddClause <- ' '
  if (!is.null(whereClause)){
    whereAddClause <- paste0(" WHERE ", whereClause)
  }
  # Get table
  query <- paste("SELECT", paste(q.df[,1][q.df[,1] != geomcol], collapse=", "), paste(", ST_ASTEXT(", geomcol, ") AS ",geomcol," FROM", sep=""), schema.table, whereAddClause, ";")
  t.res <- dbSendQuery(con, statement=query)
  t.df <<- fetch(t.res, -1)
  
  ## Get geometry ID column number
  if (!is.null(idcol)) {
    idcolnum <- which(names(t.df) == idcol)
  } else {
    t.df$id.new <- 1:nrow(t.df)
    idcolnum <- which(names(t.df) == "id.new")
  }
  
  ## Get geometry column number
  geomcolnum <- which(names(t.df) == geomcol)
  
  ## Build spatial data frame using OGR
  write.df <<- t.df[,geomcolnum,drop=FALSE]
  names(write.df) <- "WKT"
  filename <- paste("vector_", as.character(format(Sys.time(), "%H_%M_%S")), sep="")
  filename.csv <- paste(filename, ".csv", sep="")
  write.csv(write.df, paste(gsub("[\\]", "/", tempdir()), "/", filename.csv, sep=""), row.names=TRUE)
  down.spdf <- readOGR(dsn=paste(gsub("[\\]", "/", tempdir()), "/", filename.csv, sep=""), layer=filename, verbose=FALSE)
  rv <- file.remove(paste(gsub("[\\]", "/", tempdir()), "/", filename.csv, sep=""))
  data.df <- data.frame(t.df[,-geomcolnum])
  names(data.df) <- names(t.df)[-geomcolnum]  
  
  # For Spatial Points Data Frame  
  if (grepl("POINT", t.df[1,geomcolnum])) {
    spatial.df <-  SpatialPointsDataFrame(down.spdf@coords, data.df, match.ID=FALSE)
  }
  # For Spatial Polygons/Lines Data Frame    
  if (grepl("POLYGON", t.df[1,geomcolnum]) | grepl("LINE", t.df[1,geomcolnum])) {
    spatial.df <- down.spdf
    spatial.df@data <- data.df
    spatial.df <- spChFIDs(spatial.df, paste(t.df[,idcolnum]))
  }
  return(spatial.df)
}



dbWriteSpatial <- function(con, spatial.df, schemaname="public", tablename, replace=FALSE) {
  
  # con:          A PostgreSQL connection (from RPostgreSQL)
  # spatial.df:   A Spatial Data Frame object
  # schemaname:   Target schema name
  # tablename:    Target table name
  # replace:      Replace the target table if it already exists
  
  # Create well known text and add to spatial DF
  spatialwkt <- writeWKT(spatial.df, byid=TRUE)
  spatial.df$wkt <- spatialwkt
  
  # Add temporary unique ID to spatial DF
  spatial.df$spatial_id <- 1:nrow(spatial.df)
  
  # Set column names to lower case
  names(spatial.df) <- tolower(names(spatial.df))
  
  # Upload DF to DB
  data.df <- spatial.df@data
  rv <- dbWriteTable(con, c(schemaname, tablename), data.df, overwrite=replace, row.names=FALSE)
  
  # Create geometry column and clean up table
  schema.table <- paste(schemaname, ".", tablename, sep="")
  query1 <- paste("ALTER TABLE ", schema.table, " ADD COLUMN the_geom GEOMETRY;", sep="")
  query2 <- paste("UPDATE ", schema.table, " SET the_geom = ST_GEOMETRYFROMTEXT(t.wkt) FROM ", schema.table, " t  WHERE t.spatial_id = ", schema.table, ".spatial_id;", sep="")
  query3 <- paste("ALTER TABLE ", schema.table, " DROP COLUMN spatial_id;")
  query4 <- paste("ALTER TABLE ", schema.table, " DROP COLUMN wkt;")
  er <- dbSendQuery(con, statement=query1)
  er <- dbSendQuery(con, statement=query2)
  er <- dbSendQuery(con, statement=query3)
  er <- dbSendQuery(con, statement=query4) 
  return(TRUE)
}



sp = dbReadSpatial(con, tablename="cp_sensors")
#colnames(sp@data)
spIds = unique(sp@data$sensor_annotation_id)
sp@data
plot(sp)
#sp = dbReadSpatial(con, tablename="cp_sensor_corr_view")
report_id = spIds[5]
#spplot(sp)
shp = dbReadSpatial(con, tablename="cp_sensor_corr_view", whereClause=paste0("trg_report_id = '",report_id,"'"))
View(shp@data)
shp.df   <- data.frame(id=rownames(shp@data),
                        #values=sample(1:10,length(shp),replace=T),
                        shp@data, stringsAsFactors=F)

data_fort   <- fortify(shp)
data_merged <- join(data_fort, shp.df, by="id")
colnames(data_merged)



#ggmap(get_map(unlist(geocode("Aarhus")),zoom=11))+
getplot <- function(varname){
  vn <<- varname # has to be 
  print(varname)
  g <-ggplot()+
      geom_path(data=data_merged,size=.1,
                aes(x=long,y=lat,group=group,color=get(vn)))+
      labs(x="",y="")+
      scale_colour_gradient(limits=c(min(data_merged[,vn]), max(data_merged[,vn])), low='green', high='red')+
      theme(axis.text=element_blank(),axis.ticks=element_blank())+
      ggtitle(vn)
  return(g)
}
#varnames=c('duration_src_p2_trg_p1', 'pearson_offset_random_mean', 'spearman_offset_random_dct_mean','kendall_random_median')
data_merged[,varnames] <- abs(data_merged[,varnames])
for (i in 1:length(varnames)){
  filen= paste0('plots/',report_id,'_',varnames[i],'.pdf')
  print(filen)
  g<-getplot(varnames[i])
  g
  ggsave(file=filen, g)  
}

summary(data_merged[,varnames])


