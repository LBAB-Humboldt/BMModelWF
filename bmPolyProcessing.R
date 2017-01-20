##bmPolygonProcessing
library("RSQLite");
dbfile = "Z:/BioModelos/DB_current/production.sqlite3" # Assign the sqlite datbase and full path to a variable
sqlite = dbDriver("SQLite") # Instantiate the dbDriver to a convenient object
mydb = dbConnect(sqlite, dbfile) # Assign the connection string to a connection object

getBMPoly<-function(con, species, date, out.folder){  
  edits.q <- paste0("SELECT r.user_id AS 'UserID', r.id AS 'EditID', s.sci_name AS Especie, m.description AS Umbral, m.level AS Nivel, r.GeoJSON AS 'Tipo de Edición', u.name AS Experto, r.created_at AS Fecha
                    FROM species AS s
                    JOIN models AS m ON s.id = m.species_id
                    JOIN reviews AS r ON r.model_id = m.id
                    JOIN users AS u ON u.id = r.user_id
                    WHERE r.created_at > \'", date, "\' AND s.sci_name = \'", species,
                    "\' ORDER BY s.sci_name")
  q.result <-dbSendQuery(con, edits.q)
  edits <- fetch(q.result)
  dbClearResult(q.result)
  shps<-c()
  if(nrow(edits)>0){
    for(i in 1:nrow(edits)){
      print(nrow(edits))
      write(edits$`Tipo de Edición`[i],paste0(sub(" ","_",species),".json"))
      out.shp<-tryCatch(geojson2shp(paste0(sub(" ","_",species),".json")),
                        error=function(err){
                          print(paste("MY_ERROR:  ",err))
                          return(NULL)
                        })
      if(class(out.shp)=="SpatialPolygonsDataFrame"){
        shps<-append(shps,out.shp)
        layer<-paste0(sub(" ","_",species),"-", edits$UserID[i],"-",edits$EditID[i])
        writeOGR(out.shp,out.folder,layer, driver="ESRI Shapefile",overwrite_layer=TRUE)
      }
    }
  }
  if(length(shps)>0){
    return(list(edits=edits,shps=shps))
  } else {
    return(NULL)
  }
}

geojson2shp<-function(infile){
  shp<-readOGR(infile,"OGRGeoJSON")
  #layer<-strsplit(infile,"\\.")[[1]][1]
  #writeOGR(shp,getwd(),out.name, driver="ESRI Shapefile",overwrite_layer=TRUE)
  return(shp)
}

sp.list<-read.csv("D:/Projects/TallerBST/verifList.csv",as.is=T)[,1]
howManyPoly<-rep(0,length(sp.list))
for(i in 1:length(sp.list)){
  result<-getBMPoly(mydb,sp.list[i],"2016-10-01")
  if(!is.null(result)){
    howManyPoly[i]<-length(result)
  }
}

con<-mydb
species<-sp.list[4]
date <- "2000-01-01"
