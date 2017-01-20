library("RSQLite");
dbfile = "Z:/BioModelos/DB_current/02112016/production.sqlite3" # Assign the sqlite datbase and full path to a variable
sqlite = dbDriver("SQLite") # Instantiate the dbDriver to a convenient object
mydb = dbConnect(sqlite, dbfile) # Assign the connection string to a connection object

spList<-read.csv("D:/Projects/Primates2016/primateList.csv",as.is=T)

getScoresByUser<-function(con, date, species, user.id){
  score.q<-paste0("SELECT s.sci_name AS Especie, m.description AS Umbral, m.level AS Nivel, u.name AS Experto, r.score AS Calificación, r.updated_at AS Fecha
  FROM species AS s
  JOIN models AS m ON s.id = m.species_id
  JOIN ratings AS r ON r.model_id = m.id
  JOIN users AS u ON u.id = r.user_id
  WHERE r.created_at > \'",date,"\' AND r.user_id = ",user.id," AND s.sci_name = \'",species,
  "\' ORDER BY s.sci_name")
  
  q.result <-dbSendQuery(con, score.q)
  edits <- fetch(q.result)
  dbClearResult(q.result)
  return(edits)
}

###Edition table
ed.table<-data.frame(species=NA,user=NA,edit_id=NA,model_id=NA,action=NA,score=NA)

for(i in 4:nrow(spList)){
  date<-"2016-11-01"
  raster.dir<-"D:/Projects/Primates2016/TIFs"
  con<-mydb
  dir.name<-sub(" ","_",spList[i,1])
  dir.create(paste0(out.folder,"/",dir.name))
  
  polys<-getBMPoly(con,spList[i,1],date,paste0(out.folder,"/",dir.name))
  if(!is.null(polys)){
    df<-data.frame(Species=polys$edits$Especie,
                   UserID=polys$edits$UserID,
                   UserName=polys$edits$Experto,
                   EditID=polys$edits$EditID,
                   Model=as.character(sapply(polys$shps,function(in.shp) in.shp@data$Modelo)),
                   Action=as.character(sapply(polys$shps,function(in.shp) in.shp@data$Accion)),
                   Score=NA)
    
    for(j in 1:nrow(df)){
      result<-getScoresByUser(con,date,spList[i,1],df$UserID[j])$`Calificación`
      if(length(result>0)){
        df$Score[j]<-result
      }
    }
    df$Score<-as.numeric(df$Score)
    write.csv(df,paste0(out.folder,"/",dir.name,"/",dir.name,".csv"),row.names=FALSE)
  }
}

ResultsTable<-function(wd){
  csvs<-list.files(wd, "*.csv", full.names=T)
  results<-data.frame()
  for(i in 1:length(csvs)){
    results<-rbind(results, read.csv(csvs[i]))
  }
  return(results)
}

u.list<-unique(df$UserID)
for(j in 1:length(u.list)){
  df.sub<-subset(df,UserID==u.list[j])
  for(k in 1:nrow(df.sub)){
    in.raster<-raster(paste0(raster.dir,"/",df.sub$Model[k],".tif"))
    shp.raster<-rasterize(polys$shps[[which(df$EditID==df.sub$EditID[k])]],
                          in.raster, background=0)
    if(df.sub$Score[k]>2){
#      in.shp<-readOGR(paste0(out.folder,"/",dir.name),paste0(dir.name,"-",df.sub$UserID[k],"-",df$EditID[k]))

      if(df.sub$Action[k]=="Add"){
        out.raster<-in.raster+shp.raster
        out.raster[Which(out.raster>1)]<-1
      }
      if(df.sub$Action[k]=="Intersect"){
        out.raster<-in.raster*shp.raster
      }
      if(df.sub$Action[k]=="Cut"){
        out.raster<-in.raster-shp.raster
        out.raster[Which(out.raster<0)]<-0
      }
    } else {
      shp.raster[is.na(in.raster)]<-NA
      out.raster<-shp.raster
    }
    out.raster.name<-paste0(out.folder,"/",dir.name,"/",dir.name,"-",df.sub$UserID[k],"-",df.sub$EditID[k],".tif")
    writeRaster(out.raster, out.raster.name,overwrite=T)
  }
}



out.folder="D:/Projects/Primates2016/Polygons"
date<-"2016-11-01"

polys<-getBMPoly(mydb,spList[i,1],date,paste0(out.folder,"/",dir.name))
for(j in 1:length(u.list)){
  scores<-getScoresByUser(mydb,date,spList[i,1],u.list[j])
  ed.by.user<-which(polys$edits$UserID==u.list[j])
  if(scores$Calificación > 2){
    sp.raster<-raster(paste0(raster.dir,"/",polys$shps[[i]]@data$Modelo,".tif"))
    
  }

