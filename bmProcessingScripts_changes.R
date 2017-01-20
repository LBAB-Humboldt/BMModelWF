#Update CSVs (Agregate reports from all users)

#getPtError.R
#Gets the error reports from BioModelos DB
#species: the species to query. 
#date: the date beyond which error reports are considered in YYYY-MM-DD format.
#con: an SQLiteConnection object

getPtError<-function(con, species, date){
  pt.errors.q <- paste0("SELECT s.sci_name AS Especies, pc.recordId AS 'ID Registro', pc.isOutlier AS 'Outlier Geográfico', pc.geoIssue AS 'Georreferenciación dudosa', pc.idIssue AS 'Identificación dudosa', pc.oldTaxonomy AS 'Nombre desactualizado', pc.inCaptivity AS 'Individuo introducido', pc.otherIssues AS 'Otros', pc.comment AS Comentarios, u.name AS 'Reportado por', pc.created_at AS 'Reportado' 
  FROM point_comments AS pc
  JOIN users AS u ON pc.user_id = u.id
  JOIN species AS s ON pc.species_id = s.id
  WHERE pc.created_at > \'", date, "\' AND s.sci_name = \'", species,
  "\' ORDER BY s.sci_name")
  q.result <-dbSendQuery(con, pt.errors.q) 
  pt.errors <- fetch(q.result)
  dbClearResult(q.result)
  if(nrow(pt.errors)>0){
    return(pt.errors)
  } else {
    return(NULL)
  }
}

#bmJSON2table
#Cast a JSON string from a BioModelos edition into a data frame (single row)
#Returns NULL value for geometries other than Point.

bmJSON2table<-function(json){
  interp.json<-tryCatch(fromJSON(json),
                        error=function(err){
                          print(paste("MY_ERROR:  ",err))
                          return(NULL)
                        })
  if(is.null(interp.json)){
    return(NULL)
  }
  if(interp.json$features$geometry$type=="Point"){
    df<-cbind(geometry=interp.json$features$geometry$type,
          lon=unlist(interp.json$features$geometry$coordinates)[1],
          lat=unlist(interp.json$features$geometry$coordinates)[2],
          interp.json$features$properties)
    return(df)
  } else {
    return(NULL)
  }
}

#Get added records
getNewRecs<-function(con,species, date){  
  edits.q <- paste0("SELECT r.id AS 'Edición ID', s.sci_name AS Especie, m.description AS Umbral, m.level AS Nivel, r.GeoJSON AS 'Tipo de Edición', u.name AS Experto, r.created_at AS Fecha
    FROM species AS s
    JOIN models AS m ON s.id = m.species_id
    JOIN reviews AS r ON r.model_id = m.id
    JOIN users AS u ON u.id = r.user_id
    WHERE r.created_at > \'", date, "\' AND s.sci_name = \'", species,
    "\' ORDER BY s.sci_name")
  q.result <-dbSendQuery(con, edits.q)
  edits <- fetch(q.result)
  dbClearResult(q.result)
  df<-data.frame()
  if(nrow(edits)>0){
    for(i in 1:nrow(edits)){
      result<-bmJSON2table(edits$`Tipo de Edición`[i])
      if(!is.null(result)){
        df <- rbind(df,result)
      } 
    }
  } 
  if(nrow(df)>0){
    return(df)
  } else {
    return(NULL)
  }
}

Cast2BMcsv<-function(df){
  result<-data.frame(id=rep("BioModelos user",nrow(df)),
             species=df$Especie,
             EspecieOriginal=rep(NA,nrow(df)),
             lon=df$lon,
             lat=df$lat,
             Localidad=df$Localidad,
             Municipio=rep(NA,nrow(df)),
             Departamento=rep(NA,nrow(df)),
             Pais=rep(NA,nrow(df)),
             Altitud=rep(NA,nrow(df)),
             Fecha=df$`Fecha de Registro`,
             Institucion=rep(NA,nrow(df)),
             Colector=df$Observador,
             Evidencia=df$Cita,
             Publico=rep("Si",nrow(df)))
  return(result)
}


UpdateCSV<-function(con, sp.name, date, file.name, in.folder, out.folder){
  in.file <- paste0(in.folder, "/", file.name)
  if(file.exists(in.file)){
    df0<-read.csv(in.file)
    #Get error reports and added records
    df1<-getPtError(con,sp.name, date)
    df2<-getNewRecs(con,sp.name, date)
    if(is.null(df1)&is.null(df2)){
      print(paste("Nothing to update for",sp.name))
      return(NULL)
    }
    #Remove records with errors
    if(!is.null(df1)){
      results <- df0[!(df0$id%in%df1$`ID Registro`), ]
    } else {
      results <- df0
    }
    
    #Add records
    if(!is.null(df2)){
      results<-rbind(results,Cast2BMcsv(df2))
    }
    write.csv(results, paste0(out.folder,"/",file.name),row.names=FALSE)
    return(results)
  } else {
    print(paste("File",file.name,"does not exist"))
    return(NULL)
  }
}

###Update CSV for species in a list
# Load the SQLite library
library("RSQLite");
dbfile = "Z:/BioModelos/DB_current/production.sqlite3" # Assign the sqlite datbase and full path to a variable
sqlite = dbDriver("SQLite") # Instantiate the dbDriver to a convenient object
mydb = dbConnect(sqlite, dbfile) # Assign the connection string to a connection object
#dbListTables(mydb) # Request a list of tables using the connection object

sp.list<-read.csv("D:/Projects/TallerBST/verifList.csv",as.is=T)[,1]
file.names<-paste0(sub(" ","_",sp.list),".csv")

occs<-data.frame()
for(i in 1:length(sp.list)){
  df.row<-UpdateCSV(mydb, sp.list[i], "2016-10-12", file.names[i],
            "C:/Workspace/prueba/CSV", "C:/Workspace/prueba/CSV2")
  if(!is.null(df.row)){
    occs<-rbind(occs,df.row)
  }
}

hasErrors<-rep(0,length(sp.list))
hasNewRecs<-rep(0,length(sp.list))

for (i in 1:length(sp.list)){
  df1<-getPtError(mydb,sp.list[i], '2016-10-12')
  df2<-getNewRecs(mydb,sp.list[i], '2016-10-12')
  if(!is.null(df1)) hasErrors[i] <- nrow(df1)
  if(!is.null(df2)) hasNewRecs[i] <- nrow(df2)
}
View(cbind(sp.list,hasErrors,hasNewRecs))

