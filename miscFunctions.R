ChMap<-function(occs, aoi, buffer, proj.crs){
  in.pts <- SpatialPoints(cbind(occs$lon, occs$lat), proj4string = CRS(projection(aoi)))
  cells <- unique(cellFromXY(aoi,in.pts))
  if(length(cells) > 2){ 
    ch.shp <- convHull(in.pts)@polygons
  } else {
    ch.shp <- in.pts
  }
  
  if(!is.null(buffer)){
    if(buffer>0){
      ch.shp.proj <- spTransform(ch.shp, proj.crs)
      ch.shp.proj <- buffer(ch.shp.proj, width=buffer)
      ch.shp <- spTransform(ch.shp.proj, crs(aoi))
    }
  }
  bkg <- rasterize(ch.shp, aoi, field=1, background = 0)
  bkg[Which(is.na(aoi[[1]]))] <- NA
  return(bkg)
}

PoolPValues <- function(p.values){
  p.values <- na.omit(p.values)
  if(length(p.values) > 1){
    result <- 1 - pchisq(-2*sum(log(p.values)), 2*length(p.values)) #pp795 from Sokal & Rohlf (1995) after Fisher (1954)
  }
  if(length(p.values)==1){
    result <- p.values
  }
  if(length(p.values)==0){
    result <- NA
  }
  return(result)
}

WriteCSV <- function(occs, out.file, as.is){
  if(!as.is){
    out.csv <- with(occs,cbind(occurrenceID, acceptedNameUsage, speciesOriginal, lon, lat,
                    locality, adm2, adm1, country, alt, earliestDateCollected,
                    institution, collector, basisOfRecord))
    out.csv <- as.data.frame(out.csv)
    colnames(out.csv) <- c("id", "species", "EspecieOriginal", "lon", "lat", "Localidad",
                           "Municipio", "Departamento", "Pais", "Altitud", "Fecha",
                           "Institucion", "Colector","Evidencia")
    out.csv$Publico <- rep("Si",nrow(occs))
    write.csv(out.csv, out.file, row.names=FALSE)
  } else {
    write.csv(occs,out.file, row.names=FALSE)
  }
}

CreateMXArgs<-function(enmeval.obj){
  mxnt.args<- c("linear=TRUE")
  best.ind<- which.min(enmeval.obj@results$delta.AICc)
  features <- enmeval.obj@results$features[best.ind]
  betamultiplier <- enmeval.obj@results$rm[best.ind]
  if(grepl("Q", features)){
    mxnt.args <- c(mxnt.args, "quadratic=TRUE")
  } else {
    mxnt.args <- c(mxnt.args, "quadratic=FALSE")
  }
  if(grepl("H", features)){
    mxnt.args <- c(mxnt.args, "hinge=TRUE")
  } else {
    mxnt.args <- c(mxnt.args, "hinge=FALSE")
  }
  if(grepl("P", features)){
    mxnt.args <- c(mxnt.args, "product=TRUE")
  } else {
    mxnt.args <- c(mxnt.args, "product=FALSE")
  }
  if(grepl("T", features)){
    mxnt.args <- c(mxnt.args, "threshold=TRUE")
  } else {
    mxnt.args <- c(mxnt.args, "threshold=FALSE")
  }
  mxnt.args <- c(mxnt.args, paste0("betamultiplier=",betamultiplier))
  return(mxnt.args)
}

CountTIF<-function(prefix, wd){
  files <-list.files(wd,prefix)
  result <- sum(grepl(".tif$", files))
  return(result)
}

ResultsTable<-function(wd){
  csvs<-list.files(wd, "*_modelResults.csv", full.names=T)
  results<-data.frame()
  for(i in 1:length(csvs)){
    results<-rbind(results, read.csv(csvs[i]))
  }
  return(results)
}