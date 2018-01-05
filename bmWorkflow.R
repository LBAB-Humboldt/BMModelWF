#bmWorkflow.R
# Prepares data for species distribution modeling in Maxent, and runs
# distribution models in parallel using Maxent.
# Arguments:
## Data input, cleaning and output (mandatory):
### occ.file(string):  Full path to species occurrence file.
###                    Occurrence file must be comma separated and must contain fields
###                    id, species, lat, lon (lowercase, order not important)
### env.dir(string):   Path to directory that contains environmental layers
### env.files(string): File names of environmental layers to be used in distribution models.
###                    Must contain extension, e.g. bio_1.tif.
### wd(string):        Path to directory where output files will be saved
### dist(numeric):     Distance (in meters) within which records are considered duplicates.
###                    Only one record is kept within dist.Default is 1000.
##
## Background generation options:
### bkg.aoi(string):   Keyword that defines where background will be sampled from.
###                      extent (default): background will be sampled from raster extent
###                      regions: background will be species specific, and it will correspond
###                               to the polygons of a shapefile that completely contain the
###                               species records.
###                      ch: background will be species specific and it will correspond to the
###                          convex hull of the species' records.
### bkg.type(string):  Keyword that defines how the background will be sampled from bkg.aoi.
###                    random (default): background will be sampled randomly from bkg.aoi
###                    samples: get background samples from a file.
### Optional arguments:
###   n.bkg(numeric):           number of background samples.
###                             Used when bkg.type="random". Default is 10000.
###   sample.bkg(string):       Path to comma separated file containing background samples.
###                             Must include the fields lat and lon (lowercase, order doesn't matter).
###                             Used only when bkg.type="samples"
###   regions(SpatialPolygons): SpatialPolygons object with the regions that will be used to
###                             define species background.
###                             Used only when bkg.aoi="regions"
###   field(string):            field (column name) that defines the regions.
###                             Used only when bkg.aoi="regions"
###   buffer(numeric):          Buffer in meters to be applied to convex polygons.
###                             Used only when bkg.aoi="ch".
## Evaluation and regularization options:
### optimize.lambda(logical):Optimize regularization value? Default FALSE.
### lambda(numeric):    Regularization multiplier. Deafault 1.
### do.eval(logical):   Do model evaluation? Default TRUE.
### folds(numeric):     Number of folds for k-fold partitioning used in evaluation
###                     and regularization optimization. Default = 10.
## Modeling options:
### mxnt.args(vector):  character vector containing arguments to pass to dismos'
###                     maxent function.
### n.cpu:               Number of cores to uses for parallel processing
## Post-processing options:
### do.threshold(logical): Threshold distribution models?
### raw.threshold(vector): numeric or character vector. If numeric, this will
###                        specify the percentiles (from 0 to 100) at which
###                        models should be thresholded according to the
###                        "probability of occurrence" at training sites.
###                        If character, this should be any combination of the
###                        following keywords: min, 10p, ess, mss.
### do.cut(logical):       Select distribution patches with evidence of occurrence
###                        from thresholded distribution models?
#
#
# Example:
#
# bmWorkflow(occ.file="D:/Projects/acuaticas/experiment/species_db.csv",
#            env.dir="C:/ws2",
#            env.files=c(paste0("bio_",1:19,".tif"),"slope_deg.tif","tri.tif","twi.tif"),
#            wd="~/tmp3",
#            dist=1000,
#            bkg.aoi = "extent",
#            bkg.type="random",
#            n.bkg = 10000,
#            sample.bkg = NULL,
#            optimize.lambda=TRUE,
#            folds=10,
#            do.eval=TRUE,
#            n.cpu=4,
#            mxnt.args=c("autofeature=FALSE","linear=TRUE","quadratic=TRUE","product=FALSE","hinge=TRUE","threshold=FALSE",
#                        "extrapolate=FALSE","doclamp=TRUE","addsamplestobackground=TRUE"),
#            do.threshold=TRUE,
#            raw.threshold=c(0,10,20,30),
#            do.cut=TRUE)


# wd<-"/home/rstudio/test/Modelos/11102016"
# env.dir="/home/rstudio/test/baseData/Capas"
# env.files<-paste0("bio_",c(1,2,3,4,12,15,18),".asc")
# occ.file<-occ.file$occs[which(external.use==1), ]
# sp.col <- "acceptedNameUsage"
# id.col <- "occurrenceID"
# dist <- sqrt(2)*1000
# n.bkg=10000
# bias.raster<-raster("plantae.asc")
# mxnt.args=c("autofeature=FALSE","extrapolate=FALSE","doclamp=TRUE","addsamplestobackground=TRUE")
# raw.threshold<-c(0,10,20,30)
# folds <-5
# do.threshold<-TRUE
# buffer<-150000
# proj.crs<-"+proj=utm +zone=18 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# n.cpu<-15
# as.is=T
bmWorkflow<-function(wd, env.dir, env.files, occ.file, sp.col, id.col, dist, n.bkg, bias.raster, 
               mxnt.args, do.treshold, raw.threshold, folds,buffer,proj.crs,ncpu,as.is,prefix){  
  #Create log file
  sink(paste0(wd,"/log.txt"))
  on.exit(sink())
  
  #Load Functions
  library(devtools)
  source_url("https://raw.githubusercontent.com/LBAB-Humboldt/BMModelWF/master/preModelingFunctions.R")
  source_url("https://raw.githubusercontent.com/LBAB-Humboldt/BMModelWF/master/evaluationFunctions.R")
  source_url("https://raw.githubusercontent.com/LBAB-Humboldt/BMModelWF/master/postModelingFunctions.R")
  source_url("https://raw.githubusercontent.com/LBAB-Humboldt/BMModelWF/master/miscFunctions.R")
  
  LoadLibraries()
  cat(paste(Sys.time(), "Functions and libraries loaded\n"))
  
  #Load and clean data
  env.vars <- raster:::stack(paste0(env.dir,"/",env.files))
  cat(paste(Sys.time(), "Loaded environmental layers", paste(as.character(env.files), collapse=","), "from directory", env.dir,"\n"))
  
  if(is.na(projection(env.vars))|projection(env.vars)=="NA"){
    cat(paste(Sys.time(), "WARNING: Undefined environmental variables projection\n"))
    cat(paste(Sys.time(), "WARNING: Setting projection to geographic\n"))
    projection(env.vars)<-"+proj=longlat +ellps=WGS84 +datum=WGS84"
  }
  
  occs <- LoadOccs(occ.file, env.vars, sp.col, id.col)
  orig.occs <- occs
  occ.summary<-ddply(occs, sp.col, function(df) return(c(LonLatFilter=nrow(df))))
  cat(paste(Sys.time(), "Loaded",nrow(occs), "records corresponding to",
            nrow(occ.summary), "species from file.\n"))
  
  ## Remove records within radius defined by variable "dist"
  occs <- ddply(occs, sp.col, IdNeighbors, dist=dist)
  tmp.summary <- ddply(occs, sp.col, function(df) return(c(RadiusFilter=nrow(df))))
  occ.summary$RadiusFilter <- 0
  occ.summary$RadiusFilter[match(tmp.summary[, sp.col], occ.summary[, sp.col])] <- tmp.summary$RadiusFilter
  cat(paste(Sys.time(), "After removing  points within",dist, "meters of each other, ",
            nrow(occs), "records remain\n"))
  
  #Extract covariate data for presences (and background if bkg.aoi="extent")
  occs.covs <- data.frame(extract(env.vars, cbind(occs$lon,occs$lat)))
  nna.rows <- which(apply(!is.na(occs.covs), 1, any))
  occs.covs <- occs.covs[nna.rows, ]
  occs <- occs[nna.rows, ]
  tmp.summary <- ddply(occs, sp.col, function(df) return(c(EnvNAFilter=nrow(df))))
  occ.summary$EnvNAFilter <- 0
  occ.summary$EnvNAFilter[match(tmp.summary[, sp.col], occ.summary[, sp.col])] <- tmp.summary$EnvNAFilter
  
  #Sample random background
  train.bkg <- GenerateBkg(n.bkg, env.vars)
  test.bkg <- GenerateBkg(n.bkg, env.vars)
  cat(paste(Sys.time(), "Background generated for raster extent using random sampling \n"))

  ## Define method depending on number of records
  occ.summary$modelType <- "none"
 # occ.summary$modelType[occ.summary$EnvNAFilter > 0 & occ.summary$EnvNAFilter < 5] <- "ch"
 # occ.summary$modelType[occ.summary$EnvNAFilter > 4 & occ.summary$EnvNAFilter <10] <- "bc"
  occ.summary$modelType[occ.summary$EnvNAFilter >= 10] <- "mx"
  
  #Write summary files
  write.csv(occs, paste0(wd, "/occurrences.csv"), row.names = FALSE)
  write.csv(occ.summary, paste0(wd, "/occurrence_summary.csv"), row.names = FALSE)
  cat(paste(Sys.time(), "Began parallel loop using", n.cpu, "cores \n"))
  sink()
  
  #Create model results template
  initial.template <- read.csv("modelTemplate.csv")
  
  #Begin modeling loop
  sfInit(parallel=T,cpus=n.cpu)#Initialize nodes
  sfExportAll() #Export vars to all the nodes
  sfClusterSetupRNG()
  sfClusterApplyLB(1:nrow(occ.summary),function(i){
    tmp.dir <- tempdir()
    sink(paste0(wd,"/log.txt"), append=TRUE)
    on.exit(sink())
    LoadLibraries()
    
    #Get species data
    sp.name <- occ.summary[i, sp.col]
    out.sp.name <- sub(" ","_",sp.name)
    print(sp.name)
    
    if(occ.summary$modelType[i] == "ch"){
      sp.idx <- which(orig.occs[, sp.col] == sp.name)
      sp.occs <- orig.occs[sp.idx, ]
      ch.eval <- EvalCH(sp.occs, env.vars ,buffer, proj.crs)
      ch.map <- ChMap(sp.occs, env.vars, buffer = buffer, proj.crs)
      omission <- 1 - sum(extract(ch.map, with(sp.occs,cbind(lon,lat))),na.rm=T) / nrow(sp.occs)
      write.csv(ch.eval, paste0(wd, "/",  out.sp.name, "_ch_evaluation.csv"),row.names=FALSE)
      writeRaster(ch.map, paste0(wd, "/",  out.sp.name, "_ch.tif"), format="GTiff", overwrite=TRUE, NAflag=-9999)
      
      results.template <- initial.template
      results.template[1, c("modelID","taxID","acceptedNameUsage", "modelingMethod", "validationType",
                            "perfStatType", "perfStatValue", "perfStatSD", "pValue",
                            "recsUsed","consensusMethod","thresholdValue", "omission",
                            "modelLevel", "modelStatus","tifPath", "dd", "mm", "yyyy")] <-
        c(paste0(prefix,"-",i),unique(sp.occs$taxID),sp.name, "ch", "jackniffe", "TSS.test", mean(ch.eval$tss, na.rm=T),
          sd(ch.eval$tss, na.rm=T), PoolPValues(ch.eval$p.value), nrow(sp.occs), "all",
          NA,omission, 1, "Developing",paste0(sub(" ","_",sp.name), "_ch.tif"),
          format(Sys.Date(),"%d"), format(Sys.Date(),"%m"), format(Sys.Date(),"%Y"))
      write.csv(results.template, paste0(wd, "/",  out.sp.name, "_modelResults.csv"), row.names=FALSE)
      WriteCSV(sp.occs, paste0(wd, "/",  out.sp.name,".csv"),as.is)
      cat(paste(Sys.time(), "Generated prediction of convex hull distribution model for", sp.name, "\n"))
    }
    
    if(occ.summary$modelType[i] == "bc"){
      sp.idx <- which(occs[, sp.col] == sp.name)
      sp.occs <- occs[sp.idx, ]
      bc.eval <- EvaluateBioclimModel(nrow(sp.occs), occs.covs[sp.idx, ], train.bkg, test.bkg)
      bc.obj <- bioclim(occs.covs[sp.idx, ])
      bc.map <- predict(env.vars, bc.obj)
      
      #Write Results
      writeRaster(bc.map, paste0(wd, "/",  out.sp.name, "_bc.tif"), format="GTiff", overwrite=TRUE, NAflag=-9999)
      results.template <- initial.template
      results.template[1, c("modelID","taxID","acceptedNameUsage", "modelingMethod", "thresholdType","validationType",
                            "perfStatType", "perfStatValue", "perfStatSD", "pValue",
                            "recsUsed", "consensusMethod","thresholdValue", "omission",
                            "modelLevel", "modelStatus","tifPath", "dd", "mm", "yyyy")] <-
        c(paste0(prefix,"-",i),unique(sp.occs$taxID),sp.name, "bc", "Continuous", "jackniffe", "AUC.test", mean(bc.eval$test.auc, na.rm=T),
          sd(bc.eval$test.auc, na.rm=T), PoolPValues(bc.eval$p.value), nrow(sp.occs),
          "all", NA, "NA",1, "Developing", paste0(sub(" ","_",sp.name), "_bc.tif"), format(Sys.Date(),"%d"),
          format(Sys.Date(),"%m"), format(Sys.Date(),"%Y"))
      write.csv(results.template, paste0(wd, "/",  out.sp.name, "_modelResults.csv"), row.names=FALSE)
      WriteCSV(sp.occs, paste0(wd, "/",  out.sp.name,".csv"),as.is)
      write.csv(bc.eval, paste0(wd, "/",  out.sp.name, "_bc_evaluation.csv"),row.names=FALSE)
      save(bc.obj, file=paste0(wd, "/", out.sp.name, ".RData"))
      cat(paste(Sys.time(), "Generated Bioclim distribution model for", sp.name, "\n"))
      
      #Post-processing: threshold & cut
      if(do.threshold){
        thres.maps <- sapply(raw.threshold, FUN=ThresholdBRT, brt.obj=bc.obj, sp.covs=occs.covs[sp.idx, ],
                             map=bc.map)
        for(j in 1:length(raw.threshold)){
          bc.eval.t <- EvaluateBioclimModel(nrow(sp.occs), occs.covs[sp.idx, ], train.bkg, test.bkg,raw.threshold[j])
          write.csv(bc.eval.t, paste0(wd, "/",  out.sp.name, "_", raw.threshold[j], "_bc_evaluation.csv"), row.names=FALSE)
          writeRaster(thres.maps[[j]],filename=paste0(wd, "/", out.sp.name,"_", raw.threshold[j], "_bc.tif"),
                      format="GTiff",overwrite=TRUE, NAflag=-9999)
          results.template <- initial.template
          thresholds <- quantile(predict(bc.obj, occs.covs[sp.idx, ]),raw.threshold[j] / 100)
          results.template[1, c("modelID","taxID","acceptedNameUsage", "modelingMethod","thresholdType", "validationType",
                                "perfStatType", "perfStatValue", "perfStatSD", "pValue",
                                "recsUsed", "consensusMethod","thresholdValue", "omission",
                                "modelLevel", "modelStatus","tifPath", "dd", "mm", "yyyy")] <-
            c(paste0(prefix,"-",i),unique(sp.occs$taxID),sp.name, "bc", raw.threshold[j],"jackniffe", "TSS.test", mean(bc.eval.t$tss),
              sd(bc.eval.t$tss), PoolPValues(bc.eval.t$p.value), nrow(sp.occs),
              "all", thresholds[j], (1-mean(bc.eval.t$sens)), 1, "Developing", paste0(sub(" ","_",sp.name), "_", raw.threshold[j], "_bc.tif"),
              format(Sys.Date(),"%d"), format(Sys.Date(),"%m"), format(Sys.Date(),"%Y"))
          write.csv(results.template, paste0(wd, "/",  out.sp.name, "_", raw.threshold[j], "_modelResults.csv"), row.names=FALSE)
        }
        cat(paste(Sys.time(), "Generated thresholded prediction of Bioclim distribution model
                  using thresholds ", paste(raw.threshold,collapse=", "), "for", sp.name, "\n"))
      }
      }
    
    if(occ.summary$modelType[i] == "mx"){
      sp.idx <- which(occs[, sp.col] == sp.name)
      sp.occs <- occs[sp.idx, ]
      if(is.null(bias.raster)){
        bkg.coords <- randomPoints(env.vars, n.bkg, prob=F)
      } else {
        bkg.coords <- randomPoints(bias.raster, n.bkg, prob=T)
      }
      bkg.covs <- extract(env.vars, bkg.coords)
      enmeval.obj <- ENMevaluate(with(sp.occs,cbind(lon,lat)), env.vars, bkg.coords, RMvalues = seq(0.5, 4, 0.5),
                            fc = c("L", "LQ", "LQH"), method = "checkerboard1",
                            parallel=F, rasterPreds = FALSE)
      mxnt.args <- c(CreateMXArgs(enmeval.obj), mxnt.args)
      
      mx.obj <- maxent(x=rbind(occs.covs[sp.idx, ], bkg.covs),
                         p=c(rep(1,length(sp.idx)),rep(0,nrow(bkg.covs))),
                         removeDuplicates=FALSE, args=mxnt.args, path=tmp.dir)
      mx.map <- predict(env.vars, mx.obj)
    
      #Write Results
      writeRaster(mx.map, paste0(wd, "/",  out.sp.name, "_mx.tif"), format="GTiff", overwrite=TRUE, NAflag=-9999)
      best.ind<-which.max(enmeval.obj@results$Mean.AUC)
      results.template <- initial.template
      results.template[1, c("modelID","taxID","acceptedNameUsage", "modelingMethod","thresholdType" ,"validationType",
                            "perfStatType", "perfStatValue", "perfStatSD", "pValue",
                            "recsUsed", "consensusMethod","thresholdValue", "omission",
                            "modelLevel", "modelStatus","tifPath", "dd", "mm", "yyyy")] <-
        c(paste0(prefix,"-",i),unique(sp.occs$taxID),sp.name, "mx","Continuous", "checkerboard1", "AUC.test", enmeval.obj@results$Mean.AUC[best.ind],
          sqrt(enmeval.obj@results$Var.AUC[best.ind]), NA, nrow(sp.occs),
          "all", NA, "NA",1, "Developing", paste0(sub(" ","_",sp.name), "_mx.tif"), 
          format(Sys.Date(),"%d"), format(Sys.Date(),"%m"), format(Sys.Date(),"%Y"))
      write.csv(results.template, paste0(wd, "/",  out.sp.name, "_modelResults.csv"), row.names=FALSE)
      WriteCSV(sp.occs, paste0(wd, "/",  out.sp.name,".csv"),as.is)
      save(enmeval.obj, file=paste0(wd, "/", out.sp.name, "_enmeval.RData"))
      save(mx.obj, file=paste0(wd, "/", out.sp.name, ".RData"))
      cat(paste(Sys.time(), "Generated Maxent distribution model for", sp.name, "\n"))
      
      if(do.threshold){
        thres.maps <- sapply(raw.threshold, FUN=Threshold2, mxnt.obj=mx.obj,
                             map=mx.map)
        for(j in 1:length(raw.threshold)){
          mx.eval.t<-ThresholdMXEval(folds, occs.covs[sp.idx, ], bkg.covs, test.bkg, mxnt.args,raw.threshold[j])
          write.csv(mx.eval.t, paste0(wd, "/",  out.sp.name, "_", raw.threshold[j], "_mx_evaluation.csv"), row.names=FALSE)
          writeRaster(thres.maps[[j]],filename=paste0(wd, "/", out.sp.name,"_", raw.threshold[j], "_mx.tif"),
                      format="GTiff",overwrite=TRUE, NAflag=-9999)
          results.template <- initial.template
          thresholds <- quantile(predict(mx.obj, occs.covs[sp.idx, ]),raw.threshold[j] / 100)
          results.template[1, c("modelID","taxID","acceptedNameUsage", "modelingMethod","thresholdType", "validationType",
                                "perfStatType", "perfStatValue", "perfStatSD", "pValue",
                                "recsUsed", "consensusMethod","thresholdValue", "omission",
                                "modelLevel", "modelStatus","tifPath", "dd", "mm", "yyyy")] <-
            c(paste0(prefix,"-",i),unique(sp.occs$taxID),sp.name, "mx", raw.threshold[j],"k-fold", "TSS.test", mean(mx.eval.t$tss),
              sd(mx.eval.t$tss), PoolPValues(mx.eval.t$p.value), nrow(sp.occs),
              "all", thresholds[j], (1-mean(mx.eval.t$sens)), 1, "Developing", paste0(sub(" ","_",sp.name),"_" ,raw.threshold[j], "_mx.tif"),
              format(Sys.Date(),"%d"), format(Sys.Date(),"%m"), format(Sys.Date(),"%Y"))
          write.csv(results.template, paste0(wd, "/",  out.sp.name, "_", raw.threshold[j], "_modelResults.csv"), row.names=FALSE)
        }
        cat(paste(Sys.time(), "Generated thresholded prediction of maxent distribution model
                  using thresholds ", paste(raw.threshold,collapse=", "), "for", sp.name, "\n"))
        }
      }
    
    #Remove temporary files
    removeTmpFiles(2)
    })
  sfStop()
  occ.summary$TIFs <- sapply(paste0(sub(" ","_",occ.summary[,sp.col])),CountTIF, wd=wd)
  results.summary<-ResultsTable(wd)
  write.csv(occ.summary, paste0(wd, "/occurrence_summary.csv"), row.names = FALSE)
  write.csv(results.summary, paste0(wd, "/results_summary.csv"), row.names=FALSE)
}