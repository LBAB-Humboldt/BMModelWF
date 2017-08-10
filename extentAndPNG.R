#extentAndPNG
#Este script asigna el extent y sistema de referencia de BioModelos a los modelos 
#publicados por investigadores externos al LBA. Adicionalmente, utiliza la funcion
#"convert2PNG" para pasar los modelos al formato KMZ y PNG para poder publicarlos a 
#traves de BioModelos.

#Los objetos del script son:
#ref.map: es el raster de referencia con el extent para publicar BioModelos
#archivos: listado de archivos geográficos raster (.asc, .tif, etc.) ubicados en el
#archivos_name: listado de los nombres de los archivos a ser procesados luego de remover
#la extensión (.asc, por ejemplo)
#outputfile: directorio de salida de los archivos que resulten (.tif, capetas "PNG", "KMZ" y "thumbs")

#El resultado son los archivos .tif de cada uno de los mapas originales publicados a través
#de BioModelos junto a las carpetas "PNG", "KMZ" y "thumbs, que a su vez continen los archivos
#de cada modelo publicado en el formato necesario para ser publicados a través del visor de
#BioModelos.

#Autor: Helena Olaya
#Fecha de creación: 11 de noviembre de 2016

library(rgdal)
library(raster)
library(maptools)
library(repmis)

ref.map <- raster("//192.168.11.113/Lab_biogeografia2/BioModelos/Modelos_publicados/referencia/Coeligena_helianthea_0_mx.tif")

#Definir el directorio de trabajo donde se encuentran los modelos que serán publicados
#en formato .tif o .asc
setwd("//192.168.11.113/Lab_biogeografia2/...")

archivos <- list.files(pattern = "\\.asc")
archivos_name <- gsub('.asc','', archivos)
outputfile <- "//192.168.11.113/Lab_biogeografia2/..."

#llamar la función "convert2PNG" y el archivo "params.RData"
source("https://raw.githubusercontent.com/LBAB-Humboldt/BMModelWF/master/convert2PNG.R")
source_data("https://github.com/LBAB-Humboldt/BMModelWF/blob/master/params.RData?raw=true")
##Para llamar la función de forma local (en el PC de Helena)
#ruta_funcion=("C:/Users/ertb/Documents/Helena Olaya/BioModelos/Mapas publicados/convert2PNG/Jorge")
#setwd(ruta_funcion)
#source("convert2PNG.R") 
#load("params.RData")


#Transformar los archivos originales de los modelos publicados a .tif con el extent y 
#sistema de referencia de BioModelos.

for (i in 1:length(archivos)){
#leer cada modelo y darle el sistema de referencia WGS84
  map <- raster(archivos[i], crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  #map <- projectRaster(map,ref.map) #usar si el mapa viene en otra proyeccion
#dar el extent de BioModelos a los modelos publicados
  map2<-extend(map,ref.map)
  extent(map2)<-extent(ref.map)
#exportar los .tif de los modelos con el sistema de referencia y extent de BioModelos
  writeRaster(map2,paste0(outputfile,"/",archivos_name[i]),format="GTiff",overwrite=T)
 }

#parametros de la funcionn "convert2PNG" (ver archivo de la funcion para leer 
#definiciones de los parametros)

#Color naranja para modelos que no son ni continuo sni nivel 2
col.pal=rgb(193,140,40,maxColorValue=255)

#Medidas de los thumbnails (en pixeles) para las dos versiones de BioModelos
#145 205 Versión 1 
#179, 220 Versión 2

w=145
h=205

in.folder <- outputfile
setwd(in.folder)
sp.raster <- list.files(pattern = "*.tif$") #"*.tif$" solamente llama los archivos .tif (sin mas caracteres luego del .tif)
name = gsub('.tif','', sp.raster)

#Aplicar la función convert2PNG a cada .tif.

for (i in 1:length(sp.raster)){
  convert2PNG(sp.raster[i], name[i], in.folder, col.pal, FALSE, params,w,h) #Colocar FALSE si no hya background (cuando solo hay valores de 1 en el raster y NA)
}

