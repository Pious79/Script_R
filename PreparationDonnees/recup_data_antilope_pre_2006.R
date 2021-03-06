##____________________________________________________________________________##
##  Script to extract Antilope rain pre 2006                                  ##
##  Pierre L'HERMITE - 20171107 - Recup_data_Antilope_pre2006.R               ##
##____________________________________________________________________________##

#__Root_Directory___________________________________________________________####
# ---- si travail sur le serveur
User <- "Pierre"
# ---- si travail en local
Disc <- "H"

if(substr(version$os, 1, 5) == "linux") {
  DIR_ROOT <- paste0("/home/RHAX21/UTILISATEURS/", User, "/Workspace_R/")
} else {
  if(Disc == "H") {
    DIR_ROOT <- paste0(Disc, ":/Workspace_R/")
  } else {
    DIR_ROOT <- paste0(Disc, ":/Users/pierre.lhermite/Documents/Pierre/Workspace_R/")
  }
}
setwd(DIR_ROOT)
getwd()
# ---- suppression des objets contenus en memoire
rm(list = ls())
gc()

##__Directories_____________________________________________________________####
DIR_DATA_INPUT          <- "DataInput/"
DIR_DATA_INPUT_RDATA    <- paste0(DIR_DATA_INPUT, "bvRdata/")
DIR_DATA_OUTPUT         <- "DataOutput/"
DIR_FUNCTIONS           <- "Functions_R/"
DIR_GRAPHE              <- paste0(DIR_DATA_OUTPUT, "Graphes/")
DIR_FUNCTIONS_LOIEAU    <- "H:/Workspace_R/Fonctions_LOIEAU"

##__Functions_______________________________________________________________####
for(FileName in list.files(DIR_FUNCTIONS, pattern = "\\.[Rr]$")) {
  source(file.path(DIR_FUNCTIONS, FileName))
}
for(FileName in list.files(DIR_FUNCTIONS_LOIEAU, pattern = "\\.[Rr]$")) {
  source(file.path(DIR_FUNCTIONS_LOIEAU, FileName))
}
rm(FileName); gc()

##__Packages________________________________________________________________####
library(raster)
library(sp)
library(rgdal)

##__contenu_script__________________________________________________________####
DATA_FILE <- "Z:/DONNEES/PLUIE/SPATIAL/REANA/REANA_COR/24H/"

jour <- list.files(DATA_FILE, pattern = ".Rdata")

#Coordonnees pluviometre
FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoPluvio/infoPluvio_Real.RData")
load(FileInfobv); rm(FileInfobv); gc()

Pluie_pluvio <- as.data.frame(matrix(NA, ncol = length(infoPluvio$code),
                                     nrow = length(jour)))
rownames(Pluie_pluvio) <- seq.Date(as.Date("1997-01-01"), as.Date("2006-12-30"), "days")
colnames(Pluie_pluvio) <- infoPluvio$nom
prjs.L2E <- CRS("+init=epsg:27572")

for(iday in 1:length(jour)){
  load(paste0(DATA_FILE, jour[iday]))
  #Passage au nouveau raster du fichier antilope
  pRaster2 <- fc.raster_to_raster_2(Rdata)
  projection(pRaster2) <- prjs.L2E
  
  #Definir la zone d'etude et decouper
  Zone <- extent(bbox(infoPluvio$coord))
  Raster_cut <- crop(pRaster2, Zone, snap="out")
  
  #extraction pluie fichier raster
  Pluie_raster <- raster::extract(Raster_cut, infoPluvio$coord)
  
  Pluie_pluvio[iday, ] <- Pluie_raster
}
save(Pluie_pluvio, file = paste0(DIR_DATA_OUTPUT,"Antilope_pre2006.RData"))
