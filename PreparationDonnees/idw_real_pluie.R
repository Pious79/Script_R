##____________________________________________________________________________##
##  Script to extract Antilope rain post 2006                                 ##
##  Pierre L'HERMITE - 20171107 - Recup_data_Antilope_post2006.R              ##
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
library(gstat)
library(sp)

##__Definition_des_projections______________________________________________####
prjs.L2E <- CRS("+init=epsg:27572")

##__Lecture_Shapes_HYDRO____________________________________________________####
DsnReal <- paste0(DIR_DATA_INPUT, "contour")
LayerReal <- "BvsReal_limni"
## Chargement des shapes (en L2E)
Bvs_Real <- readOGR(dsn = DsnReal, layer = LayerReal, stringsAsFactors = FALSE)
Bv_data <- slot(Bvs_Real, "data")
rm(DsnReal, LayerReal); gc()

##__InfoPluvio__________________________________________________________####
load(paste0(DIR_DATA_INPUT_RDATA, "/InfoPluvio/infoPluvio_Real.RData"))

##__Creation_Tableau_Pluie__________________________________________________####
iPluvio <- 1
load(paste0(DIR_DATA_INPUT_RDATA, "/PluvioData_Real/PluvioData_",
            infoPluvio$code[iPluvio],".RData"))

#Tableau des pluies en ligne
TabPluie <- as.data.frame(matrix(NA, ncol = length(PluvioData$TabDatesR),
                                 nrow = length(infoPluvio$code)))
colnames(TabPluie) <- PluvioData$TabDatesR
row.names(TabPluie) <- infoPluvio$nom
TabPluie[iPluvio, ] <- PluvioData$TabObsP

#Remplissage du tableau TabPluie
for (iPluvio in 1:length(infoPluvio$nom)){
  load(paste0(DIR_DATA_INPUT_RDATA, "/PluvioData_Real/PluvioData_",
              infoPluvio$code[iPluvio],".RData"))
  TabPluie[iPluvio, ] <- PluvioData$TabObsP
}

##__Boucle_idw_jour_Pour_combler_pluvio_____________________________________####
#Tableau avec les pluies des pluvios à interpoler
TabIDW <- as.data.frame(matrix(NA, ncol = 3,
                               nrow = length(infoPluvio$code)))
row.names(TabIDW) <- infoPluvio$code
colnames(TabIDW) <- c("x", "y", "Pluie_day")

#Remplissage du spatial point data frame IDW avec les coordonnees et les pluies
TabIDW[, 1:2] <- coordinates(infoPluvio$coord)
coordinates(TabIDW) <- ~x+y
proj4string(TabIDW) <- proj4string(infoPluvio$coord)

for(iDay in 1:length(TabPluie[1,])){
  if(length(which(!is.na(TabPluie[, iDay]))) == 15){
    
  } else if(length(which(!is.na(TabPluie[, iDay]))) < 3){
    
  } else if(length(which(is.na(TabPluie[, iDay]))) == 15){
    
  } else {
    TabIDW$Pluie_day <- TabPluie[, iDay]
    idwtest <- gstat::idw(Pluie_day ~ 1, TabIDW[!is.na(TabIDW$Pluie_day), ],
                          TabIDW[is.na(TabIDW$Pluie_day), ], idp = 3.0, nmax = 3)
    TabPluie[is.na(TabPluie[, iDay]), iDay] <- idwtest$var1.pred
  }
}
#save(TabPluie, file = paste0(DIR_DATA_INPUT, "Tableau_pluie_IDW3.RData"))

##__Boucle_idw_jour_Pour_creation_raster_pluie______________________________####
##Creation de la grille de sortie indiquant les pluvios a combler
bb <- bbox(Bvs_Real)
cs <- c(1000, 1000)
cc <- bb[, 1] + (cs/2)
cd <- ceiling(diff(t(bb))/cs)
real_grd <- GridTopology(cellcentre.offset = cc, cellsize = cs, cells.dim = cd)
real_SG <- SpatialGrid(real_grd, proj4string = prjs.L2E)
raster_Pluie <- stack()

#Boucle jours
load(paste0(DIR_DATA_INPUT, "Tableau_pluie_IDW3.RData"))
Vect_NA <- rep(1, length(TabPluie[1, ]))
for(iDay  in 1:length(TabPluie[1, ])){
  if(length(which(!is.na(TabPluie[, iDay]))) != 15){
    Vect_NA[iDay] <- 0
  } else {
    TabIDW$Pluie_day <- TabPluie[, iDay]
    
    #IDW cas creation raster
    idwtest <- gstat::idw(Pluie_day ~ 1, TabIDW, newdata = real_SG,
                          idp = 3.0, nmax = 3)
    raster_Pluie <- addLayer(raster_Pluie, raster(idwtest, layer = 1))
  }
}
save(TabPluie, file = paste0(DIR_DATA_INPUT, "Tableau_pluie_bassin_IDW3.RData"))
save(raster_Pluie, file = paste0(DIR_DATA_INPUT, "Raster_pluie_real.RData"))

for(i in 1:length(Bvs_Real$ID)){
  Pluie <- rep(NA, length(Vect_NA))
  Pluie[Vect_NA == 1] <- fc.extractValRasterFromPolygon(raster_Pluie, Bvs_Real[i, ], EXTRAC = "Moy")
  Pluie_zoo <- zoo(Pluie, as.Date(PluvioData$TabDatesR))
  save(Pluie_zoo, file= paste0(DIR_DATA_INPUT, "Vecteur_pluie_bassin_",Bvs_Real$ID[i],".RData"))
}
