##____________________________________________________________________________##
##  Script to change data in basindata                                        ##
##                                                                            ##
##  Pierre L'Hermite - 20171114 - Complete_basin_data.R                       ##
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
library(zoo)

##__Chargement_data_________________________________________________________####
load(paste0(DIR_DATA_INPUT_RDATA, "InfoPluvio/infoPluvio_Real.RData"))
load(paste0(DIR_DATA_INPUT_RDATA, "InfoBv/infobv_Real.RData"))
load(paste0(DIR_DATA_INPUT_RDATA, "PluvioData_Real/PluvioData_83043052.RData"))

##__Data_cut________________________________________________________________####
for(i in 1:length(infobv$code)){
  load(paste0(DIR_DATA_INPUT,"Vecteur_pluie_bassin_", infobv$code[i],".RData"))
  load(paste0(DIR_DATA_INPUT_RDATA, "BasinData_Real/BasinData_", infobv$code[i],".RData"))
  BasinData$TabObsP <- coredata(Pluie_zoo)
  
  zoo_Qm3s <- zoo(BasinData$TabObsQm3s, as.Date(BasinData$TabDatesR))
  zoo_Qmm <- zoo(BasinData$TabObsQmm, as.Date(BasinData$TabDatesR))
  
  Qmm_cut <- Qm3s_cut <- zoo(NA, as.Date(BasinData$TabDatesR))
  Qm3s_cut[1: which(index(zoo_Qm3s)=="2007-12-31")] <- zoo_Qm3s[1: which(index(zoo_Qm3s)=="2007-12-31")]
  Qmm_cut[1: which(index(zoo_Qmm)=="2007-12-31")] <- zoo_Qmm[1: which(index(zoo_Qmm)=="2007-12-31")]
  
  BasinData$TabObsQm3s <- coredata(Qm3s_cut)
  BasinData$TabObsQmm <- coredata(Qmm_cut)
  
  save(BasinData, file = paste0(DIR_DATA_INPUT_RDATA, "BasinData_Real/BasinData_", infobv$code[i], ".RData"))
}

