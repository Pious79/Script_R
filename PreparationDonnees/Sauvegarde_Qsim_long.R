##____________________________________________________________________________##
##  Script to save Qsim in a long zoo with NA pour basinData                  ##
##                                                                            ##
##  Pierre L'Hermite - 20171024 - Sauvegarde_Qsim_long.R                      ##
##____________________________________________________________________________##

#__Root_Directory___________________________________________________________####
# ---- si travail sur le serveur
User <- "Pierre"
# ---- si travail en local
Disc <- "C"

if(substr(version$os, 1, 5) == "linux") {
  DIR_ROOT <- paste0("/home/RHAX21/UTILISATEURS/", User, "/Workspace_R/")
} else {
  if(Disc == "X") {
    DIR_ROOT <- paste0(Disc, ":/UTILISATEURS/", User, "/Workspace_R/")
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
DIR_FUNCTIONS           <- "Functions/"
DIR_GRAPHE              <- paste0(DIR_DATA_OUTPUT, "Graphes/")
DIR_FUNCTIONS_LOIEAU    <- "X:/UTILISATEURS/LOIEAU/Workspace_R/Functions"

##__Functions_______________________________________________________________####
for(FileName in list.files(DIR_FUNCTIONS, pattern = "\\.[Rr]$")) {
  source(file.path(DIR_FUNCTIONS, FileName))
}
for(FileName in list.files(DIR_FUNCTIONS_LOIEAU, pattern = "\\.[Rr]$")) {
  source(file.path(DIR_FUNCTIONS_LOIEAU, FileName))
}
rm(FileName); gc()

##__Packages________________________________________________________________####
library(hydroTSM)
library(zoo)
library(lattice)

##__contenu_script__________________________________________________________####
FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoBv/infobv_Real.RData")
load(FileInfobv); rm(FileInfobv); gc()

for (i in c(1:length(infobv$code))){
  ##__Choix_Basin_ou_Boucles_Sur_Plusieurs_Basins_____________________________####
  BasinCode <- infobv$code[i]
  
  ##__BasinData_______________________________________________________________####
  ## RData a creer en amont de ce script, voir script Prep_BasinData.R
  load(paste0(DIR_DATA_INPUT_RDATA, "BasinData_Real/BasinData_",
              BasinCode, ".RData"))
  
  ## Chargement du débit simulé
  load(paste0(DIR_DATA_INPUT, "Qsimule/Qsim_calagecut/Qsim_",BasinCode,".RData"))
  
  Q <- zoo(NA, order.by = as.Date(BasinData$TabDatesR))
  
  ## Creation du zoo de meme longueur que les BasinData
  DebChron <- which(as.Date(BasinData$TabDatesR) == index(Qsim[1]))
  FinChron <- which(as.Date(BasinData$TabDatesR) == index(Qsim[length(Qsim)]))
  Q[DebChron:FinChron] <- Qsim
  
  save(Q, file = paste0(DIR_DATA_OUTPUT, "Qsim_long/Qsim_long_", BasinCode,".RData"))
}