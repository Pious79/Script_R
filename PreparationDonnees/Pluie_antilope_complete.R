##____________________________________________________________________________##
##  Script to complete rain data from Antilope                                ##
##  Pierre L'HERMITE - 2017-11-07 - Pluie_antilope_complete.R                 ##
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
library(zoo)

##__contenu_script__________________________________________________________####

load(paste0(DIR_DATA_OUTPUT, "Antilope_post2006.RData"))
Post2006 <- Pluie_pluvio
load(paste0(DIR_DATA_OUTPUT, "Antilope_pre2006.RData"))
Pre2006 <- Pluie_pluvio
rm(Pluie_pluvio)


#Coordonnees pluviometre
FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoPluvio/infoPluvio_Real.RData")
load(FileInfobv); rm(FileInfobv); gc()

Date <- seq.Date(as.Date("1997-01-01"), as.Date("2006-12-30"), "days")
Date2 <- seq.Date(as.Date("2006-07-01"), as.Date("2015-10-29"), "days")
for(icol in 1:length(infoPluvio$nom)){
  pluie <- zoo(Pre2006[ , icol], Date)
  pluiefin <- zoo(Post2006[ , icol], Date2)
  pluiefin <- pluiefin[which(index(pluiefin)=="2006-12-31"):length(pluiefin)]
  Pantilope <- c(pluie, pluiefin)
  save(Pantilope, file = paste0(DIR_DATA_OUTPUT, "Pluie_antilope/Pluie_antilope_", infoPluvio$code[icol],".RData"))
}
