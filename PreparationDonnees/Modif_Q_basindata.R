##____________________________________________________________________________##
##  Script to calculate trend and breaking point on stream flow               ##
##                                                                            ##
##  Pierre L'Hermite - 20171123 - Test_Pluie.R                                ##
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
DIR_DATA_INPUT_PLUIE  <- paste0(DIR_DATA_INPUT_RDATA, "PJ_bassin_reconstituee_zoo/")
if(substr(version$os, 1, 5) == "linux") {
  DIR_DATA_INPUT_SAFRAN <- "/home/RHAX/DONNEES/LOIEAU/SAFRAN/"
} else {
  DIR_DATA_INPUT_SAFRAN <- "Z:/DONNEES/LOIEAU/SAFRAN/"
}
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

FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoBv/infobv_Real.RData")
load(FileInfobv); rm(FileInfobv); gc()

for(iBV in 1:length(infobv$code)){
  ##__Choix_Basin_ou_Boucles_Sur_Plusieurs_Basins___________________________####
  BVCode <- infobv$code[iBV]
  
  ##__BasinData_____________________________________________________________####
  ## RData a creer en amont de ce script, voir script Prep_BasinData.R
  load(paste0(DIR_DATA_INPUT_RDATA, "BasinData_Real/BasinData_",
              BVCode, ".RData"))
  
  QJ_brut <- read.table(file = paste0(DIR_DATA_INPUT,"bvTxt/QJ_brut/QJ_", infobv$code[iBV],".csv"),
                        sep=";", header = TRUE)
  zoo_brute <- zoo(QJ_brut$Debit, as.Date(strptime(QJ_brut$Date, format = "%Y%m%d")))
  zoo_cut <- zoo_brute[1 : which(index(zoo_brute) == "2007-12-31")]
  
  zoo_NA <- zoo(NA, as.Date(BasinData$TabDatesR))
  zoo_NA[which(index(zoo_NA) == index(zoo_cut[1])) : which(index(zoo_NA) == index(zoo_cut[length(zoo_cut)]))] <- zoo_cut
  
  BasinData$TabObsQm3s <- coredata(zoo_NA)
  BasinData$TabObsQmm <- coredata(zoo_NA)*24*3.6/(infobv$surf[iBV]*1000)
  
  save(BasinData, file = paste0(DIR_DATA_INPUT_RDATA, "BasinData_Real/BasinData_", infobv$code[iBV], ".RData"))
}