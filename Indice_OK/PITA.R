##______________________________________________________________________________##
##  Script to calculate ISSP index of Pita                                      ##
##  Pierre L'HERMITE - 2017-10-11 - Script_Pita.R                               ##
##______________________________________________________________________________##

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
library(ggplot2)
library(zoo)

##__contenu_script__________________________________________________________####
FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoPluvio/infoPluvio_Real.RData")
load(FileInfobv); rm(FileInfobv); gc()

Sech <- as.data.frame(matrix(NA, nrow = length(infoPluvio$code) , ncol = 7))
rownames(Sech) <- infoPluvio$nom
colnames(Sech) <- c("ExWet", "VWet", "Wet", "Normal", "Dry", "VDry", "ExDry")

pdf(paste0(DIR_GRAPHE,"PITA/ISSP.pdf"), paper ="a4r", height=0, width=0)
for (i in c(1:length(infoPluvio$code))){
  ##__Choix_Basin_ou_Boucles_Sur_Plusieurs_Basins_____________________________####
  PluvioCode <- infoPluvio$code[i]
  
  ##__BasinData_______________________________________________________________####
  ## RData a creer en amont de ce script, voir script Prep_BasinData.R
  load(paste0(DIR_DATA_INPUT_RDATA, "PluvioData_Real/PluvioData_",
              PluvioCode, ".RData"))
  
  #Passage des données journalières au mensuelles
  Pday <- zoo(PluvioData$TabcompleteP, order.by = as.Date(PluvioData$TabDatesR))
  datam<-fc.daily2monthly(Pday, FUN = sum)
  
  Res<-fc.ISSP(datam)
  
  ISSP_df<-data.frame(index(Res$ISSP), coredata(Res$ISSP))
  colnames(ISSP_df)<-c("Dates","ISSP")
  
  Sech[i, ] <- Res$Sech$Pluvio
  
  fc.plot_MK_Sen_SQMK_Pettitt(Res$ISSP, type_data = "ISSP",
                              bv_nom = infoPluvio$nom[i], nom_axex = "Date [-]",
                              nom_axey = "ISSP [-]")
}

save(Sech, file = paste0(DIR_GRAPHE, "PITA/Sech_ISSP.RData"))
dev.off()
