##______________________________________________________________________________##
##  Script to calculate ISSQ                                                    ##
##  Pierre L'HERMITE - 2018-01-19 - ISSQ.R                                      ##
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
library(SCI)
library(zoo)

##__contenu_script__________________________________________________________####

FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoBv/infobv_Real.RData")
load(FileInfobv); rm(FileInfobv); gc()

Sech <- as.data.frame(matrix(NA, nrow = length(infobv$code) , ncol = 7))
rownames(Sech) <- infobv$nom
colnames(Sech) <- c("ExWet", "VWet", "Wet", "Normal", "Dry", "VDry", "ExDry")

pdf(paste0(DIR_GRAPHE,"ISSQ_BV/ISSQ.pdf"), paper ="a4r", height=0, width=0)
for (iBV in c(1:1)){ #length(infobv$code)
  ##__Choix_Basin_ou_Boucles_Sur_Plusieurs_Basins_____________________________####
  BVCode <- infobv$code[iBV]
  
  ##__BasinData_______________________________________________________________####
  ## RData a creer en amont de ce script, voir script Prep_BasinData.R
  load(paste0(DIR_DATA_INPUT_RDATA, "BasinData_Real/BasinData_",
              BVCode, ".RData"))
  
  #Passage des données journalières au mensuelles
  Qday <- zoo(BasinData$TabObsQmm, order.by = as.Date(BasinData$TabDatesR))
  MonthlyData <- fc.daily2monthly(Qday, FUN = sum, na.rm = TRUE, threshold = 0.1)
  
  Decile_mois <- rep(NA, 12)
  for(mois in 1:12){
    Decile <- fc.CalcStat(extract(MonthlyData, trgt = mois), debYear = "08-01")
  }
  
  #Fonction pour calculer le ISSQ
  Resultat <- fc.ISSQ(MonthlyData)
  
  #Trace les graphiques avec les résultats de MK, Sen et Pettitt
  fc.plot_MK_Sen_SQMK_Pettitt(data = Resultat$ISSQ, type_data = "ISSQ",
                              bv_nom = infobv$nom[iBV], nom_axex = "Annee",
                              nom_axey = "ISSQ [-]")
  
  #Incrementation du tableau finale de secheresse
  Sech[iBV, ] <- Resultat$Drought$Pluvio
}

save(Sech, file = paste0(DIR_GRAPHE, "ISSQ_BV/Sech_ISSQ.RData"))
dev.off()
