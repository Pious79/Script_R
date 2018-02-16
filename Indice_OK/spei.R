##____________________________________________________________________________##
##  Script to calculate SPEI                                                  ##
##  Pierre L'HERMITE - 2017-11-06 - SPEI.R                                    ##
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
library(hydroTSM)
library(SPEI)
library(zoo)
library(graphics)
##__contenu_script__________________________________________________________####

FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoBv/infobv_Real.RData")
load(FileInfobv); rm(FileInfobv); gc()

for(Delta in c(1, 3, 6, 9, 12, 24)){
  Sech <- as.data.frame(matrix(NA, nrow = length(infobv$code) , ncol = 7))
  rownames(Sech) <- infobv$nom
  colnames(Sech) <- c("ExWet", "VWet", "Wet", "Normal", "Dry", "VDry", "ExDry")
  #Delta <- 24
  
  pdf(paste0(DIR_GRAPHE,"SPEI_BV/SPEI_", Delta, ".pdf"), paper ="a4r", height=0, width=0)
  for (iBV in c(1:length(infobv$code))){
    ##__Choix_Basin_ou_Boucles_Sur_Plusieurs_Basins_____________________________####
    BVCode <- infobv$code[iBV]
    
    ##__BasinData_______________________________________________________________####
    ## RData a creer en amont de ce script, voir script Prep_BasinData.R
    load(paste0(DIR_DATA_INPUT_RDATA, "BasinData_Real/BasinData_",
                BVCode, ".RData"))
    
    #Passage des données journalières au mensuelles
    Pday <- zoo(BasinData$TabObsP, order.by = as.Date(BasinData$TabDatesR))
    ETPday <- zoo(BasinData$TabObsE, order.by = as.Date(BasinData$TabDatesR))
    Pmonth <- fc.daily2monthly(Pday, FUN = sum, na.rm = TRUE, threshold = 0.1)
    ETPmonth <- fc.daily2monthly(ETPday, FUN = sum, na.rm = TRUE, threshold = 0.1)
    
    #Diff entre le cumul des pluies mensuelles et le cumul de l'ETP Penman
    #mensuel
    Diff <- Pmonth - ETPmonth
    
    #Fonction pour calculer le SPEI
    Resultat <- fc.SPEI(Diff, Delta = Delta, Distribution = "log-Logistic")
    
    #Tracer 
    fc.plot_MK_Sen_SQMK_Pettitt(data = Resultat$SPEI, type_data = "SPEI",
                                bv_nom = infobv$nom[iBV], nom_axex = "Temps",
                                nom_axey = "SPEI [-]")
    
    Sech[iBV, ] <- Resultat$Drought$Pluvio
  }
  
  save(Sech, file = paste0(DIR_GRAPHE, "SPEI_BV/Sech_SPEI_", Resultat$Delta, ".RData"))
  dev.off()
}
