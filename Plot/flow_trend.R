##____________________________________________________________________________##
##  Script to calculate trend and breaking point on stream flow               ##
##                                                                            ##
##  Pierre L'Hermite - 20171123 - Test_Pluie.R                                ##
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

##__contenu_script__________________________________________________________####

FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoBv/infobv_Real.RData")
load(FileInfobv); rm(FileInfobv); gc()

pdf(paste0(DIR_GRAPHE,"Flow_trend.pdf"), paper ="a4r", height=0, width=0)
for (iBV in c(1:length(infobv$code))){
  ##__Choix_Basin_ou_Boucles_Sur_Plusieurs_Basins___________________________####
  BVCode <- infobv$code[iBV]
  
  ##__BasinData_____________________________________________________________####
  ## RData a creer en amont de ce script, voir script Prep_BasinData.R
  load(paste0(DIR_DATA_INPUT_RDATA, "BasinData_Real/BasinData_",
              BVCode, ".RData"))
  
  #Passage des données journalières au mensuelles
  Pday <- zoo(BasinData$TabObsP, order.by = as.Date(BasinData$TabDatesR))
  MonthlyData <- fc.daily2monthly(Pday, FUN = sum, na.rm = TRUE, threshold = 0.1)
  
  fc.plot_MK_Sen_SQMK_Pettitt(data = MonthlyData, type_data = "Stremflow",
                              bv_nom = infobv$nom[iBV], 
                              nom_axex ="Temps [months]",
                              nom_axey = "Stremflow [mm/month]")
}
dev.off()