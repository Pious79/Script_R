##____________________________________________________________________________##
##  Script to calculate trend and breaking point on rain                      ##
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
DIR_DATA_INPUT_DEBITS <- paste0(DIR_DATA_INPUT, "bvTxt/QJ_brute_csv_ls/")
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
FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoPluvio/infoPluvio_Real.RData")
load(FileInfobv); rm(FileInfobv); gc()
mois <- c("Janvier", "Fevrier", "Mars", "Avril", "Mai", "Juin", "Juillet",
          "Aout", "Septembre", "Octobre", "Novembre", "Decembre")

for (m in 1:12){
  pdf(paste0(DIR_GRAPHE,"Pluvio_trend_",mois[m], ".pdf"), paper ="a4r", height=0, width=0)
  for (iPluvio in c(1:length(infoPluvio$code))){
    ##__Choix_Basin_ou_Boucles_Sur_Plusieurs_Basins___________________________####
    PluvioCode <- infoPluvio$code[iPluvio]
    
    ##__BasinData_____________________________________________________________####
    ## RData a creer en amont de ce script, voir script Prep_BasinData.R
    load(paste0(DIR_DATA_INPUT_RDATA, "PluvioData_Real/PluvioData_",
                PluvioCode, ".RData"))
    
    #Passage des données journalières au mensuelles
    Pday <- zoo(PluvioData$TabcompleteP, order.by = PluvioData$TabDatesR)
    MonthlyData <- fc.daily2monthly(Pday, FUN = sum, na.rm = TRUE, threshold = 0.1)
    
    fc.plot_MK_Sen_SQMK_Pettitt(data = extract(MonthlyData, trgt = m),
                                type_data = "Pluie",
                                bv_nom = infoPluvio$nom[iPluvio], 
                                nom_axex ="Temps [months]",
                                nom_axey = "Pluie [mm/month]")
  }
  dev.off()
}