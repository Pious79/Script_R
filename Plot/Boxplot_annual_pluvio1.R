##____________________________________________________________________________##
##  Script to plot boxplot annual mean                                        ##
##                                                                            ##
##  Pierre L'Hermite - 20171213 - Boxplot_annual_pluvio.R                     ##
##____________________________________________________________________________##

#__Root_Directory___________________________________________________________####
# ---- si travail sur le serveur
User <- "Pierre"
# ---- si travail en local
Disc <- "H"

if(substr(version$os, 1, 5) == "linux") {
  DIR_ROOT <- paste0("/home/RHAX21/UTILISATEURS/", User, "/Workspace_R/")
} else {
  if(Disc == "X") {
    DIR_ROOT <- paste0(Disc, ":/UTILISATEURS/", User, "/Workspace_R/")
  } else {
    DIR_ROOT <- paste0(Disc, ":/Workspace_R/")
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
DIR_FUNCTIONS_LOIEAU    <- "H:/Stage IRSTEA/Dossier/Fonctions_LOIEAU"

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
library(hydroTSM)

##__contenu_script__________________________________________________________####

FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoPluvio/infoPluvio_Real.RData")
load(FileInfobv); rm(FileInfobv); gc()

nom <- infoPluvio$nom #[c(12, 14, 3, 5)]
nom2 <- c("Davids", "Peyrol", "Portaniere", "Martels", "Meffrey", "Bonaud",
          "Vaudreches", "Babaou", "Cros\nde Guerin", "Col des\nfourches",
          "Bourdins", "Lambert", "Cros de\nmouton", "Croix\nd'Anselme",
          "Louvieres")
ordre <- c(15, 11, 12, 9, 10, 2, 13, 1, 4, 6, 14, 7, 5, 3, 8)
Pluie <- as.data.frame(matrix(NA, nrow = 58, ncol = 15))
irow <- 1

for (i in ordre){ #c(12, 14, 3, 5)
  ##__Choix_Basin_ou_Boucles_Sur_Plusieurs_Basins_____________________________####
  PluvioCode <- infoPluvio$code[i]
  
  ##__BasinData_______________________________________________________________####
  ## RData a creer en amont de ce script, voir script Prep_BasinData.R
  load(paste0(DIR_DATA_INPUT_RDATA, "PluvioData_Real/PluvioData_",
              PluvioCode, ".RData"))
  
  Pday <- zoo(PluvioData$TabcompleteP, PluvioData$TabDatesR)
  Pannual <- fc.daily2annual(Pday, debYear = "08-01", FUN = sum, na.rm = TRUE,
                             threshold = 0.1)
  Pluie [ , irow] <- coredata(Pannual)
  irow <- irow +1
}
colnames(Pluie) <- nom2

color <- c("white", "white", "red", "white", "white", "white", "white", "white",
           "white", "white", "red", "white", "red", "red", "white")

boxplot20(Pluie, main = "Cumul annuel sur les bassins versants",
          ylab = "Precipitations [mm/an]", cex.axis = 0.8, boxfill = color)
