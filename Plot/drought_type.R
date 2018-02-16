##____________________________________________________________________________##
##  Plot drought type of different indices                                    ##
##                                                                            ##
##  Pierre L'Hermite - 20171129 - Plot_drought.R                              ##
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

DIR_SPI <- ("C:/Users/pierre.lhermite/Documents/Pierre/Workspace_R/DataOutput/Graphes/SPI_Pluvio/")
DIR_RAI <- ("C:/Users/pierre.lhermite/Documents/Pierre/Workspace_R/DataOutput/Graphes/RAI_Pluvio/")
DIR_PNI <- ("C:/Users/pierre.lhermite/Documents/Pierre/Workspace_R/DataOutput/Graphes/PNI_Pluvio/")

Delta <- 1

load(paste0(DIR_SPI, "Sech_SPI_", Delta, ".RData"))
SPI_tab <- Sech
load(paste0(DIR_PNI, "Sech_PNI_", Delta, ".RData"))
PNI_tab <- Sech

mat <- matrix(c(3, 2, 1, 5, 6, 4), nrow = 3, dimnames = list(c("1", "2", "3"), c("a", "b")))
barplot(mat, beside = TRUE, horiz = TRUE, col = c("red", "green", "blue"), names.arg = toupper(colnames(mat)), legend.text = TRUE)