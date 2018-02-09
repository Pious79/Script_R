##____________________________________________________________________________##
##  Plot sur  la robustesse du modele GR4J sur les bassins versants           ##
##          du Real Collobrier                                                ##
##  Florine Garcia - 20171103 - Analyse_Robustesse_GR4J.R                     ##
##  Script modifie le 20171105 par Florine Garcia :                           ##
##____________________________________________________________________________##

#__Root_Directory___________________________________________________________####
# ---- si travail sur le serveur
User <- "Pierre"
# ---- si travail en local
Disc <- "C"

if (substr(version$os, 1, 5) == "linux") {
  DIR_ROOT <- paste0("/home/RHAX21/UTILISATEURS/", User, "/Workspace_R/")
} else {
  if (Disc == "X") {
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
DIR_DATA_INPUT       <- "DataInput/"
DIR_DATA_INPUT_RDATA <- paste0(DIR_DATA_INPUT, "bvRdata/")
DIR_DATA_OUTPUT      <- "DataOutput/"
DIR_FUNCTIONS        <- "Functions/"
DIR_GRAPHE           <- paste0(DIR_DATA_OUTPUT, "Graphes/")
DIR_FUNCTIONS_LOIEAU    <- "X:/UTILISATEURS/LOIEAU/Workspace_R/Functions"

##__Functions_______________________________________________________________####
for (FileName in list.files(DIR_FUNCTIONS, pattern = "\\.[Rr]$")) {
  source(file.path(DIR_FUNCTIONS, FileName))
}
for(FileName in list.files(DIR_FUNCTIONS_LOIEAU, pattern = "\\.[Rr]$")) {
  source(file.path(DIR_FUNCTIONS_LOIEAU, FileName))
}
rm(FileName); gc()

##__Packages________________________________________________________________####
library(rgdal); library(maptools)

##__Definition_des_projections______________________________________________####
prjs.L2E <- CRS("+init=epsg:27572")

##__Lecture_Shapes_HYDRO____________________________________________________####
DsnReal <- paste0("H:/Stage IRSTEA/Dossier/Workspace_R/DataInput/contour")
LayerReal <- "BvsReal_limni"
## Chargement des shapes (en L2E)
Bvs_Real <- readOGR(dsn = DsnReal, layer = LayerReal, stringsAsFactors = FALSE)
Bv_data <- slot(Bvs_Real, "data")
rm(DsnReal, LayerReal); gc()
col <- c("black", "green", "red", "lightblue", "purple", "orange", "pink")
colReal <- col
for(iBasin in 1:dim(Bv_data)[1]) {
  colReal[iBasin] <- col[infobv$code == Bv_data$ID[iBasin]]
}

pdf("H:/Stage IRSTEA/Dossier/Workspace_R/DataOutput/Graphes/BV_coleur_Real.pdf",
    paper = "a4r", height = 0, width = 0)
plot(Bvs_Real, col = colReal)
points(infobv$exu_L2E, pch = 16, col = "cyan")
legend("topleft", legend = infobv$nom, fill = col, cex = 1)
dev.off()

