##____________________________________________________________________________##
##  Script to plot indices SPI vs PNI                                         ##
##  Pierre L'HERMITE - 2018-01-11 - Plot_versus_SPIvsPNI.R                    ##
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
library(hydroTSM)
library(SPEI)
library(zoo)
library(graphics)
##__contenu_script__________________________________________________________####

FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoBv/infobv_Real.RData")
FileInfoPluvio <- paste0(DIR_DATA_INPUT_RDATA, "InfoPluvio/infoPluvio_Real.RData")
load(FileInfobv); rm(FileInfobv)
load(FileInfoPluvio); rm(FileInfoPluvio); gc()

pdf(paste0(DIR_GRAPHE,"Versus_PNIvsSPI.pdf"), paper ="a4r", height=0, width=0)
layout(matrix(c(1, 2,
                3, 4), ncol = 2, byrow = TRUE))
for(Delta in c(1, 3, 6, 9, 12, 24)){
  for (iBV in c(12, 14, 3, 5)){
    ##__Choix_Basin_ou_Boucles_Sur_Plusieurs_Basins_____________________________####
    PluvioCode <- infoPluvio$code[iBV]
    ##__BasinData_______________________________________________________________####
    ## RData a creer en amont de ce script, voir script Prep_BasinData.R
    load(paste0(DIR_DATA_INPUT_RDATA, "PluvioData_Real/PluvioData_",
                PluvioCode, ".RData"))
    
    #Fonction pour calculer les indices
    Ppluvio <- fc.daily2monthly(zoo(PluvioData$TabcompleteP, PluvioData$TabDatesR),
                                FUN = sum, na.rm = TRUE, threshold = 0.1)
    ResSPI <- fc.SPI(Ppluvio, Delta = Delta, Distribution = "gamma")
    
    ResPNI <- fc.PNI(Ppluvio, Delta = Delta)
    
    CorSPI <- ResSPI$SPI
    CorSPI[is.na(ResSPI$SPI)] <- 0
    
    CorPNI <- ResPNI$PNI
    CorPNI[is.na(ResPNI$PNI)] <- 0
    
    Corre <- cor(CorSPI, CorPNI, method = "pearson")
    
    #Plot SPI
    plot(ResSPI$SPI, ResPNI$PNI, axes=FALSE, panel.first = grid(), pch = 18,
         ylim = c(0, 200), xlim = c(-4, 4), col = "grey34", 
         xlab = "SPI", ylab = "PNI", main = paste(PluvioData$PluvioName, Delta))
    axis(1, c(-4: 4), pos = 100, cex.axis = 0.8) # Abscisses
    axis(2, c(0, 50, 100, 150, 200), pos = 0, cex.axis = 0.8, las = 2) # Ordonnees
    legend("topleft", legend = paste0( "r = ", round(Corre, 2)))
  }
}
dev.off()
