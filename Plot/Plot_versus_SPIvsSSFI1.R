##____________________________________________________________________________##
##  Script to plot indices SPI vs PNI                                         ##
##  Pierre L'HERMITE - 2018-01-11 - Plot_versus_SPIvsSSFI.R                    ##
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

pdf(paste0(DIR_GRAPHE,"Versus_SSFIvsSPI.pdf"), paper ="a4r", height=0, width=0)
for(Delta in c(1, 3, 6, 9, 12, 24)){
  iBV <- 0
  for (iPluvio in c(12, 8, 14, 4, 7, 1, 7)){
    iBV <- iBV + 1
    ##__Choix_Basin_ou_Boucles_Sur_Plusieurs_Basins_____________________________####
    PluvioCode <- infoPluvio$code[iPluvio]
    BVCode <- infobv$code[iBV]
    
    ##__BasinData_______________________________________________________________####
    ## RData a creer en amont de ce script, voir script Prep_BasinData.R
    load(paste0(DIR_DATA_INPUT_RDATA, "PluvioData_Real/PluvioData_",
                PluvioCode, ".RData"))
    load(paste0(DIR_DATA_INPUT_RDATA, "BasinData_Real/BasinData_",
                BVCode, ".RData"))
    
    #Fonction pour calculer les indices
    Ppluvio <- fc.daily2monthly(zoo(PluvioData$TabcompleteP, PluvioData$TabDatesR),
                                FUN = sum, na.rm = TRUE, threshold = 0.1)
    ResSPI <- fc.SPI(Ppluvio, Delta = Delta, Distribution = "gamma")
    
    Qmonth <- fc.daily2monthly(zoo(BasinData$TabObsQmm, as.Date(BasinData$TabDatesR)),
                               FUN = sum, na.rm = TRUE, threshold = 0.1)
    ResSSFI <- fc.SSFI(Qmonth, Delta = Delta, Distribution = "gamma")
    
    CorSPI <- ResSPI$SPI
    CorSPI[is.na(ResSPI$SPI)] <- 0
    
    CorSSFI <- ResSSFI$SSFI
    CorSSFI[is.na(ResSSFI$SSFI)] <- 0
    
    Corre <- cor(CorSPI, CorSSFI, method = "pearson")
    
    #Plot SPI
    plot(ResSPI$SPI, ResSSFI$SSFI, axes=FALSE, panel.first = grid(), pch = 18,
         ylim = c(-4, 4), xlim = c(-4, 4), col = "grey34", 
         xlab = "SPI", ylab = "SSFI", main = paste("Pluie :", PluvioData$PluvioName,
                                                   "Debit :", BasinData$BasinName, 
                                                   Delta))
    axis(1, c(-4: 4), pos = 0, cex.axis = 0.8) # Abscisses
    axis(2, c(-4: 4), pos = 0, cex.axis = 0.8, las = 2) # Ordonnees
    abline(a = 0, b = 1, col = "red")
    legend("topleft", legend = paste0( "r = ", round(Corre, 2)))
  }
}
dev.off()
