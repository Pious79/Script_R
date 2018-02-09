##____________________________________________________________________________##
##  Script to plot indices SPI, PNI, RAI, ISSP                                ##
##  Pierre L'HERMITE - 2017-11-06 - PLot_SPI_SPEI.R                           ##
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

pdf(paste0(DIR_GRAPHE,"Comparaison_Standard.pdf"), paper ="a4r", height=0, width=0)
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
    Pmonth <- fc.daily2monthly(zoo(PluvioData$TabcompleteP, PluvioData$TabDatesR),
                                FUN = sum, na.rm = TRUE, threshold = 0.1)
    ResSPI <- fc.SPI(Pmonth, Delta = Delta, Distribution = "gamma")
    
    Qmonth <- fc.daily2monthly(zoo(BasinData$TabObsQmm, as.Date(BasinData$TabDatesR)),
                            FUN = sum, na.rm = TRUE, threshold = 0.1)
    ResSSFI <- fc.SSFI(Qmonth, Delta = Delta, Distribution = "gamma")
    
    ETPday <- zoo(BasinData$TabObsE, order.by = as.Date(BasinData$TabDatesR))
    ETPmonth <- fc.daily2monthly(ETPday, FUN = sum, na.rm = TRUE, threshold = 0.1)
    
    #Diff entre le cumul des pluies mensuelles et le cumul de l'ETP Penman
    #mensuel
    Diff <- Pmonth - ETPmonth
    
    #Fonction pour calculer le SPEI
    ResSPEI <- fc.SPEI(Diff, Delta = Delta, Distribution = "log-Logistic")
    
    plot(ResSPI$SPI, xlab = "Temps", ylab = "Indice standardisé", ylim = c(-4:4),
         main = paste("Comparaison des indices standardisés SPI, SPEI, SSFI
                      Pas de temps :", Delta, "mois  Debit : ", infobv$nom[iBV],
                      "Pluie :", infoPluvio$nom[iPluvio]))
    legend("topleft", legend = c("SPI", "SSFI", "SPEI"),
           col = c("black", "blue", "green"), lty=1, lwd = 1.5, bty = "n",
           cex = 0.8) 
    lines(ResSSFI$SSFI, col = "blue")
    lines(ResSPEI$SPEI, col = "green")
    abline(h = 0, col = "grey34")
  }
}
dev.off()
