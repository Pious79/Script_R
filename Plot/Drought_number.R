##____________________________________________________________________________##
##  Script to plot indices SPI, PNI, RAI, ISSP                                ##
##  Pierre L'HERMITE - 2017-11-06 - PLot_SPI_SPEI.R                           ##
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
library(hydroTSM)
library(SPEI)
library(zoo)
library(graphics)
##__contenu_script__________________________________________________________####

FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoBv/infobv_Real.RData")
FileInfoPluvio <- paste0(DIR_DATA_INPUT_RDATA, "InfoPluvio/infoPluvio_Real.RData")
load(FileInfobv); rm(FileInfobv)
load(FileInfoPluvio); rm(FileInfoPluvio); gc()

Duree <- matrix(NA, nrow = 4, ncol = 4)
nom <- infoPluvio$nom[c(12, 14, 3, 5)]
colnames(Duree) <- nom
rownames(Duree) <- c("SPI", "ISSP", "RAI", "PNI")
ikok <- 1
#pdf(paste0(DIR_GRAPHE,"Max_drought.pdf"), paper ="a4r", height=0, width=0)
for(Delta in c(12)){
  for (iBV in c(5)){
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
    
    ResISSP <- fc.ISSP(Ppluvio)
    
    ResPNI <- fc.PNI(Ppluvio, Delta = Delta)
    
    ResRAI <- fc.RAI(Ppluvio, Delta = Delta)
    
    Duree [1, ikok] <- abs(min(ResSPI$Duree, na.rm = TRUE))
    Duree [2, ikok] <- abs(min(ResISSP$Duree, na.rm = TRUE))
    Duree [3, ikok] <- abs(min(ResRAI$Duree, na.rm = TRUE))
    Duree [4, ikok] <- abs(min(ResPNI$Duree, na.rm = TRUE))

    ikok <- ikok + 1
  }
  data <- read.csv(file = "H:/Stage IRSTEA/Dossier/Colloque_Rouen/Sech.csv",
             header = TRUE, sep = ";")
  data2 <- matrix(NA, nrow = 4, ncol = 4)
  data2 <- as.matrix(data)
  
  barplot(data2, beside = TRUE, col = c("red", "green", "blue", "black"),
          ylim = c(0,75), main = "Duree maximum de la periode seche (1988-1993)",
          ylab = "Nombre de secheresse [mois]", names.arg = nom)
  legend("topleft", horiz=TRUE, legend=c("SPI", "ISSP", "RAI", "PNI"),
         col= c("red", "green", "blue", "black"), pch = 15, cex = 1)
}
#dev.off()