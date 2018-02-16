##____________________________________________________________________________##
##  Plot selon le type de secheresse SPI                                      ##
##  Pierre L'HERMITE - 2017-10-12 - Carte_type_periode_SPI.R                  ##
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
library(SCI)
library(zoo)
library(rgdal)

##__contenu_script__________________________________________________________####

FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoBV/infobv_Real.RData")
FileInfoPluvio <- paste0(DIR_DATA_INPUT_RDATA, "InfoPluvio/infoPluvio_Real.RData")
load(FileInfobv); rm(FileInfobv); load(FileInfoPluvio); rm(FileInfoPluvio); gc() 

Delta <- 24

##__Tab_Sech________________________________________________________________####
## Sech du SPI pour tous les pluviometres
load(paste0(DIR_GRAPHE, "SPI_Pluvio/Sech_SPI_",
            Delta, ".RData"))
SechPluvio <- Sech

## Sech du SPI pour les pluies du BV
load(paste0(DIR_GRAPHE, "SPI_BV/Sech_SPI_BV_",
            Delta, ".RData"))
SechBV <- Sech

rm(Sech)

## Chargement du fond de carte du bassin
DsnReal <- paste0("C:/Users/pierre.lhermite/Documents/Pierre/Workspace_R/DataInput/contour")
LayerReal <- "BvsReal_limni"
Bvs_Real <- readOGR(dsn = DsnReal, layer = LayerReal, stringsAsFactors = FALSE)

# Cours <- "COURS_D_EAU_polyline"
# Cours_Real <- readOGR(dsn = DsnReal, layer = Cours, stringsAsFactors = FALSE)
# lines(Cours_Real, col="blue")
# points(infobv$exu_L2E, pch= 16)
# plot(infoPluvio$coord, pch=16, col="red")

SechPluvio[2,] <- SechPluvio[2,] + SechPluvio[1,]
SechPluvio[6,] <- SechPluvio[6,] + SechPluvio[7,]
SechBV[2,] <- SechBV[2,] + SechBV[1,]
SechBV[6,] <- SechBV[6,] + SechBV[7,]

paletteWet <- colorRampPalette(c("white", "blue"))(11)
#Couleur humide pour les types sur les pluvio
ColWet <- as.data.frame(matrix(NA, ncol = length(SechPluvio) , nrow = 7))
for (tab in c(SechPluvio, SechBV)){
  for(i in c(1:2)){
    for(j in c(1:length(SechPluvio))){
      if(SechPluvio[i,j] >= 0 & SechPluvio[i,j] < 5){
        ColWet[i, j] <- paletteWet[1]
      } else if(SechPluvio[i, j] >= 5 & SechPluvio[i,j] < 10){
        ColWet[i, j] <- paletteWet[2]
      } else if(SechPluvio[i, j] >= 10 & SechPluvio[i,j] < 15){
        ColWet[i, j] <- paletteWet[3]
      } else if(SechPluvio[i, j] >= 15 & SechPluvio[i,j] < 20){
        ColWet[i, j] <- paletteWet[4]
      } else if(SechPluvio[i, j] >= 20 & SechPluvio[i,j] < 25){
        ColWet[i, j] <- paletteWet[5]
      } else if(SechPluvio[i, j] >= 25 & SechPluvio[i,j] < 30){
        ColWet[i, j] <- paletteWet[6]
      } else if(SechPluvio[i, j] >= 30 & SechPluvio[i,j] < 35){
        ColWet[i, j] <- paletteWet[7]
      } else if(SechPluvio[i, j] >= 35 & SechPluvio[i,j] < 40){
        ColWet[i, j] <- paletteWet[8]
      } else if(SechPluvio[i, j] >= 40 & SechPluvio[i,j] < 45){
        ColWet[i, j] <- paletteWet[9]
      } else if(SechPluvio[i, j] >= 45 & SechPluvio[i,j] <= 50){
        ColWet[i, j] <- paletteWet[10]
      } else if(SechPluvio[i, j] >= 50){
        ColWet[i, j] <- paletteWet[11]
      }
    }
  }
}

#Couleur humide pour les types sur les BV
ColWet_BV <- as.data.frame(matrix(NA, ncol = length(SechBV) , nrow = 7))
for (tab in c(SechBV, SechBV)){
  for(i in c(1:2)){
    for(j in c(1:length(SechBV))){
      if(SechBV[i,j] >= 0 & SechBV[i,j] < 5){
        ColWet_BV[i, j] <- paletteWet[1]
      } else if(SechBV[i, j] >= 5 & SechBV[i,j] < 10){
        ColWet_BV[i, j] <- paletteWet[2]
      } else if(SechBV[i, j] >= 10 & SechBV[i,j] < 15){
        ColWet_BV[i, j] <- paletteWet[3]
      } else if(SechBV[i, j] >= 15 & SechBV[i,j] < 20){
        ColWet_BV[i, j] <- paletteWet[4]
      } else if(SechBV[i, j] >= 20 & SechBV[i,j] < 25){
        ColWet_BV[i, j] <- paletteWet[5]
      } else if(SechBV[i, j] >= 25 & SechBV[i,j] < 30){
        ColWet_BV[i, j] <- paletteWet[6]
      } else if(SechBV[i, j] >= 30 & SechBV[i,j] < 35){
        ColWet_BV[i, j] <- paletteWet[7]
      } else if(SechBV[i, j] >= 35 & SechBV[i,j] < 40){
        ColWet_BV[i, j] <- paletteWet[8]
      } else if(SechBV[i, j] >= 40 & SechBV[i,j] < 45){
        ColWet_BV[i, j] <- paletteWet[9]
      } else if(SechBV[i, j] >= 45 & SechBV[i,j] <= 50){
        ColWet_BV[i, j] <- paletteWet[10]
      } else if(SechBV[i, j] >= 50){
        ColWet_BV[i, j] <- paletteWet[11]
      }
    }
  }
}

paletteDry <- colorRampPalette(c("white", "red"))(11)
#Couleur seche pour les types sur les pluvio
ColDry <- as.data.frame(matrix(NA, ncol = length(SechPluvio) , nrow = 7))
for(i in c(6:7)){
  for(j in c(1:length(SechPluvio))){
    if(SechPluvio[i,j] >= 0 & SechPluvio[i,j] < 5){
      ColDry[i, j] <- paletteDry[1]
    } else if(SechPluvio[i, j] >= 5 & SechPluvio[i,j] < 10){
      ColDry[i, j] <- paletteDry[2]
    } else if(SechPluvio[i, j] >= 10 & SechPluvio[i,j] < 15){
      ColDry[i, j] <- paletteDry[3]
    } else if(SechPluvio[i, j] >= 15 & SechPluvio[i,j] < 20){
      ColDry[i, j] <- paletteDry[4]
    } else if(SechPluvio[i, j] >= 20 & SechPluvio[i,j] < 25){
      ColDry[i, j] <- paletteDry[5]
    } else if(SechPluvio[i, j] >= 25 & SechPluvio[i,j] < 30){
      ColDry[i, j] <- paletteDry[6]
    } else if(SechPluvio[i, j] >= 30 & SechPluvio[i,j] < 35){
      ColDry[i, j] <- paletteDry[7]
    } else if(SechPluvio[i, j] >= 35 & SechPluvio[i,j] < 40){
      ColDry[i, j] <- paletteDry[8]
    } else if(SechPluvio[i, j] >= 40 & SechPluvio[i,j] < 45){
      ColDry[i, j] <- paletteDry[9]
    } else if(SechPluvio[i, j] >= 45 & SechPluvio[i,j] <= 50){
      ColDry[i, j] <- paletteDry[10]
    } else if(SechPluvio[i, j] >= 50){
      ColDry[i, j] <- paletteDry[11]
    }
  }
}

#Couleur seche pour les types sur les BV
ColDry_BV <- as.data.frame(matrix(NA, ncol = length(SechBV) , nrow = 7))
for(i in c(6:7)){
  for(j in c(1:length(SechBV))){
    if(SechBV[i,j] >= 0 & SechBV[i,j] < 5){
      ColDry_BV[i, j] <- paletteDry[1]
    } else if(SechBV[i, j] >= 5 & SechBV[i,j] < 10){
      ColDry_BV[i, j] <- paletteDry[2]
    } else if(SechBV[i, j] >= 10 & SechBV[i,j] < 15){
      ColDry_BV[i, j] <- paletteDry[3]
    } else if(SechBV[i, j] >= 15 & SechBV[i,j] < 20){
      ColDry_BV[i, j] <- paletteDry[4]
    } else if(SechBV[i, j] >= 20 & SechBV[i,j] < 25){
      ColDry_BV[i, j] <- paletteDry[5]
    } else if(SechBV[i, j] >= 25 & SechBV[i,j] < 30){
      ColDry_BV[i, j] <- paletteDry[6]
    } else if(SechBV[i, j] >= 30 & SechBV[i,j] < 35){
      ColDry_BV[i, j] <- paletteDry[7]
    } else if(SechBV[i, j] >= 35 & SechBV[i,j] < 40){
      ColDry_BV[i, j] <- paletteDry[8]
    } else if(SechBV[i, j] >= 40 & SechBV[i,j] < 45){
      ColDry_BV[i, j] <- paletteDry[9]
    } else if(SechBV[i, j] >= 45 & SechBV[i,j] <= 50){
      ColDry_BV[i, j] <- paletteDry[10]
    } else if(SechBV[i, j] >= 50){
      ColDry_BV[i, j] <- paletteDry[11]
    }
  }
}

pdf(paste0(DIR_GRAPHE,"Type_Periode_", Delta, ".pdf"), paper ="a4r", height=0, width=0)
layout(matrix(c(1, 1, 2, 3, 3,
                4, 4, 5, 6, 6), ncol = 5, byrow = TRUE))
Type <- c("ExWet", "VWet", "Wet", "Normal", "Dry", "VDry", "ExDry")
#ExWet
plot(Bvs_Real, col = as.character(ColWet_BV[1, ]), main = paste0(Type[1], " for SPI", Delta))
points(infoPluvio$coord, pch=21, col = "black", bg = as.character(ColWet[1,]), cex = 2)
#legend
plot(c(0, 100), c(0, 100), pch = "", axes = FALSE, xlab = "", ylab = "")
box("plot")
legend("center", legend = c("[0 - 4]", "[5 - 9]", "[10 - 14]", "[15 - 19]", "[20 - 24]",
                            "[25 - 29]","[30 - 34]","[35 - 39]","[40 - 44]","[45 - 49]",
                            "[50 - ["),
       fill = paletteWet[1:11], title = ("Number of months"),
       bty = "n", cex = 1.3)
#VWet
plot(Bvs_Real, col = as.character(ColWet_BV[2, ]), main = paste0(Type[2], " for SPI", Delta))
points(infoPluvio$coord, pch=21, col = "black", bg = as.character(ColWet[2,]), cex = 2)

#ExDry
plot(Bvs_Real, col = as.character(ColDry_BV[7, ]), main = paste0(Type[7], " for SPI", Delta))
points(infoPluvio$coord, pch=21, col = "black", bg = as.character(ColDry[7,]), cex = 2)
#legend
plot(c(0, 200), c(0, 100), pch = "", axes = FALSE, xlab = "", ylab = "")
box("plot")
legend("center", legend = c("[0 - 4]", "[5 - 9]", "[10 - 14]", "[15 - 19]", "[20 - 24]",
                            "[25 - 29]","[30 - 34]","[35 - 39]","[40 - 44]","[45 - 49]",
                            "[50 - ["),
       fill = paletteDry[1:11], title = ("Number of months"),
       bty = "n", cex = 1.3)
#VDry
plot(Bvs_Real, col = as.character(ColDry_BV[6, ]), main = paste0(Type[6], " for SPI", Delta))
points(infoPluvio$coord, pch=21, col = "black", bg = as.character(ColDry[6,]), cex = 2)

dev.off()
