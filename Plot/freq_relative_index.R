##____________________________________________________________________________##
##  Script to plot relative frequency of different type                       ##
##                                                                            ##
##  Pierre L'Hermite - 20171211 - Freq_relative_index.R                       ##
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
library(hydroTSM)

##__contenu_script__________________________________________________________####

FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoPluvio/infoPluvio_Real.RData")
load(FileInfobv); rm(FileInfobv); gc()

nom <- infoPluvio$nom[c(12, 14, 3, 5)]
Type <- c("Extr\nhumide", "Tres\nhumide", "Humide", "Normal", "Sec",
          "Tres\nsec", "Extr\nsec")

pdf(paste0(DIR_GRAPHE,"Freq_relative.pdf"), paper ="a4r", height=0, width=0)
layout(matrix(c(1, 2,
                3, 4), ncol = 2, byrow = TRUE))
#Frequence relative SPI 12
load(paste0(DIR_GRAPHE,"SPI_Pluvio/Sech_SPI_12.RData"))

freq <- matrix(NA, nrow = 5, ncol = 7)
kok <- 1
color <- c("Black", "red", "lightblue", "green")
for (imat in c(12, 14, 3, 5)){
  freq[kok, ] <- (as.numeric(Sech[imat, ])/sum(Sech[imat, ]))
  kok <- kok + 1
}

barplot(freq, beside = TRUE, ylim = c(0,0.8))
grid(nx = NA, ny = 8, col="grey")
box()
barplot(freq, beside = TRUE, col = color, ylim = c(0,0.8), legend = nom,
        xlab = "Type de periode", names.arg = Type, add= TRUE,
        main = "Frequence relative des periodes seches et humides\nselon le SPI",
        ylab = "Frequence relative", cex.names = 1)

#Frequence relative PNI 12
load(paste0(DIR_GRAPHE,"PNI_Pluvio/Sech_PNI_12.RData"))

freq <- matrix(NA, nrow = 5, ncol = 7)
kok <- 1
color <- c("Black", "red", "lightblue", "green")
for (imat in c(12, 14, 3, 5)){
  freq[kok, ] <- (as.numeric(Sech[imat, ])/sum(Sech[imat, ]))
  kok <- kok + 1
}

barplot(freq, beside = TRUE, ylim = c(0,0.8))
grid(nx = NA, ny = 8, col="grey")
box()
barplot(freq, beside = TRUE, col = color, ylim = c(0,0.8), legend = nom,
        xlab = "Type de periode", names.arg = Type, add= TRUE,
        main = "Frequence relative des periodes seches et humides\nselon le PNI",
        ylab = "Frequence relative", cex.names = 1)

#Frequence relative ISSP
load(paste0(DIR_GRAPHE,"PITA/Sech_ISSP.RData"))

freq <- matrix(NA, nrow = 5, ncol = 7)
kok <- 1
color <- c("Black", "red", "lightblue", "green")
for (imat in c(12, 14, 3, 5)){
  freq[kok, ] <- (as.numeric(Sech[imat, ])/sum(Sech[imat, ]))
  kok <- kok + 1
}

barplot(freq, beside = TRUE, ylim = c(0,0.8))
grid(nx = NA, ny = 8, col="grey")
box()
barplot(freq, beside = TRUE, col = color, ylim = c(0,0.8), legend = nom,
        xlab = "Type de periode", names.arg = Type, add= TRUE,
        main = "Frequence relative des periodes seches et humides\nselon l'ISSP",
        ylab = "Frequence relative", cex.names = 1)

#Frequence relative RAI 12
load(paste0(DIR_GRAPHE,"RAI_Pluvio/Sech_RAI_12.RData"))

freq <- matrix(NA, nrow = 5, ncol = 7)
kok <- 1
color <- c("Black", "red", "lightblue", "green")
for (imat in c(12, 14, 3, 5)){
  freq[kok, ] <- (as.numeric(Sech[imat, ])/sum(Sech[imat, ]))
  kok <- kok + 1
}

barplot(freq, beside = TRUE, ylim = c(0,0.8))
grid(nx = NA, ny = 8, col="grey")
box()
barplot(freq, beside = TRUE, col = color, ylim = c(0,0.8), legend = nom,
        xlab = "Type de periode", names.arg = Type, add= TRUE,
        main = "Frequence relative des periodes seches et humides\nselon le RAI",
        ylab = "Frequence relative", cex.names = 1)
dev.off()
