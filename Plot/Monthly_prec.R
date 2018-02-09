##______________________________________________________________________________##
##  Script to plot monthly precipitation                                        ##
##  Pierre L'HERMITE - 2017-12-01 - Monthly_prec.R                              ##
##______________________________________________________________________________##

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
DIR_FUNCTIONS           <- "Functions/"
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
library(hydroTSM)
library(zoo)

##__contenu_script__________________________________________________________####
FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoPluvio/infoPluvio_Real.RData")
load(FileInfobv); rm(FileInfobv); gc()

pdf(paste0(DIR_GRAPHE,"Monthly/Pluie_mensuelle.pdf"), paper ="a4r",
    height=0, width=0)

for (iPluvio in c(1:length(infoPluvio$code))){
  ##__Choix_Basin_ou_Boucles_Sur_Plusieurs_Basins_____________________________####
  PluvioCode <- infoPluvio$code[iPluvio]
  
  ##__BasinData_______________________________________________________________####
  ## RData a creer en amont de ce script, voir script Prep_BasinData.R
  load(paste0(DIR_DATA_INPUT_RDATA, "PluvioData_Real/PluvioData_",
              PluvioCode, ".RData"))
  
  #Passage des données journalières au mensuelles
  Pday <- zoo(PluvioData$TabcompleteP, order.by = as.Date(PluvioData$TabDatesR))
  datam <- fc.daily2monthly(Pday, FUN = sum)
  
  moy <- mean(datam, na.rm = TRUE)
  moyl <- rep(moy, length(datam))
  
  datatest <- datam
  
  #extraire annee
  ystart<-as.numeric(format(start(datatest), format="%Y"))
  yend<-as.numeric(format(end(datatest), format="%Y"))
  
  #Modification pour le plot
  coredata(datatest)[is.na(coredata(datatest))] <- moy
  indexlabel <- format(index(datatest), format = "%Y-%m")
  datatest <- c(moy, coredata(datatest), moy)
  
  #Plot avec la couleur selon la valeur
  plot(datatest, type="l", xaxt="n", ylim = c(round(min(coredata(datatest),
                                                        na.rm=T),5),
                                              round(max(coredata(datatest),
                                                        na.rm=T),5)),
       xlab = "Temps", ylab = "Precipitations [mm/month]",
       main = paste0("Precipitations a ",infoPluvio$nom[iPluvio]," de ",ystart," a ",yend,
                     " avec la regression de Sen en rouge
                     et l'annee possible de rupture selon Pettitt en bleu"),
       cex.main=0.9, cex.lab=1, cex.axis=1)
  axis(1, at=seq(1, length(datatest), 60),
       labels=indexlabel[seq(1, length(datatest), 60)])
  datatest.pos <- ifelse(coredata(datatest) > moy, datatest, moy)
  datatest.neg <- ifelse(coredata(datatest) <= moy, datatest, moy)
  polygon(datatest.pos, col = "lightseagreen", border = NA)
  polygon(datatest.neg, col = "red3", border = NA)
  lines(moyl, col = "black")
}
dev.off()