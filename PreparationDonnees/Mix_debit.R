##____________________________________________________________________________##
##  Script to mix Qsim and Qobs to have a complete series                     ##
##                                                                            ##
##  Pierre L'Hermite - 20171024 - Mix_Debit.R                      ##
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
library(zoo)
library(lattice)

##__contenu_script__________________________________________________________####
FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoBv/infobv_Real.RData")
load(FileInfobv); rm(FileInfobv); gc()

for (i in c(1: length(infobv$code))){
##__Choix_Basin_ou_Boucles_Sur_Plusieurs_Basins_____________________________####
BasinCode <- infobv$code[i]

## RData a creer en amont de ce script, voir script Prep_BasinData.R
load(paste0(DIR_DATA_INPUT_RDATA, "BasinData_Real/BasinData_",
            BasinCode, ".RData"))

## Chargement du débit simulé long
load(paste0(DIR_DATA_INPUT, "Qsimule/Qsim_calagecut/Qsim_",BasinCode,".RData"))

Qobs <- zoo(BasinData$TabObsQmm, as.Date(BasinData$TabDatesR))

Dateend <- c("2006-12-31", "2010-12-31", "2010-12-31", "2011-12-31",
             "2010-12-31", "2010-12-31", "2009-12-31")

Deb <- which(index(Qobs) == index(Qsim[1]))
Fin <- which(index(Qobs) == Dateend[i])

Ind_change <- which(is.na(Qobs[Deb : Fin]))
Qfinal <- Qobs[Deb : Fin]
Qfinal[Ind_change] <- Qsim[Ind_change]

Deb2 <- which(index(Qsim) == Dateend[i])
Deb2 <- Deb2+1
Fin2 <- which(index(Qsim) == end(Qsim)) # ou "2015-12-31

Qfinal <- c(Qfinal, Qsim[Deb2: Fin2])

save( Qfinal, file = paste0(DIR_DATA_OUTPUT, "Q_", BasinCode, ".RData"))
}