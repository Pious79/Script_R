##____________________________________________________________________________##
##  Script to complete rain data from Antilope                                ##
##  Pierre L'HERMITE - 2017-11-07 - Pluie_antilope_complete.R                 ##
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
library(zoo)
library(dplR)

##__contenu_script__________________________________________________________####
NAO <- read.table("C:/Users/pierre.lhermite/Documents/Pierre/norm.nao.monthly.b5001.current.ascii.txt",
           header = FALSE, sep ="")

datae <- seq.Date(as.Date("1950-01-01"), as.Date("2017-12-01"), "months")
NAO <- zoo(as.numeric(NAO$V3), datae)
fc.plot_MK_Sen_SQMK_Pettitt(NAO)

# Tab<-data.frame(index(NAO), coredata(NAO))
# colnames(Tab)<-c("Date","Donnee")
# wt<-analyze.wavelet(Tab, 2, dt=1, loess.span = 0, upperPeriod = 32)
# wt.avg(wt, siglvl = c(0), show.legend = F , averagelab = "Average Power Spectrum",
#        periodlab = "Period", main="Spectre de puissance des d?bits\nmensuels simul?s sur GR4J")
# wt.image(wt, plot.contour = FALSE, plot.ridge = FALSE)

MOR <- morlet(coredata(NAO), as.numeric(index(NAO)), siglvl = 0.95)
wavelet.plot(MOR)

FileInfoPluvio <- paste0(DIR_DATA_INPUT_RDATA, "InfoPluvio/infoPluvio_Real.RData")
load(FileInfoPluvio); rm(FileInfoPluvio); gc()
##__Choix_Basin_ou_Boucles_Sur_Plusieurs_Basins_____________________________####
PluvioCode <- infoPluvio$code[iBV]
##__BasinData_______________________________________________________________####
## RData a creer en amont de ce script, voir script Prep_BasinData.R
load(paste0(DIR_DATA_INPUT_RDATA, "PluvioData_Real/PluvioData_",
            PluvioCode, ".RData"))
Ppluvio <- fc.daily2monthly(zoo(PluvioData$TabcompleteP, PluvioData$TabDatesR),
                            FUN = sum, na.rm = TRUE, threshold = 0.1)
MORpluie <- morlet(coredata(Ppluvio), as.numeric(index(Ppluvio)), siglvl = 0.95)
wavelet.plot(MORpluie)
