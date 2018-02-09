##____________________________________________________________________________##
##  Script to criticise rain data                                             ##
##  Pierre L'HERMITE - 2017-10-12 - Crit_data_Pluvio.R                        ##
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

##__contenu_script__________________________________________________________####

FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoPluvio/infoPluvio_Real.RData")
load(FileInfobv); rm(FileInfobv); gc()

pdf(paste0(DIR_GRAPHE, "Crit_Data_Pluie.pdf"), paper = "a4r", height = 0, width = 0)
for(iPluvio in 1:length(infoPluvio$code)){
  ##__Choix_Basin_ou_Boucles_Sur_Plusieurs_Basins___________________________####
  PluvioCode <- infoPluvio$code[iPluvio]
  
  ##__BasinData_____________________________________________________________####
  ## RData a creer en amont de ce script, voir script Prep_BasinData.R
  load(paste0(DIR_DATA_INPUT_RDATA, "PluvioData_Real/PluvioData_",
              PluvioCode, ".RData"))
  
  Pobs <- zoo(PluvioData$TabObsP, as.Date(PluvioData$TabDatesR))
  Pcomp <- zoo(PluvioData$TabcompleteP, as.Date(PluvioData$TabDatesR))
  
  plot(Pcomp, col="red",
       main = paste0("Pluie observée et complétée sur la chronique entière a ", infoPluvio$nom[iPluvio]))
  lines(Pobs)
  legend("topleft", legend = c("Pobservee", "Pcompletee"), lty = 1, bty = "n",
         cex = 1, col = c("black", "red"))
  
  ## Hydroplot ##
  hydroplot(Pobs, FUN = mean, pfreq = "dma", var.unit="mm",
            main = paste0("Pluie observee a ", infoPluvio$nom[iPluvio]))
  hydroplot(Pcomp, FUN = mean, pfreq = "dma", var.unit="mm",
            main = paste0("Pluie completee a ", infoPluvio$nom[iPluvio]))
  
  ## Matrix plot ##
  POm <- fc.daily2monthly(Pobs, FUN = sum, threshold = 0.1)
  POcomp <- fc.daily2monthly(Pcomp, FUN = sum, threshold = 0.1)
  
  deb <- format(index(POm[1]), format="%Y")
  fin <- format(index(POm[length(POm)]), format = "%Y")
  fin <- as.numeric(fin) - 1
  namemois<-c("Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul")
  
  QDFobs <- as.data.frame(matrix(coredata(POm), ncol = 12, byrow = TRUE,
                                 dimnames = list(c(deb:fin), namemois)))
  # Plotting the monthly flow values
  print(matrixplot(QDFobs, ColorRamp="Precipitation", 
                   main="Pluie observee [mm/month]"))
  
  deb <- format(index(POcomp[1]), format="%Y")
  fin <- format(index(POcomp[length(POcomp)]), format = "%Y")
  fin <- as.numeric(fin) - 1
  namemois<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  
  QDFcomp <- as.data.frame(matrix(coredata(POcomp), ncol = 12, byrow = TRUE,
                                  dimnames = list(c(deb:fin), namemois)))
  # Plotting the monthly flow values
  print(matrixplot(QDFcomp, ColorRamp="Precipitation", 
                   main="Pluie completee [mm/month]"))
}
dev.off()
