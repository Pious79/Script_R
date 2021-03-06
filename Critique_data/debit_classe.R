##____________________________________________________________________________##
##  Script to critical data                                                   ##
##                                                                            ##
##  Pierre L'Hermite - 20171020 - Crit_data.R                                 ##
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
library(hydroTSM)
library(zoo)
library(lattice)

##__contenu_script__________________________________________________________####
FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoBv/infobv_Real.RData")
load(FileInfobv); rm(FileInfobv); gc()

Dateend <- c("20061231", "20101231", "20101231", "20111231", "20101231", "20101231", "20091231")
for (i in c(1:length(infobv$code))){
  ##__Choix_Basin_ou_Boucles_Sur_Plusieurs_Basins_____________________________####
  BasinCode <- infobv$code[i]
  
  pdf(paste0(DIR_GRAPHE, "DC_",BasinCode,".pdf"),paper="a4r",height=0,width=0)#nom de fichier
  #par(mfrow=c(2,1))
  
  ##__BasinData_______________________________________________________________####
  ## RData a creer en amont de ce script, voir script Prep_BasinData.R
  load(paste0(DIR_DATA_INPUT_RDATA, "BasinData_Real/BasinData_",
              BasinCode, ".RData"))
  
  ##Creation d'un basindatabis avec des vecteurs sans NA
  Ind_run <- which(!is.na(BasinData$TabObsP))
  BasinDataBis <- list( BasinCode = BasinData$BasinCode, BasinName = BasinData$BasinName,
                        BasinSurf = BasinData$BasinSurf, TabDatesT = BasinData$TabDatesT[Ind_run],
                        TabDatesR = BasinData$TabDatesR[Ind_run], TabObsQm3s = BasinData$TabObsQm3s[Ind_run],
                        TabObsQmm = BasinData$TabObsQmm[Ind_run], TabObsP = BasinData$TabObsP[Ind_run],
                        TabObsPsafran = BasinData$TabObsPsafran[Ind_run], TabObsN = BasinData$TabObsN[Ind_run],
                        TabObsT = BasinData$TabObsT[Ind_run], TabObsE = BasinData$TabObsE[Ind_run],
                        TabDatesRM = BasinData$TabDatesRM, TabObsQmmM = BasinData$TabObsQmmM)
  
  #Preparation donnee
  Qobs <- zoo(BasinDataBis$TabObsQmm , as.Date(BasinDataBis$TabDatesR))
  Qobs <- Qobs[which(BasinDataBis$TabDatesT == "19680101"):which(BasinDataBis$TabDatesT == Dateend[i])]
  load(paste0(DIR_DATA_INPUT, "Qsimule/Qsim_calagecut/Qsim_",BasinCode,".RData"))
  load(paste0(DIR_DATA_INPUT, "Qsimule/Qfinal/Q_",BasinCode,".RData"))
  Qsim <- Qsim[1:which(time(Qsim) == as.Date(paste0(substr(Dateend[i], 1, 4), "/",
                                                    substr(Dateend[i], 5, 6), "/",
                                                    substr(Dateend[i], 7, 8))))]
  Qsim[is.na(Qobs)] <- NA
  Qan <- fc.daily2annual(Qsim, debYear = "01-01", FUN = mean)
  DC <- fc.daily2DCx(Qsim, Qan, debYear = "01-01", quant = c(1:100), logi.plot = TRUE)
  
  Qan2 <- fc.daily2annual(Qobs, debYear = "01-01", FUN = mean)
  DC2 <- fc.daily2DCx(Qobs, Qan2, debYear = "01-01", quant = c(1:100), logi.plot = TRUE)
  
  # Qobs[Qobs == 0] <- 0.000001; Qsim[Qsim == 0] <- 0.000001
  FDC <- fdc(data.frame(Qobs = Qobs, Qsim = Qsim), plot = FALSE)
  plot(sort(FDC[, 1] * 100, decreasing = TRUE), sort(coredata(Qobs), decreasing = FALSE),
       type = "l", lwd = 2, xlab = "% Time flow equalled or exceeded", ylab = "Q",
       main = "(b) FDC", cex.main = 1.3, cex.lab = 1.2, cex.axis = 1.1)
  lines(sort(FDC[, 2] * 100, decreasing = TRUE), sort(coredata(Qsim), decreasing = FALSE),
        col = 2)
  dev.off()
}
