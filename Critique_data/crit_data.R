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

for (i in c(1:length(infobv$code))){
  ##__Choix_Basin_ou_Boucles_Sur_Plusieurs_Basins_____________________________####
  BasinCode <- infobv$code[i]
  
  pdf(paste0(DIR_GRAPHE, "Crit_data_",BasinCode,".pdf"),paper="a4r",height=0,width=0)#nom de fichier
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
  load(paste0(DIR_DATA_INPUT, "Qsimule/Qsim_calagecut/Qsim_",BasinCode,".RData"))
  load(paste0(DIR_DATA_INPUT, "Qsimule/Qfinal/Q_",BasinCode,".RData"))
  
  Qobs <- zoo(BasinDataBis$TabObsQmm, as.Date(BasinDataBis$TabDatesR))
  plot(Qobs)
  lines(Qsim, col="red")
  lines(Qfinal, col="green")
  
  Pobs <- zoo(BasinDataBis$TabObsP, as.Date(BasinDataBis$TabDatesR))
  
  Qmonth <- fc.daily2monthly(Qobs, FUN = sum, threshold = 0.1)
  Pmonth <- fc.daily2monthly(Pobs, FUN = sum, threshold = 0.1)
  QsimM <- fc.daily2monthly(Qsim, FUN = sum, threshold = 0.1)
  QfinalM <- fc.daily2monthly(Qfinal, FUN = sum, threshold = 0.1)

  #Hydroplot
  hydroplot(Qobs, FUN = mean, pfreq = "dma", var.unit="mm")
  hydroplot(Pobs, FUN = mean, pfreq = "dma", var.unit="mm")
  hydroplot(Qsim, FUN = mean, pfreq = "dma", var.unit = "mm")
  hydroplot(Qfinal, FUN = mean, pfreq = "dma", var.unit = "mm")
  
  #Matrixplot Qobs
  deb <- format(index(Qmonth[1]), format="%Y")
  fin <- format(index(Qmonth[length(Qmonth)]), format = "%Y")
  namemois<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  
  QDF <- as.data.frame(matrix(coredata(Qmonth), ncol = 12, byrow = TRUE,
                              dimnames = list(c(deb:fin), namemois)))
  # Plotting the monthly flow values
  print(matrixplot(QDF, ColorRamp="Precipitation", 
                   main="Flow [mm/month]"))
  
  #Matrixplot Pobs
  deb <- format(index(Pmonth[1]), format="%Y")
  fin <- format(index(Pmonth[length(Pmonth)]), format = "%Y")
  namemois<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  
  PDF <- as.data.frame(matrix(coredata(Pmonth), ncol = 12, byrow = TRUE,
                              dimnames = list(c(deb:fin), namemois)))
  
  # Plotting the monthly precipitation values
  print(matrixplot(PDF, ColorRamp="Precipitation", 
                   main="Precipitation [mm/month]"))
  
  #Matrixplot Qsim
  deb <- format(index(QsimM[1]), format="%Y")
  fin <- format(index(QsimM[length(QsimM)]), format = "%Y")
  namemois<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  
  Qs <- as.data.frame(matrix(coredata(QsimM), ncol = 12, byrow = TRUE,
                              dimnames = list(c(deb:fin), namemois)))
  
  # Plotting the monthly precipitation values
  print(matrixplot(Qs, ColorRamp="Precipitation", 
                   main="Flow simule [mm/month]"))
  
  #Matrixplot Qsim
  deb <- format(index(QfinalM[1]), format="%Y")
  fin <- format(index(QfinalM[length(QfinalM)]), format = "%Y")
  namemois<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  
  Qs <- as.data.frame(matrix(coredata(QfinalM), ncol = 12, byrow = TRUE,
                             dimnames = list(c(deb:fin), namemois)))
  
  # Plotting the monthly precipitation values
  print(matrixplot(Qs, ColorRamp="Precipitation", 
                   main="Flow final [mm/month]"))
  
  dev.off()
}

