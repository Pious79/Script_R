##____________________________________________________________________________##
##  Tutorial Script to use fc.MainRunGR6J on a catchment with SAFRAN data     ##
##                                                                            ##
##  Florine Garcia - 20171026 - HowToUseMainRunGR6J                           ##
##____________________________________________________________________________##

#__Root_Directory___________________________________________________________####
# ---- si travail sur le serveur
User <- "Pierre"
# ---- si travail en local
Disc <- "C"

if (substr(version$os, 1, 5) == "linux") {
  DIR_ROOT <- paste0("/home/RHAX21/UTILISATEURS/", User, "/Workspace_R/")
} else {
  if (Disc == "X") {
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
DIR_DATA_INPUT       <- "DataInput/"
DIR_DATA_INPUT_RDATA <- paste0(DIR_DATA_INPUT, "bvRdata/")
DIR_DATA_OUTPUT      <- "DataOutput/"
DIR_FUNCTIONS        <- "Functions/"
DIR_GRAPHE           <- paste0(DIR_DATA_OUTPUT, "Graphes/")
DIR_FUNCTIONS_LOIEAU    <- "X:/UTILISATEURS/LOIEAU/Workspace_R/Functions"

##__Functions_______________________________________________________________####
for (FileName in list.files(DIR_FUNCTIONS, pattern = "\\.[Rr]$")) {
  source(file.path(DIR_FUNCTIONS, FileName))
}
for(FileName in list.files(DIR_FUNCTIONS_LOIEAU, pattern = "\\.[Rr]$")) {
  source(file.path(DIR_FUNCTIONS_LOIEAU, FileName))
}
rm(FileName); gc()

##__Packages________________________________________________________________####
library(airGR)
library(zoo)

##__infobv__________________________________________________________________####
## RData a creer en amont de ce script. Doit etre une liste contenant au moins
# les info suivantes : code [character] : code des Bvs
#                      surf [numeric] : surface des Bvs en km2
FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoBv/infobv_Real.RData")
load(FileInfobv); rm(FileInfobv); gc()

Dateend <- c("20061231", "20101231", "20101231", "20111231", "20101231", "20101231", "20091231")
Param <- as.data.frame(matrix(NA, ncol = 6, nrow = 7))

for (i in c(1:length(infobv$code))){
##__Choix_Basin_ou_Boucles_Sur_Plusieurs_Basins_____________________________####
BasinCode <- infobv$code[i]

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

##__Choix_Periodes__________________________________________________________####
## Choix des periodes : chauffe (pour initialisation des reservoirs) et run 
#   (pour calage et simulation)
P_WU_Beg <- format(BasinDataBis$TabDatesR[1], format = "%d/%m/%Y")
P_WU_End <- format(BasinDataBis$TabDatesR[730], format = "%d/%m/%Y")
P_R_Beg  <- format(BasinDataBis$TabDatesR[731], format = "%d/%m/%Y")
P_R_End  <- "31/12/2015"

##__Creation_de_InputsModel_pour_GR6J_sans_CemaNeige_(function_de_airGR)____####
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR6J,
                                 DatesR = BasinDataBis$TabDatesR,
                                 Precip = BasinDataBis$TabObsP,
                                 PotEvap = BasinDataBis$TabObsE)

##__Exemple_Application_en_calage___________________________________________####
## On cale sur toute la periode et on cale tous les parametres
BoolCrit <- rep(TRUE,
                length(BasinDataBis$TabDatesT[which(format(BasinData$TabDatesR, format = "%d/%m/%Y") == P_R_Beg):
                                                which(format(BasinData$TabDatesR, format = "%d/%m/%Y") == P_R_End)]))
BoolCrit[which(BasinDataBis$TabDatesT[which(format(BasinData$TabDatesR, format = "%d/%m/%Y") == P_R_Beg):
                                        which(format(BasinData$TabDatesR, format = "%d/%m/%Y") == P_R_End)] == Dateend[i]):length(BoolCrit)] <- FALSE
OutputsModel <- fc.MainRunGR6J(BasinDataBis, InputsModel,
                               P_WU = c(P_WU_Beg, P_WU_End),
                               P_R = c(P_R_Beg, P_R_End), Appli = "Cal",
                               transfoFO = c("Q", "inv"), BoolCrit = BoolCrit,
                               FixedParam = c(NA, NA, NA, NA, NA, NA))
Qcal <- OutputsModel$OutputsModel_Cal$Qsim
plot(zoo(BasinData$TabObsQmm, as.Date(BasinData$TabDatesR)), ylab = "Q [mm/d]",
     xlab = "days")
lines(zoo(Qcal, as.Date(OutputsModel$OutputsModel_Cal$DatesR)), col = "red")

Qsim<-zoo(Qcal, as.Date(OutputsModel$OutputsModel_Cal$DatesR))
save(Qsim, file = paste0(DIR_DATA_OUTPUT,"Qsim_GR6J_cut_",infobv$code[i],".RData"))

Param[i, ] <- OutputsModel$Param
}
write.table(Param, file= paste0(DIR_DATA_OUTPUT,"Param_calage_cut_GR6J.csv"), sep = ";",
            row.names = FALSE)

## On cale sur toute la periode mais on bloque un (ou plusieurs) parametre(s)
#   quine sera (seront) pas cale(s)
OutputsModel <- fc.MainRunGR6J(BasinData, InputsModel,
                               P_WU = c(P_WU_Beg, P_WU_End),
                               P_R = c(P_R_Beg, P_R_End), Appli = "Cal",
                               transfoFO = c("Q", "inv"),
                               FixedParam = c(NA, NA, NA, 2, NA, NA))
Qcal <- OutputsModel$OutputsModel_Cal$Qsim
plot(zoo(BasinData$TabObsQmm, as.Date(BasinData$TabDatesR)), ylab = "Q [mm/d]",
     xlab = "days")
lines(zoo(Qcal, as.Date(OutputsModel$OutputsModel_Cal$DatesR)), col = "red")

##__Exemple_Application_en_simulation_______________________________________####
## On simulate sur toute la periode
OutputsModel <- fc.MainRunGR6J(BasinData, InputsModel,
                               P_WU = c(P_WU_Beg, P_WU_End),
                               P_R = c(P_R_Beg, P_R_End), Appli = "Sim",
                               FixedParam = c(174, -0.7, 13, 2, 0.4, 22))
Qsim <- OutputsModel$OutputsModel_Sim$Qsim
plot(zoo(BasinData$TabObsQmm, as.Date(BasinData$TabDatesR)), ylab = "Q [mm/d]",
     xlab = "days")
lines(zoo(Qsim, as.Date(OutputsModel$OutputsModel_Sim$DatesR)), col = "blue")

##__Exemple_Application_en_calage/validation_et_diff_tests_de_robustesse____####
## On cale sur une premiere sous-periode et on valide sur la seconde (creation
# d'un vecteur booleen a l'aide de CreateBoolCrit_Robustesse et on cale tous les
# parametres. P1 va du 01/01/1970 au 31/12/1991 et P2 va du 01/01/1992 au
# 31/12/2013.
# On a seulement besoin de connaitre la date de fin de P1
IndPeriod_Run <- which(format(BasinData$TabDatesR,
                              format = "%d/%m/%Y") == P_R_Beg):
  which(format(BasinData$TabDatesR, format = "%d/%m/%Y") == P_R_End)
for (PER in c("P1", "P2")) {
  BoolCrit <- CreateBoolCrit_Robustesse(DatesT = BasinData$TabDatesT[IndPeriod_Run],
                                        Qobs = BasinData$TabObsQmm[IndPeriod_Run],
                                        per = PER, Day = "31/12/1991",
                                        DatesR = BasinData$TabDatesR[IndPeriod_Run])
  OutputsModel <- fc.MainRunGR6J(BasinData, InputsModel,
                                 P_WU = c(P_WU_Beg, P_WU_End),
                                 P_R = c(P_R_Beg, P_R_End), Appli = "CalVal",
                                 transfoFO = c("Q", "inv"), BoolCrit = BoolCrit,
                                 FixedParam = c(NA, NA, NA, NA, NA, NA))
  Qcal <- OutputsModel$OutputsModel_Cal$OutputsModel_Cal$Qsim[BoolCrit]
  DatesCal <- OutputsModel$OutputsModel_Cal$OutputsModel_Cal$DatesR[BoolCrit]
  Qsim <- OutputsModel$OutputsModel_Sim$OutputsModel_Sim$Qsim[!BoolCrit]
  DatesSim <- OutputsModel$OutputsModel_Sim$OutputsModel_Sim$DatesR[!BoolCrit]
  Qobs <- BasinData$TabObsQmm[IndPeriod_Run]
  DatesObs <- BasinData$TabDatesR[IndPeriod_Run]
  plot(zoo(Qobs, as.Date(DatesObs)), ylab = "Q [mm/d]", xlab = "days")
  lines(zoo(Qcal, as.Date(DatesCal)), col = "red")
  lines(zoo(Qsim, as.Date(DatesSim)), col = "blue")
}


## On cale sur les annees humides et on valide sur les seches (creation
# d'un vecteur booleen a l'aide de CreateBoolCrit_Robustesse et on cale tous les
# parametres.
# On a seulement besoin de connaitre les donnees Ptot et ETP
IndPeriod_Run <- which(format(BasinData$TabDatesR,
                              format = "%d/%m/%Y") == P_R_Beg):
  which(format(BasinData$TabDatesR, format = "%d/%m/%Y") == P_R_End)
for (PER in c("wet", "dry")) {
  BoolCrit <- CreateBoolCrit_Robustesse(DatesT = BasinData$TabDatesT[IndPeriod_Run],
                                        Qobs = BasinData$TabObsQmm[IndPeriod_Run],
                                        per = PER, Times = "daily",
                                        Ptot = (BasinData$TabObsP[IndPeriod_Run] +
                                                  BasinData$TabObsN[IndPeriod_Run]),
                                        ETP = BasinData$TabObsE[IndPeriod_Run],
                                        DatesR = BasinData$TabDatesR[IndPeriod_Run])
  OutputsModel <- fc.MainRunGR6J(BasinData, InputsModel,
                                 P_WU = c(P_WU_Beg, P_WU_End),
                                 P_R = c(P_R_Beg, P_R_End), Appli = "CalVal",
                                 transfoFO = c("Q", "inv"), BoolCrit = BoolCrit,
                                 FixedParam = c(NA, NA, NA, NA, NA, NA))
  Qcal <- OutputsModel$OutputsModel_Cal$OutputsModel_Cal$Qsim[BoolCrit]
  DatesCal <- OutputsModel$OutputsModel_Cal$OutputsModel_Cal$DatesR[BoolCrit]
  Qsim <- OutputsModel$OutputsModel_Sim$OutputsModel_Sim$Qsim[!BoolCrit]
  DatesSim <- OutputsModel$OutputsModel_Sim$OutputsModel_Sim$DatesR[!BoolCrit]
  Qobs <- BasinData$TabObsQmm[IndPeriod_Run]
  DatesObs <- BasinData$TabDatesR[IndPeriod_Run]
  plot(zoo(Qobs, as.Date(DatesObs)), ylab = "Q [mm/d]", xlab = "days")
  lines(zoo(Qcal, as.Date(DatesCal)), col = "red")
  lines(zoo(Qsim, as.Date(DatesSim)), col = "blue")
}


## On cale une annee sur 2 et on valide sur celles n'ayant pas servies (creation
# d'un vecteur booleen a l'aide de CreateBoolCrit_Robustesse et on cale tous les
# parametres.
# On a seulement besoin de connaitre les donnees des DatesT. "odd" signigie
# annee impaire (en terme d'indice, si 1958 est la premiere annee de la
# chronique, alors elle est consideree comme impaire) et "even" signifie annee
# paire
IndPeriod_Run <- which(format(BasinData$TabDatesR,
                              format = "%d/%m/%Y") == P_R_Beg):
  which(format(BasinData$TabDatesR, format = "%d/%m/%Y") == P_R_End)
for (PER in c("odd", "even")) {
  BoolCrit <- CreateBoolCrit_Robustesse(DatesT = BasinData$TabDatesT[IndPeriod_Run],
                                        Qobs = BasinData$TabObsQmm[IndPeriod_Run],
                                        per = PER)
  OutputsModel <- fc.MainRunGR6J(BasinData, InputsModel,
                                 P_WU = c(P_WU_Beg, P_WU_End),
                                 P_R = c(P_R_Beg, P_R_End), Appli = "CalVal",
                                 transfoFO = c("Q", "inv"), BoolCrit = BoolCrit,
                                 FixedParam = c(NA, NA, NA, NA, NA, NA))
  Qcal <- OutputsModel$OutputsModel_Cal$OutputsModel_Cal$Qsim[BoolCrit]
  DatesCal <- OutputsModel$OutputsModel_Cal$OutputsModel_Cal$DatesR[BoolCrit]
  Qsim <- OutputsModel$OutputsModel_Sim$OutputsModel_Sim$Qsim[!BoolCrit]
  DatesSim <- OutputsModel$OutputsModel_Sim$OutputsModel_Sim$DatesR[!BoolCrit]
  Qobs <- BasinData$TabObsQmm[IndPeriod_Run]
  DatesObs <- BasinData$TabDatesR[IndPeriod_Run]
  plot(zoo(Qobs, as.Date(DatesObs)), ylab = "Q [mm/d]", xlab = "days")
  lines(zoo(Qcal, as.Date(DatesCal)), col = "red")
  lines(zoo(Qsim, as.Date(DatesSim)), col = "blue")
}
