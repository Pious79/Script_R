##____________________________________________________________________________##
##  Tutorial Script to use fc.MainRunGR4J on a catchment with SAFRAN data     ##
##                                                                            ##
##  Florine Garcia - 20170816 - HowToUseMainRunGR4J                           ##
##  Script modifie le 20171017 par Florine Garcia :                           ##
##    - integration des changements du package airGR (version 1.0)            ##
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
library(airGR); library(hydroGOF)
library(zoo)

##__infobv__________________________________________________________________####
## RData a creer en amont de ce script. Doit etre une liste contenant au moins
# les info suivantes : code [character] : code des Bvs
#                      surf [numeric] : surface des Bvs en km2
FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoBv/infobv_Real.RData")
load(FileInfobv); rm(FileInfobv); gc()

Dateend <- c("20061231", "20071231", "20071231", "20071231", "20071231",
             "20071231", "20071231")
Dateend2 <- c("31/12/2006", "31/12/2007", "31/12/2007", "31/12/2007",
              "31/12/2007", "31/12/2007", "31/12/2007")

names_Recap <- c("Per", paste0("X", 1:4), "FO", "C2M_Q", "C2M_sqrtQ", "C2M_invQ")
Recap_ToutCal <- as.data.frame(matrix(NA, ncol = 9, nrow = 7,
                                      dimnames = list(infobv$code, names_Recap)))
Recap_Cal2Per <- Recap_Sim2Per <- Recap_CalWD <- Recap_SimWD <-
  Recap_CalO <- Recap_SimO <-
  as.data.frame(matrix(NA, ncol = 9, nrow = 14,
                       dimnames = list(c(paste0(infobv$code, "_1"),
                                         paste0(infobv$code, "_2")),
                                       names_Recap)))

for (i in c(1:length(infobv$code))){
  ##__Choix_Basin_ou_Boucles_Sur_Plusieurs_Basins___________________________####
  BasinCode <- infobv$code[i]
  
  ##__BasinData_____________________________________________________________####
  ## RData a creer en amont de ce script, voir script Prep_BasinData.R
  load(paste0(DIR_DATA_INPUT_RDATA, "BasinData_Real/BasinData_",
              BasinCode, ".RData"))
  
  ##Creation d'un basindatabis avec des vecteurs sans NA
  Ind_run <- which(!is.na(BasinData$TabObsP))
  BasinDataBis <- list(BasinCode = BasinData$BasinCode,
                       BasinName = BasinData$BasinName,
                       BasinSurf = BasinData$BasinSurf,
                       TabDatesT = BasinData$TabDatesT[Ind_run],
                       TabDatesR = BasinData$TabDatesR[Ind_run],
                       TabObsQm3s = BasinData$TabObsQm3s[Ind_run],
                       TabObsQmm = BasinData$TabObsQmm[Ind_run],
                       TabObsP = BasinData$TabObsP[Ind_run],
                       TabObsPsafran = BasinData$TabObsPsafran[Ind_run],
                       TabObsN = BasinData$TabObsN[Ind_run],
                       TabObsT = BasinData$TabObsT[Ind_run],
                       TabObsE = BasinData$TabObsE[Ind_run],
                       TabDatesRM = BasinData$TabDatesRM,
                       TabObsQmmM = BasinData$TabObsQmmM)
  ##__Choix_Periodes________________________________________________________####
  ## Choix des periodes : chauffe (pour initialisation des reservoirs) et run 
  #   (pour calage et simulation)
  P_WU_Beg <- format(BasinDataBis$TabDatesR[1], format = "%d/%m/%Y")
  P_WU_End <- format(BasinDataBis$TabDatesR[730], format = "%d/%m/%Y")
  P_R_Beg  <- format(BasinDataBis$TabDatesR[731], format = "%d/%m/%Y")
  P_R_End  <- Dateend2[i] #"31/12/2015"
  
  ##__Creation_de_InputsModel_pour_GR4J_sans_CemaNeige_(function_de_airGR)__####
  InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4J,
                                   DatesR = BasinDataBis$TabDatesR,
                                   Precip = BasinDataBis$TabObsP,
                                   PotEvap = BasinDataBis$TabObsE)
  
  ##__Exemple_Application_en_calage_________________________________________####
  ## On cale sur toute la periode et on cale tous les parametres
  BoolCrit <- rep(TRUE,
                  length(which(format(BasinDataBis$TabDatesR, format = "%d/%m/%Y") == P_R_Beg):
                           which(format(BasinDataBis$TabDatesR, format = "%d/%m/%Y") == P_R_End)))
  if(P_R_End > Dateend2[i]) {
    BoolCrit[(which(format(BasinDataBis$TabDatesR[which(format(BasinDataBis$TabDatesR, format = "%d/%m/%Y") == P_R_Beg):
                                                    which(format(BasinDataBis$TabDatesR, format = "%d/%m/%Y") == P_R_End)],
                           format = "%d/%m/%Y") == Dateend2[i])+1):length(BoolCrit)] <- FALSE
  }
  OutputsModel <- fc.MainRunGR4J(BasinDataBis, InputsModel,
                                 P_WU = c(P_WU_Beg, P_WU_End),
                                 P_R = c(P_R_Beg, P_R_End), Appli = "Cal",
                                 transfoFO = c("Q", "inv"), BoolCrit = BoolCrit,
                                 FixedParam = c(NA, NA, NA, NA))
  Qcal <- OutputsModel$OutputsModel_Cal$Qsim
  Qobs <- BasinDataBis$TabObsQmm[which(format(BasinDataBis$TabDatesR, format = "%d/%m/%Y") == P_R_Beg):
                                   which(format(BasinDataBis$TabDatesR, format = "%d/%m/%Y") == P_R_End)]
  meanQobs <- mean(Qobs, na.rm = TRUE); epsilon <- 1 / 100 * meanQobs
  plot(zoo(Qobs, as.Date(OutputsModel$OutputsModel_Cal$DatesR)), ylab = "Q [mm/d]",
       xlab = "days", ylim = c(0, 80))
  lines(zoo(Qcal, as.Date(OutputsModel$OutputsModel_Cal$DatesR)), col = "red")
  
  Qsim<-zoo(Qcal, as.Date(OutputsModel$OutputsModel_Cal$DatesR))
  save(Qsim, file = paste0(DIR_DATA_OUTPUT,"Qsim_",infobv$code[i],".RData"))
  
  ## Calcul tableau recap
  Recap_ToutCal[i, 1] <- "AllPer"
  Recap_ToutCal[i, 2:5] <- OutputsModel$Param
  Recap_ToutCal$FO[i] <- mean(KGE(Qcal, Qobs, na.rm = TRUE),
                              KGE(1 / (Qcal + epsilon), 1 / (Qobs + epsilon),
                                  na.rm = TRUE))
  Recap_ToutCal$C2M_Q[i] <- C2M(Qcal, Qobs, na.rm = TRUE)
  Recap_ToutCal$C2M_sqrtQ[i] <- C2M(sqrt(Qcal), sqrt(Qobs), na.rm = TRUE)
  Recap_ToutCal$C2M_invQ[i] <- C2M(1 / (Qcal + epsilon), 1 / (Qobs + epsilon),
                                   na.rm = TRUE)
  
  ##__Exemple_Application_en_calage/validation_et_diff_tests_de_robustesse____####
  ## On cale sur une premiere sous-periode et on valide sur la seconde (creation
  # d'un vecteur booleen a l'aide de CreateBoolCrit_Robustesse et on cale tous les
  # parametres. P1 va du 01/01/1970 au 31/12/1991 et P2 va du 01/01/1992 au
  # 31/12/2013.
  # On a seulement besoin de connaitre la date de fin de P1
  IndPeriod_Run <- which(format(BasinDataBis$TabDatesR,
                                format = "%d/%m/%Y") == P_R_Beg):
    which(format(BasinDataBis$TabDatesR, format = "%d/%m/%Y") == P_R_End)
  for (PER in c("P1", "P2")) {
    DayCoup <- format(BasinDataBis$TabDatesR[IndPeriod_Run][length(IndPeriod_Run)/2+1],
                      format = "%d/%m/%Y")
    BoolCrit <- CreateBoolCrit_Robustesse(DatesT = BasinDataBis$TabDatesT[IndPeriod_Run],
                                          Qobs = BasinDataBis$TabObsQmm[IndPeriod_Run],
                                          per = PER, Day = DayCoup,
                                          DatesR = BasinDataBis$TabDatesR[IndPeriod_Run])
    OutputsModel <- fc.MainRunGR4J(BasinDataBis, InputsModel,
                                   P_WU = c(P_WU_Beg, P_WU_End),
                                   P_R = c(P_R_Beg, P_R_End), Appli = "CalVal",
                                   transfoFO = c("Q", "inv"), BoolCrit = BoolCrit,
                                   FixedParam = c(NA, NA, NA, NA))
    Qcal <- OutputsModel$OutputsModel_Cal$OutputsModel_Cal$Qsim[BoolCrit]
    # DatesCal <- OutputsModel$OutputsModel_Cal$OutputsModel_Cal$DatesR[BoolCrit]
    Qsim <- OutputsModel$OutputsModel_Sim$OutputsModel_Sim$Qsim[!BoolCrit]
    # DatesSim <- OutputsModel$OutputsModel_Sim$OutputsModel_Sim$DatesR[!BoolCrit]
    Qobs <- BasinDataBis$TabObsQmm[IndPeriod_Run]
    # DatesObs <- BasinDataBis$TabDatesR[IndPeriod_Run]
    # plot(zoo(Qobs, as.Date(DatesObs)), ylab = "Q [mm/d]", xlab = "days")
    # lines(zoo(Qcal, as.Date(DatesCal)), col = "red")
    # lines(zoo(Qsim, as.Date(DatesSim)), col = "blue")
    ## Calcul tableau recap
    if(PER == "P1") {
      ic <- i; PERs <- "P2"
    } else {
      ic <- i + 7; PERs <- "P1"
    }
    ## Resu calage
    Recap_Cal2Per[ic, 1] <- PER
    Recap_Cal2Per[ic, 2:5] <- OutputsModel$OutputsModel_Cal$Param
    epsilonC <- 1 / 100 * mean(Qobs[BoolCrit], na.rm = TRUE)
    Recap_Cal2Per$FO[ic] <- mean(KGE(Qcal, Qobs[BoolCrit], na.rm = TRUE),
                                 KGE(1 / (Qcal + epsilonC),
                                     1 / (Qobs[BoolCrit] + epsilonC),
                                     na.rm = TRUE))
    Recap_Cal2Per$C2M_Q[ic] <- C2M(Qcal, Qobs[BoolCrit], na.rm = TRUE)
    Recap_Cal2Per$C2M_sqrtQ[ic] <- C2M(sqrt(Qcal), sqrt(Qobs[BoolCrit]),
                                       na.rm = TRUE)
    Recap_Cal2Per$C2M_invQ[ic] <- C2M(1 / (Qcal + epsilonC),
                                      1 / (Qobs[BoolCrit] + epsilonC),
                                      na.rm = TRUE)
    ## Resu simulation
    Recap_Sim2Per[ic, 1] <- PERs
    Recap_Sim2Per[ic, 2:5] <- OutputsModel$OutputsModel_Cal$Param
    epsilonS <- 1 / 100 * mean(Qobs[!BoolCrit], na.rm = TRUE)
    Recap_Sim2Per$FO[ic] <- mean(KGE(Qsim, Qobs[!BoolCrit], na.rm = TRUE),
                                 KGE(1 / (Qsim + epsilonS),
                                     1 / (Qobs[!BoolCrit] + epsilonS),
                                     na.rm = TRUE))
    Recap_Sim2Per$C2M_Q[ic] <- C2M(Qsim, Qobs[!BoolCrit], na.rm = TRUE)
    Recap_Sim2Per$C2M_sqrtQ[ic] <- C2M(sqrt(Qsim), sqrt(Qobs[!BoolCrit]),
                                       na.rm = TRUE)
    Recap_Sim2Per$C2M_invQ[ic] <- C2M(1 / (Qsim + epsilonS),
                                      1 / (Qobs[!BoolCrit] + epsilonS),
                                      na.rm = TRUE)
  }
  
  
  ## On cale sur les annees humides et on valide sur les seches (creation
  # d'un vecteur booleen a l'aide de CreateBoolCrit_Robustesse et on cale tous les
  # parametres.
  # On a seulement besoin de connaitre les donnees Ptot et ETP
  IndPeriod_Run <- which(format(BasinDataBis$TabDatesR,
                                format = "%d/%m/%Y") == P_R_Beg):
    which(format(BasinDataBis$TabDatesR, format = "%d/%m/%Y") == P_R_End)
  for (PER in c("wet", "dry")) {
    BoolCrit <- CreateBoolCrit_Robustesse(DatesT = BasinDataBis$TabDatesT[IndPeriod_Run],
                                          Qobs = BasinDataBis$TabObsQmm[IndPeriod_Run],
                                          per = PER, Times = "daily",
                                          Ptot = (BasinDataBis$TabObsP[IndPeriod_Run] +
                                                    BasinDataBis$TabObsN[IndPeriod_Run]),
                                          ETP = BasinDataBis$TabObsE[IndPeriod_Run],
                                          DatesR = BasinDataBis$TabDatesR[IndPeriod_Run])
    OutputsModel <- fc.MainRunGR4J(BasinDataBis, InputsModel,
                                   P_WU = c(P_WU_Beg, P_WU_End),
                                   P_R = c(P_R_Beg, P_R_End), Appli = "CalVal",
                                   transfoFO = c("Q", "inv"), BoolCrit = BoolCrit,
                                   FixedParam = c(NA, NA, NA, NA))
    Qcal <- OutputsModel$OutputsModel_Cal$OutputsModel_Cal$Qsim[BoolCrit]
    # DatesCal <- OutputsModel$OutputsModel_Cal$OutputsModel_Cal$DatesR[BoolCrit]
    Qsim <- OutputsModel$OutputsModel_Sim$OutputsModel_Sim$Qsim[!BoolCrit]
    # DatesSim <- OutputsModel$OutputsModel_Sim$OutputsModel_Sim$DatesR[!BoolCrit]
    Qobs <- BasinDataBis$TabObsQmm[IndPeriod_Run]
    # DatesObs <- BasinDataBis$TabDatesR[IndPeriod_Run]
    # plot(zoo(Qobs, as.Date(DatesObs)), ylab = "Q [mm/d]", xlab = "days")
    # lines(zoo(Qcal, as.Date(DatesCal)), col = "red")
    # lines(zoo(Qsim, as.Date(DatesSim)), col = "blue")
    if(PER == "wet") {
      ic <- i; PERs <- "dry"
    } else {
      ic <- i + 7; PERs <- "wet"
    }
    ## Resu calage
    Recap_CalWD[ic, 1] <- PER
    Recap_CalWD[ic, 2:5] <- OutputsModel$OutputsModel_Cal$Param
    epsilonC <- 1 / 100 * mean(Qobs[BoolCrit], na.rm = TRUE)
    Recap_CalWD$FO[ic] <- mean(KGE(Qcal, Qobs[BoolCrit], na.rm = TRUE),
                               KGE(1 / (Qcal + epsilonC),
                                   1 / (Qobs[BoolCrit] + epsilonC),
                                   na.rm = TRUE))
    Recap_CalWD$C2M_Q[ic] <- C2M(Qcal, Qobs[BoolCrit], na.rm = TRUE)
    Recap_CalWD$C2M_sqrtQ[ic] <- C2M(sqrt(Qcal), sqrt(Qobs[BoolCrit]),
                                     na.rm = TRUE)
    Recap_CalWD$C2M_invQ[ic] <- C2M(1 / (Qcal + epsilonC),
                                    1 / (Qobs[BoolCrit] + epsilonC),
                                    na.rm = TRUE)
    ## Resu simulation
    Recap_SimWD[ic, 1] <- PERs
    Recap_SimWD[ic, 2:5] <- OutputsModel$OutputsModel_Cal$Param
    epsilonS <- 1 / 100 * mean(Qobs[!BoolCrit], na.rm = TRUE)
    Recap_SimWD$FO[ic] <- mean(KGE(Qsim, Qobs[!BoolCrit], na.rm = TRUE),
                               KGE(1 / (Qsim + epsilonS),
                                   1 / (Qobs[!BoolCrit] + epsilonS),
                                   na.rm = TRUE))
    Recap_SimWD$C2M_Q[ic] <- C2M(Qsim, Qobs[!BoolCrit], na.rm = TRUE)
    Recap_SimWD$C2M_sqrtQ[ic] <- C2M(sqrt(Qsim), sqrt(Qobs[!BoolCrit]),
                                     na.rm = TRUE)
    Recap_SimWD$C2M_invQ[ic] <- C2M(1 / (Qsim + epsilonS),
                                    1 / (Qobs[!BoolCrit] + epsilonS),
                                    na.rm = TRUE)
  }
  
  
  ## On cale une annee sur 2 et on valide sur celles n'ayant pas servies (creation
  # d'un vecteur booleen a l'aide de CreateBoolCrit_Robustesse et on cale tous les
  # parametres.
  # On a seulement besoin de connaitre les donnees des DatesT. "odd" signigie
  # annee impaire (en terme d'indice, si 1958 est la premiere annee de la
  # chronique, alors elle est consideree comme impaire) et "even" signifie annee
  # paire
  IndPeriod_Run <- which(format(BasinDataBis$TabDatesR,
                                format = "%d/%m/%Y") == P_R_Beg):
    which(format(BasinDataBis$TabDatesR, format = "%d/%m/%Y") == P_R_End)
  for (PER in c("odd", "even")) {
    BoolCrit <- CreateBoolCrit_Robustesse(DatesT = BasinDataBis$TabDatesT[IndPeriod_Run],
                                          Qobs = BasinDataBis$TabObsQmm[IndPeriod_Run],
                                          per = PER)
    OutputsModel <- fc.MainRunGR4J(BasinDataBis, InputsModel,
                                   P_WU = c(P_WU_Beg, P_WU_End),
                                   P_R = c(P_R_Beg, P_R_End), Appli = "CalVal",
                                   transfoFO = c("Q", "inv"), BoolCrit = BoolCrit,
                                   FixedParam = c(NA, NA, NA, NA))
    Qcal <- OutputsModel$OutputsModel_Cal$OutputsModel_Cal$Qsim[BoolCrit]
    # DatesCal <- OutputsModel$OutputsModel_Cal$OutputsModel_Cal$DatesR[BoolCrit]
    Qsim <- OutputsModel$OutputsModel_Sim$OutputsModel_Sim$Qsim[!BoolCrit]
    # DatesSim <- OutputsModel$OutputsModel_Sim$OutputsModel_Sim$DatesR[!BoolCrit]
    Qobs <- BasinDataBis$TabObsQmm[IndPeriod_Run]
    # DatesObs <- BasinDataBis$TabDatesR[IndPeriod_Run]
    # plot(zoo(Qobs, as.Date(DatesObs)), ylab = "Q [mm/d]", xlab = "days")
    # lines(zoo(Qcal, as.Date(DatesCal)), col = "red")
    # lines(zoo(Qsim, as.Date(DatesSim)), col = "blue")
    if(PER == "odd") {
      ic <- i; PERs <- "even"
    } else {
      ic <- i + 7; PERs <- "odd"
    }
    ## Resu calage
    Recap_CalO[ic, 1] <- PER
    Recap_CalO[ic, 2:5] <- OutputsModel$OutputsModel_Cal$Param
    epsilonC <- 1 / 100 * mean(Qobs[BoolCrit], na.rm = TRUE)
    Recap_CalO$FO[ic] <- mean(KGE(Qcal, Qobs[BoolCrit], na.rm = TRUE),
                              KGE(1 / (Qcal + epsilonC),
                                  1 / (Qobs[BoolCrit] + epsilonC),
                                  na.rm = TRUE))
    Recap_CalO$C2M_Q[ic] <- C2M(Qcal, Qobs[BoolCrit], na.rm = TRUE)
    Recap_CalO$C2M_sqrtQ[ic] <- C2M(sqrt(Qcal), sqrt(Qobs[BoolCrit]),
                                    na.rm = TRUE)
    Recap_CalO$C2M_invQ[ic] <- C2M(1 / (Qcal + epsilonC),
                                   1 / (Qobs[BoolCrit] + epsilonC),
                                   na.rm = TRUE)
    ## Resu simulation
    Recap_SimO[ic, 1] <- PERs
    Recap_SimO[ic, 2:5] <- OutputsModel$OutputsModel_Cal$Param
    epsilonS <- 1 / 100 * mean(Qobs[!BoolCrit], na.rm = TRUE)
    Recap_SimO$FO[ic] <- mean(KGE(Qsim, Qobs[!BoolCrit], na.rm = TRUE),
                              KGE(1 / (Qsim + epsilonS),
                                  1 / (Qobs[!BoolCrit] + epsilonS),
                                  na.rm = TRUE))
    Recap_SimO$C2M_Q[ic] <- C2M(Qsim, Qobs[!BoolCrit], na.rm = TRUE)
    Recap_SimO$C2M_sqrtQ[ic] <- C2M(sqrt(Qsim), sqrt(Qobs[!BoolCrit]),
                                    na.rm = TRUE)
    Recap_SimO$C2M_invQ[ic] <- C2M(1 / (Qsim + epsilonS),
                                   1 / (Qobs[!BoolCrit] + epsilonS),
                                   na.rm = TRUE)
  }
}
save(Recap_ToutCal, Recap_Cal2Per, Recap_Sim2Per, Recap_CalWD, Recap_SimWD,
     Recap_CalO, Recap_SimO,
     file = paste0(DIR_DATA_OUTPUT, "RobustesseGR4J/Recap_AnalyseRobustesseGR4J.RData"))
# write.table(Param, file= paste0(DIR_DATA_OUTPUT,"Param_calage_cut.csv"), sep = ";",
#             row.names = FALSE)
# 
# ## On cale sur toute la periode mais on bloque un (ou plusieurs) parametre(s)
# #   qui ne sera (seront) pas cale(s)
# OutputsModel <- fc.MainRunGR4J(BasinDataBis, InputsModel,
#                                P_WU = c(P_WU_Beg, P_WU_End),
#                                P_R = c(P_R_Beg, P_R_End), Appli = "Cal",
#                                transfoFO = c("Q", "inv"),
#                                FixedParam = c(NA, NA, NA, 2))
# Qcal <- OutputsModel$OutputsModel_Cal$Qsim
# plot(zoo(BasinDataBis$TabObsQmm, as.Date(BasinDataBis$TabDatesR)), ylab = "Q [mm/d]",
#      xlab = "days")
# lines(zoo(Qcal, as.Date(OutputsModel$OutputsModel_Cal$DatesR)), col = "red")
# 
##__Exemple_Application_en_simulation_______________________________________####
## On simulate sur toute la periode
pdf(paste0(DIR_GRAPHE, "Trace_debit_sim.pdf"), paper = "a4r", height = 0, width = 0)
layout(matrix(c(1,
                2), ncol = 1, byrow = TRUE))
for (i in c(1:length(infobv$code))){
  ##__Choix_Basin_ou_Boucles_Sur_Plusieurs_Basins___________________________####
  BasinCode <- infobv$code[i]
  
  ##__BasinData_____________________________________________________________####
  ## RData a creer en amont de ce script, voir script Prep_BasinData.R
  load(paste0(DIR_DATA_INPUT_RDATA, "BasinData_Real/BasinData_",
              BasinCode, ".RData"))
  
  ##Creation d'un basindatabis avec des vecteurs sans NA
  Ind_run <- which(!is.na(BasinData$TabObsP))
  BasinDataBis <- list(BasinCode = BasinData$BasinCode,
                       BasinName = BasinData$BasinName,
                       BasinSurf = BasinData$BasinSurf,
                       TabDatesT = BasinData$TabDatesT[Ind_run],
                       TabDatesR = BasinData$TabDatesR[Ind_run],
                       TabObsQm3s = BasinData$TabObsQm3s[Ind_run],
                       TabObsQmm = BasinData$TabObsQmm[Ind_run],
                       TabObsP = BasinData$TabObsP[Ind_run],
                       TabObsPsafran = BasinData$TabObsPsafran[Ind_run],
                       TabObsN = BasinData$TabObsN[Ind_run],
                       TabObsT = BasinData$TabObsT[Ind_run],
                       TabObsE = BasinData$TabObsE[Ind_run],
                       TabDatesRM = BasinData$TabDatesRM,
                       TabObsQmmM = BasinData$TabObsQmmM)
  ##__Choix_Periodes________________________________________________________####
  ## Choix des periodes : chauffe (pour initialisation des reservoirs) et run 
  #   (pour calage et simulation)
  P_WU_Beg <- format(BasinDataBis$TabDatesR[1], format = "%d/%m/%Y")
  P_WU_End <- format(BasinDataBis$TabDatesR[730], format = "%d/%m/%Y")
  P_R_Beg  <- format(BasinDataBis$TabDatesR[731], format = "%d/%m/%Y")
  P_R_End  <- Dateend2[i] #Dateend2[i]
  
  ##__Creation_de_InputsModel_pour_GR4J_sans_CemaNeige_(function_de_airGR)__####
  InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4J,
                                   DatesR = BasinDataBis$TabDatesR,
                                   Precip = BasinDataBis$TabObsP,
                                   PotEvap = BasinDataBis$TabObsE)
  
  OutputsModel <- fc.MainRunGR4J(BasinDataBis, InputsModel,
                                 P_WU = c(P_WU_Beg, P_WU_End),
                                 P_R = c(P_R_Beg, P_R_End), Appli = "Sim",
                                 FixedParam = as.numeric(Recap_ToutCal[i, 2:5]))
  Qsim <- zoo(OutputsModel$OutputsModel_Sim$Qsim, as.Date(OutputsModel$OutputsModel_Sim$DatesR))
  Qobs <- zoo(BasinDataBis$TabObsQmm, as.Date(BasinDataBis$TabDatesR))
  plot(Qobs, ylab = "Q [mm/d]", xlab = "days", main = infobv$nom[i])
  lines(Qsim, col = "red")
}
dev.off()
