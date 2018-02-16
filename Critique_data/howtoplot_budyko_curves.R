##____________________________________________________________________________##
##  Tutorial Script to plot Budyko Curves and the Budyko Curve Tool           ##
##                                                                            ##
##  Florine Garcia - 20171020 - HowToPlotBudykoCurves.R                       ##
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

##__infobv__________________________________________________________________####
## RData a creer en amont de ce script. Doit etre une liste contenant au moins
# les info suivantes : code [character] : code des Bvs
#                      surf [numeric] : surface des Bvs en km2
FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoBv/infobv_Real.RData")
load(FileInfobv); rm(FileInfobv); gc()
ListBasinCode <- infobv$code

##__Chargement_Donnees_et_Calcul_Moyenne_Annuelle___________________________####
AnDeb <- 1959; AnFin <- 2016
MAP <- MAE <- MAR <- as.data.frame(matrix(NA, ncol = length(ListBasinCode),
                                          nrow = (AnFin - AnDeb + 1),
                                          dimnames = list(AnDeb:AnFin,
                                                          ListBasinCode)))
for (iBasin in 1:length(ListBasinCode)) {
  BasinCode <- infobv$code[iBasin]
  load(paste0(DIR_DATA_INPUT_RDATA, "BasinData_Real/BasinData_",
              BasinCode, ".RData"))
  #Chargement des données simulées
  load(paste0(DIR_DATA_INPUT, "Qsimule/Qsim_long/Qsim_long_",BasinCode,".RData"))
  
  MAP[, iBasin] <- fc.daily2annual(zoo((BasinData$TabObsP),
                                       as.Date(BasinData$TabDatesR)),
                                   debYear = "08-01", FUN = sum, na.rm = TRUE,
                                   threshold = 0.1)
  MAE[, iBasin] <- fc.daily2annual(zoo(BasinData$TabObsE,
                                       as.Date(BasinData$TabDatesR)),
                                   debYear = "08-01", FUN = sum, na.rm = TRUE,
                                   threshold = 0.1)
  MAR[, iBasin] <- fc.daily2annual(Q, # zoo(BasinData$TabObsQmm, as.Date(BasinData$TabDatesR)) selon la volonté
                                   debYear = "08-01", FUN = sum, na.rm = TRUE,
                                   threshold = 0.1)
}
##__Calcul_Moyenne_Interannuelle_et_Glissante_______________________________####
## Gestion Lacunes
for (iBasin in 1:length(ListBasinCode)) {
  MAP[is.na(MAR[, iBasin]), iBasin] <- MAE[is.na(MAR[, iBasin]), iBasin] <- NA
}
## Moyenne Interannuelle
MAPmmy <- apply(MAP, MARGIN = 2, FUN = mean, na.rm = TRUE)
MAEmmy <- apply(MAE, MARGIN = 2, FUN = mean, na.rm = TRUE)
MARmmy <- apply(MAR, MARGIN = 2, FUN = mean, na.rm = TRUE)
## Moyenne Glissante sur 10 ans
MAPma <- as.data.frame(sapply(ListBasinCode, FUN = function(x, MA, y, na.rm) {
  MeanVec <- rep(NA, dim(MA)[1] - y + 1)
  for (i in 1:(dim(MA)[1] - y + 1)) {
    MeanVec[i] <- mean(MA[i:(i + y), names(MA) == x], na.rm = na.rm)
  }
  MeanVec[is.nan(MeanVec)] <- NA
  return(MeanVec)
}, MA = MAP, y = 10, na.rm = TRUE))
MAEma <- as.data.frame(sapply(ListBasinCode, FUN = function(x, MA, y, na.rm) {
  MeanVec <- rep(NA, dim(MA)[1] - y + 1)
  for (i in 1:(dim(MA)[1] - y + 1)) {
    MeanVec[i] <- mean(MA[i:(i + y), names(MA) == x], na.rm = na.rm)
  }
  MeanVec[is.nan(MeanVec)] <- NA
  return(MeanVec)
}, MA = MAE, y = 10, na.rm = TRUE))
MARma <- as.data.frame(sapply(ListBasinCode, FUN = function(x, MA, y, na.rm) {
  MeanVec <- rep(NA, dim(MA)[1] - y + 1)
  for (i in 1:(dim(MA)[1] - y + 1)) {
    MeanVec[i] <- mean(MA[i:(i + y), names(MA) == x], na.rm = na.rm)
  }
  MeanVec[is.nan(MeanVec)] <- NA
  return(MeanVec)
}, MA = MAR, y = 10, na.rm = TRUE))
row.names(MAPma) <- row.names(MAEma) <- row.names(MARma) <- AnDeb:(AnFin - 9)

##__Budyko_Curve_Moyenne_Totale_Pour_Tout_Echantillon_______________________####
## Ligne
pdf(paste0(DIR_GRAPHE, "BudykoCurveToutLine_Qsim.pdf"),
    paper = "a4r", height = 0, width = 0)
colori <- rev(rainbow(10, start = 0, end = .45, alpha = 1))
fc.plotBudykoCurve(MAPmmy, MAEmmy, MARmmy, 
                   color = colori[round(10*MAEmmy/MAPmmy)],
                   xlim = c(0, 3), ylim = c(0, 1), pch = 21,
                   namePlot = paste0("Budyko Curve 08/", AnDeb, " - 09/", AnFin),
                   Type = "Line", ReturnAI = TRUE, BasinCode = ListBasinCode)
legend("topleft", fill = colori[round(10*MAEmmy/MAPmmy)], legend = ListBasinCode)
dev.off()
## Courbe
pdf(paste0(DIR_GRAPHE, "BudykoCurveToutCurve_Qsim.pdf"),
    paper = "a4r", height = 0, width = 0)
colori <- rev(rainbow(10, start = 0, end = .45, alpha = 1))
fc.plotBudykoCurve(MAPmmy, MAEmmy, MARmmy, 
                   color = colori[round(10*MAEmmy/MAPmmy)],
                   xlim = c(0, 3), ylim = c(0, 1), pch = 21,
                   namePlot = paste0("Budyko Curve 08/", AnDeb, " - 09/", AnFin),
                   Type = "Curve", ReturnAI = FALSE)
legend("topleft", fill = colori[round(10*MAEmmy/MAPmmy)], legend = ListBasinCode)
dev.off()

##__Budyko_Tool_Moy_Glissante_sur_10_ans_tous_les_2_ans_____________________####
pdf(paste0(DIR_GRAPHE, "BudykoTool_Qsim.pdf"),
    paper = "a4r", height = 0, width = 0)
nbPoints <- round(dim(MAPma)[1] / 2, 0) + 1
col <- c(colorRampPalette(c("green", "red", "yellow"))(nbPoints), "black")
nomLegend <- rep(NA, nbPoints + 1)
for (iPoints in 1:nbPoints) {
  An <- row.names(MAPma)[seq(1, dim(MAPma)[1], 2)][iPoints]
  nomLegend[iPoints] <- paste0("08/", An, " - 09/",
                               row.names(MAP)[which(row.names(MAPma) == An) + 9])
}
nomLegend[length(nomLegend)] <- paste0("08/", AnDeb, " - 09/", AnFin)
for (iBasin in 1:length(ListBasinCode)) {
  MAPgli <- c(MAPma[seq(1, dim(MAPma)[1], 2), iBasin], MAPmmy[iBasin])
  MAEgli <- c(MAEma[seq(1, dim(MAEma)[1], 2), iBasin], MAEmmy[iBasin])
  MARgli <- c(MARma[seq(1, dim(MARma)[1], 2), iBasin], MARmmy[iBasin])
  fc.plotBudykoCurve(MAPgli, MAEgli, MARgli, color = col,
                     xlim = c(0, 2), ylim = c(0, 1), pch = 19,
                     namePlot = paste0("Budyko Tool ", ListBasinCode[iBasin]),
                     Type = "Curve", ReturnAI = FALSE)
  legend("bottomleft", fill = col, legend = nomLegend,
         cex = 0.9, inset = c(0.1, 0.1))
}
dev.off()
