##____________________________________________________________________________##
##  Plot sur la robustesse du mod?le GR4J sur les bassins versants            ##
##          du Real Collobrier                                                ##
##  Florine Garcia - 20171103 - Analyse_Robustesse_GR4J.R                     ##
##  Script modifie le 20171105 par Florine Garcia :                           ##
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
library(airGR)
library(hydroGOF)
library(zoo)

##__infobv__________________________________________________________________####
## RData a creer en amont de ce script. Doit etre une liste contenant au moins
# les info suivantes : code [character] : code des Bvs
#                      surf [numeric] : surface des Bvs en km2
FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoBv/infobv_Real.RData")
load(FileInfobv) ; rm(FileInfobv) ; gc()

load(paste0(DIR_DATA_OUTPUT, "RobustesseGR4J/Recap_AnalyseRobustesseGR4J.RData"))

col <- c("black", "green", "red", "lightblue", "purple", "orange", "pink")

pdf(paste0(DIR_GRAPHE, "Robustesse_GR4J_Real.pdf"),
    paper = "a4r", height = 0, width = 0)

##__Sur_Toute_la_periode____________________________________________________####
## Param
layout(matrix(c(1, 2,
                3, 4), ncol = 2, byrow = TRUE))
plot(infobv$surf[order(infobv$surf)], Recap_ToutCal$X1[order(infobv$surf)],
     xlab = "surf [km2]", ylab = "X1", main = "X1 [mm]",
     col = col[order(infobv$surf)], pch = 19, cex = 2)
grid()
plot(infobv$surf[order(infobv$surf)], Recap_ToutCal$X2[order(infobv$surf)],
     xlab = "surf [km2]", ylab = "X2", main = "X2 [-]",
     col = col[order(infobv$surf)], pch = 19, cex = 2)
grid()
plot(infobv$surf[order(infobv$surf)], Recap_ToutCal$X3[order(infobv$surf)],
     xlab = "surf [km2]", ylab = "X3", main = "X3 [mm]",
     col = col[order(infobv$surf)], pch = 19, cex = 2)
grid()
plot(infobv$surf[order(infobv$surf)], Recap_ToutCal$X4[order(infobv$surf)],
     xlab = "surf [km2]", ylab = "X4", main = "X4 [d]",
     col = col[order(infobv$surf)], pch = 19, cex = 2)
grid()

## Error
layout(matrix(c(1, 2,
                3, 4), ncol = 2, byrow = TRUE))
plot(infobv$surf[order(infobv$surf)], Recap_Sim2Per$FO[order(infobv$surf)],
     xlab = "surf [km2]", ylab = "FO", main = "FO [-]",
     col = col[order(infobv$surf)], pch = 19, cex = 2)
grid()
plot(infobv$surf[order(infobv$surf)], Recap_Sim2Per$C2M_Q[order(infobv$surf)],
     xlab = "surf [km2]", ylab = "C2M", main = "C2M(Q) [-]",
     col = col[order(infobv$surf)], pch = 19, cex = 2)
grid()
plot(infobv$surf[order(infobv$surf)], Recap_Sim2Per$C2M_sqrtQ[order(infobv$surf)],
     xlab = "surf [km2]", ylab = "C2M", main = "C2M(sqrt(Q)) [-]",
     col = col[order(infobv$surf)], pch = 19, cex = 2)
grid()
plot(infobv$surf[order(infobv$surf)], Recap_Sim2Per$C2M_invQ[order(infobv$surf)],
     xlab = "surf [km2]", ylab = "C2M", main = "C2M(1/Q) [-]",
     col = col[order(infobv$surf)], pch = 19, cex = 2)
grid()

##__Robustesse_2_Sous_Periodes______________________________________________####
## Param
layout(matrix(c(1, 2,
                3, 4), ncol = 2, byrow = TRUE))
plot(Recap_Cal2Per$X1[1:7], Recap_Cal2Per$X1[8:14],
     xlim = c(min(Recap_Cal2Per$X1), max(Recap_Cal2Per$X1)),
     ylim = c(min(Recap_Cal2Per$X1), max(Recap_Cal2Per$X1)),
     xlab = "P1", ylab = "P2", main = "X1 [mm]", col = col, pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_Cal2Per$X2[1:7], Recap_Cal2Per$X2[8:14],
     xlim = c(min(Recap_Cal2Per$X2), max(Recap_Cal2Per$X2)),
     ylim = c(min(Recap_Cal2Per$X2), max(Recap_Cal2Per$X2)),
     xlab = "P1", ylab = "P2", main = "X2 [-]", col = col, pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_Cal2Per$X3[1:7], Recap_Cal2Per$X3[8:14],
     xlim = c(min(Recap_Cal2Per$X3), max(Recap_Cal2Per$X3)),
     ylim = c(min(Recap_Cal2Per$X3), max(Recap_Cal2Per$X3)),
     xlab = "P1", ylab = "P2", main = "X3 [mm]", col = col, pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_Cal2Per$X4[1:7], Recap_Cal2Per$X4[8:14],
     xlim = c(min(Recap_Cal2Per$X4), max(Recap_Cal2Per$X4)),
     ylim = c(min(Recap_Cal2Per$X4), max(Recap_Cal2Per$X4)),
     xlab = "P1", ylab = "P2", main = "X4 [d]", col = col, pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")

## Error Sim
layout(matrix(c(1, 2,
                3, 4), ncol = 2, byrow = TRUE))
plot(Recap_Sim2Per$FO[1:7], Recap_Sim2Per$FO[8:14],
     xlim = c(min(Recap_Sim2Per$FO), max(Recap_Sim2Per$FO)),
     ylim = c(min(Recap_Sim2Per$FO), max(Recap_Sim2Per$FO)),
     xlab = "cal P1", ylab = "cal P2", main = "FO(Qsim) [-]", col = col, pch = 19,
     cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_Sim2Per$C2M_Q[1:7], Recap_Sim2Per$C2M_Q[8:14],
     xlim = c(min(Recap_Sim2Per$C2M_Q), max(Recap_Sim2Per$C2M_Q)),
     ylim = c(min(Recap_Sim2Per$C2M_Q), max(Recap_Sim2Per$C2M_Q)),
     xlab = "cal P1", ylab = "cal P2", main = "C2M_Q(Qsim) [-]", col = col, pch = 19,
     cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_Sim2Per$C2M_sqrtQ[1:7], Recap_Sim2Per$C2M_sqrtQ[8:14],
     xlim = c(min(Recap_Sim2Per$C2M_sqrtQ), max(Recap_Sim2Per$C2M_sqrtQ)),
     ylim = c(min(Recap_Sim2Per$C2M_sqrtQ), max(Recap_Sim2Per$C2M_sqrtQ)),
     xlab = "cal P1", ylab = "cal P2", main = "C2M_sqrtQ(Qsim) [-]", col = col,
     pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_Sim2Per$C2M_invQ[1:7], Recap_Sim2Per$C2M_invQ[8:14],
     xlim = c(min(Recap_Sim2Per$C2M_invQ), max(Recap_Sim2Per$C2M_invQ)),
     ylim = c(min(Recap_Sim2Per$C2M_invQ), max(Recap_Sim2Per$C2M_invQ)),
     xlab = "cal P1", ylab = "cal P2", main = "C2M_invQ(Qsim) [-]", col = col,
     pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")

## Error Cal
layout(matrix(c(1, 2,
                3, 4), ncol = 2, byrow = TRUE))
plot(Recap_Cal2Per$FO[1:7], Recap_Cal2Per$FO[8:14],
     xlim = c(min(Recap_Cal2Per$FO), max(Recap_Cal2Per$FO)),
     ylim = c(min(Recap_Cal2Per$FO), max(Recap_Cal2Per$FO)),
     xlab = "cal P1", ylab = "cal P2", main = "FO(Qcal) [-]", col = col, pch = 19,
     cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_Cal2Per$C2M_Q[1:7], Recap_Cal2Per$C2M_Q[8:14],
     xlim = c(min(Recap_Cal2Per$C2M_Q), max(Recap_Cal2Per$C2M_Q)),
     ylim = c(min(Recap_Cal2Per$C2M_Q), max(Recap_Cal2Per$C2M_Q)),
     xlab = "cal P1", ylab = "cal P2", main = "C2M(Qcal) [-]", col = col, pch = 19,
     cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_Cal2Per$C2M_sqrtQ[1:7], Recap_Cal2Per$C2M_sqrtQ[8:14],
     xlim = c(min(Recap_Cal2Per$C2M_sqrtQ), max(Recap_Cal2Per$C2M_sqrtQ)),
     ylim = c(min(Recap_Cal2Per$C2M_sqrtQ), max(Recap_Cal2Per$C2M_sqrtQ)),
     xlab = "cal P1", ylab = "cal P2", main = "C2M(sqrt(Qcal)) [-]", col = col,
     pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_Cal2Per$C2M_invQ[1:7], Recap_Cal2Per$C2M_invQ[8:14],
     xlim = c(min(Recap_Cal2Per$C2M_invQ), max(Recap_Cal2Per$C2M_invQ)),
     ylim = c(min(Recap_Cal2Per$C2M_invQ), max(Recap_Cal2Per$C2M_invQ)),
     xlab = "cal P1", ylab = "cal P2", main = "C2M(1/Qcal) [-]", col = col,
     pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")

## Comp Cal vs sim
layout(matrix(c(1, 2,
                3, 4), ncol = 2, byrow = TRUE))
plot(Recap_Cal2Per$FO, Recap_Sim2Per$FO,
     xlim = c(min(c(Recap_Cal2Per$FO, Recap_Sim2Per$FO)),
              max(c(Recap_Cal2Per$FO, Recap_Sim2Per$FO))),
     ylim = c(min(c(Recap_Cal2Per$FO, Recap_Sim2Per$FO)),
              max(c(Recap_Cal2Per$FO, Recap_Sim2Per$FO))),
     xlab = "cal", ylab = "sim", main = "FO [-]", col = col, pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_Cal2Per$C2M_Q, Recap_Sim2Per$C2M_Q,
     xlim = c(min(c(Recap_Cal2Per$C2M_Q, Recap_Sim2Per$C2M_Q)),
              max(c(Recap_Cal2Per$C2M_Q, Recap_Sim2Per$C2M_Q))),
     ylim = c(min(c(Recap_Cal2Per$C2M_Q, Recap_Sim2Per$C2M_Q)),
              max(c(Recap_Cal2Per$C2M_Q, Recap_Sim2Per$C2M_Q))),
     xlab = "cal", ylab = "sim", main = "C2M(Q) [-]", col = col, pch = 19,
     cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_Cal2Per$C2M_sqrtQ, Recap_Sim2Per$C2M_sqrtQ,
     xlim = c(min(c(Recap_Cal2Per$C2M_sqrtQ, Recap_Sim2Per$C2M_sqrtQ)),
              max(c(Recap_Cal2Per$C2M_sqrtQ, Recap_Sim2Per$C2M_sqrtQ))),
     ylim = c(min(c(Recap_Cal2Per$C2M_sqrtQ, Recap_Sim2Per$C2M_sqrtQ)),
              max(c(Recap_Cal2Per$C2M_sqrtQ, Recap_Sim2Per$C2M_sqrtQ))),
     xlab = "cal", ylab = "sim", main = "C2M(sqrt(Q)) [-]", col = col, pch = 19,
     cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_Cal2Per$C2M_invQ, Recap_Sim2Per$C2M_invQ,
     xlim = c(min(c(Recap_Cal2Per$C2M_invQ, Recap_Sim2Per$C2M_invQ)),
              max(c(Recap_Cal2Per$C2M_invQ, Recap_Sim2Per$C2M_invQ))),
     ylim = c(min(c(Recap_Cal2Per$C2M_invQ, Recap_Sim2Per$C2M_invQ)),
              max(c(Recap_Cal2Per$C2M_invQ, Recap_Sim2Per$C2M_invQ))),
     xlab = "cal", ylab = "sim", main = "C2M(1/Q) [-]", col = col, pch = 19,
     cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")

##__Robustesse_Humide_sec___________________________________________________####
## Param
layout(matrix(c(1, 2,
                3, 4), ncol = 2, byrow = TRUE))
plot(Recap_CalWD$X1[1:7], Recap_CalWD$X1[8:14],
     xlim = c(min(Recap_CalWD$X1), max(Recap_CalWD$X1)),
     ylim = c(min(Recap_CalWD$X1), max(Recap_CalWD$X1)),
     xlab = "Wet", ylab = "Dry", main = "X1 [mm]", col = col, pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_CalWD$X2[1:7], Recap_CalWD$X2[8:14],
     xlim = c(min(Recap_CalWD$X2), max(Recap_CalWD$X2)),
     ylim = c(min(Recap_CalWD$X2), max(Recap_CalWD$X2)),
     xlab = "Wet", ylab = "Dry", main = "X2 [-]", col = col, pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_CalWD$X3[1:7], Recap_CalWD$X3[8:14],
     xlim = c(min(Recap_CalWD$X3), max(Recap_CalWD$X3)),
     ylim = c(min(Recap_CalWD$X3), max(Recap_CalWD$X3)),
     xlab = "Wet", ylab = "Dry", main = "X3 [mm]", col = col, pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_CalWD$X4[1:7], Recap_CalWD$X4[8:14],
     xlim = c(min(Recap_CalWD$X4), max(Recap_CalWD$X4)),
     ylim = c(min(Recap_CalWD$X4), max(Recap_CalWD$X4)),
     xlab = "Wet", ylab = "Dry", main = "X4 [d]", col = col, pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")

## Error Sim
layout(matrix(c(1, 2,
                3, 4), ncol = 2, byrow = TRUE))
plot(Recap_SimWD$FO[1:7], Recap_SimWD$FO[8:14],
     xlim = c(min(Recap_SimWD$FO), max(Recap_SimWD$FO)),
     ylim = c(min(Recap_SimWD$FO), max(Recap_SimWD$FO)),
     xlab = "cal Wet", ylab = "cal Dry", main = "FO(Qsim) [-]", col = col,
     pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_SimWD$C2M_Q[1:7], Recap_SimWD$C2M_Q[8:14],
     xlim = c(min(Recap_SimWD$C2M_Q), max(Recap_SimWD$C2M_Q)),
     ylim = c(min(Recap_SimWD$C2M_Q), max(Recap_SimWD$C2M_Q)),
     xlab = "cal Wet", ylab = "cal Dry", main = "C2M(Qsim) [-]", col = col,
     pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_SimWD$C2M_sqrtQ[1:7], Recap_SimWD$C2M_sqrtQ[8:14],
     xlim = c(min(Recap_SimWD$C2M_sqrtQ), max(Recap_SimWD$C2M_sqrtQ)),
     ylim = c(min(Recap_SimWD$C2M_sqrtQ), max(Recap_SimWD$C2M_sqrtQ)),
     xlab = "cal Wet", ylab = "cal Dry", main = "C2M(sqrt(Qsim)) [-]",
     col = col, pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_SimWD$C2M_invQ[1:7], Recap_SimWD$C2M_invQ[8:14],
     xlim = c(min(Recap_SimWD$C2M_invQ), max(Recap_SimWD$C2M_invQ)),
     ylim = c(min(Recap_SimWD$C2M_invQ), max(Recap_SimWD$C2M_invQ)),
     xlab = "cal Wet", ylab = "cal Dry", main = "C2M(1/Qsim) [-]", col = col,
     pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")

## Error Cal
layout(matrix(c(1, 2,
                3, 4), ncol = 2, byrow = TRUE))
plot(Recap_CalWD$FO[1:7], Recap_CalWD$FO[8:14],
     xlim = c(min(Recap_CalWD$FO), max(Recap_CalWD$FO)),
     ylim = c(min(Recap_CalWD$FO), max(Recap_CalWD$FO)),
     xlab = "cal Wet", ylab = "cal Dry", main = "FO(Qcal) [-]", col = col,
     pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_CalWD$C2M_Q[1:7], Recap_CalWD$C2M_Q[8:14],
     xlim = c(min(Recap_CalWD$C2M_Q), max(Recap_CalWD$C2M_Q)),
     ylim = c(min(Recap_CalWD$C2M_Q), max(Recap_CalWD$C2M_Q)),
     xlab = "cal Wet", ylab = "cal Dry", main = "C2M(Qcal) [-]", col = col,
     pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_CalWD$C2M_sqrtQ[1:7], Recap_CalWD$C2M_sqrtQ[8:14],
     xlim = c(min(Recap_CalWD$C2M_sqrtQ), max(Recap_CalWD$C2M_sqrtQ)),
     ylim = c(min(Recap_CalWD$C2M_sqrtQ), max(Recap_CalWD$C2M_sqrtQ)),
     xlab = "cal Wet", ylab = "cal Dry", main = "C2M(sqrt(Qcal)) [-]",
     col = col, pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_CalWD$C2M_invQ[1:7], Recap_CalWD$C2M_invQ[8:14],
     xlim = c(min(Recap_CalWD$C2M_invQ), max(Recap_CalWD$C2M_invQ)),
     ylim = c(min(Recap_CalWD$C2M_invQ), max(Recap_CalWD$C2M_invQ)),
     xlab = "cal Wet", ylab = "cal Dry", main = "C2M(1/Qcal) [-]", col = col,
     pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")

## Comp Cal vs sim
layout(matrix(c(1, 2,
                3, 4), ncol = 2, byrow = TRUE))
plot(Recap_CalWD$FO, Recap_SimWD$FO,
     xlim = c(min(c(Recap_CalWD$FO, Recap_SimWD$FO)),
              max(c(Recap_CalWD$FO, Recap_SimWD$FO))),
     ylim = c(min(c(Recap_CalWD$FO, Recap_SimWD$FO)),
              max(c(Recap_CalWD$FO, Recap_SimWD$FO))),
     xlab = "cal", ylab = "sim", main = "FO [-]", col = col, pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_CalWD$C2M_Q, Recap_SimWD$C2M_Q,
     xlim = c(min(c(Recap_CalWD$C2M_Q, Recap_SimWD$C2M_Q)),
              max(c(Recap_CalWD$C2M_Q, Recap_SimWD$C2M_Q))),
     ylim = c(min(c(Recap_CalWD$C2M_Q, Recap_SimWD$C2M_Q)),
              max(c(Recap_CalWD$C2M_Q, Recap_SimWD$C2M_Q))),
     xlab = "cal", ylab = "sim", main = "C2M(Q) [-]", col = col, pch = 19,
     cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_CalWD$C2M_sqrtQ, Recap_SimWD$C2M_sqrtQ,
     xlim = c(min(c(Recap_CalWD$C2M_sqrtQ, Recap_SimWD$C2M_sqrtQ)),
              max(c(Recap_CalWD$C2M_sqrtQ, Recap_SimWD$C2M_sqrtQ))),
     ylim = c(min(c(Recap_CalWD$C2M_sqrtQ, Recap_SimWD$C2M_sqrtQ)),
              max(c(Recap_CalWD$C2M_sqrtQ, Recap_SimWD$C2M_sqrtQ))),
     xlab = "cal", ylab = "sim", main = "C2M(sqrt(Q)) [-]", col = col, pch = 19,
     cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_CalWD$C2M_invQ, Recap_SimWD$C2M_invQ,
     xlim = c(min(c(Recap_CalWD$C2M_invQ, Recap_SimWD$C2M_invQ)),
              max(c(Recap_CalWD$C2M_invQ, Recap_SimWD$C2M_invQ))),
     ylim = c(min(c(Recap_CalWD$C2M_invQ, Recap_SimWD$C2M_invQ)),
              max(c(Recap_CalWD$C2M_invQ, Recap_SimWD$C2M_invQ))),
     xlab = "cal", ylab = "sim", main = "C2M(1/Q) [-]", col = col, pch = 19,
     cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")

##__Robustesse_1_Annee_Sur_2________________________________________________####
## Param
layout(matrix(c(1, 2,
                3, 4), ncol = 2, byrow = TRUE))
plot(Recap_CalO$X1[1:7], Recap_CalO$X1[8:14],
     xlim = c(min(Recap_CalO$X1), max(Recap_CalO$X1)),
     ylim = c(min(Recap_CalO$X1), max(Recap_CalO$X1)),
     xlab = "Odd", ylab = "Even", main = "X1 [mm]", col = col, pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_CalO$X2[1:7], Recap_CalO$X2[8:14],
     xlim = c(min(Recap_CalO$X2), max(Recap_CalO$X2)),
     ylim = c(min(Recap_CalO$X2), max(Recap_CalO$X2)),
     xlab = "Odd", ylab = "Even", main = "X2 [-]", col = col, pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_CalO$X3[1:7], Recap_CalO$X3[8:14],
     xlim = c(min(Recap_CalO$X3), max(Recap_CalO$X3)),
     ylim = c(min(Recap_CalO$X3), max(Recap_CalO$X3)),
     xlab = "Odd", ylab = "Even", main = "X3 [mm]", col = col, pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_CalO$X4[1:7], Recap_CalO$X4[8:14],
     xlim = c(min(Recap_CalO$X4), max(Recap_CalO$X4)),
     ylim = c(min(Recap_CalO$X4), max(Recap_CalO$X4)),
     xlab = "Odd", ylab = "Even", main = "X4 [d]", col = col, pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")

## Error Sim
layout(matrix(c(1, 2,
                3, 4), ncol = 2, byrow = TRUE))
plot(Recap_SimO$FO[1:7], Recap_SimO$FO[8:14],
     xlim = c(min(Recap_SimO$FO), max(Recap_SimO$FO)),
     ylim = c(min(Recap_SimO$FO), max(Recap_SimO$FO)),
     xlab = "cal Odd", ylab = "cal Even", main = "FO(Qsim) [-]", col = col,
     pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_SimO$C2M_Q[1:7], Recap_SimO$C2M_Q[8:14],
     xlim = c(min(Recap_SimO$C2M_Q), max(Recap_SimO$C2M_Q)),
     ylim = c(min(Recap_SimO$C2M_Q), max(Recap_SimO$C2M_Q)),
     xlab = "cal Odd", ylab = "cal Even", main = "C2M(Qsim) [-]", col = col,
     pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_SimO$C2M_sqrtQ[1:7], Recap_SimO$C2M_sqrtQ[8:14],
     xlim = c(min(Recap_SimO$C2M_sqrtQ), max(Recap_SimO$C2M_sqrtQ)),
     ylim = c(min(Recap_SimO$C2M_sqrtQ), max(Recap_SimO$C2M_sqrtQ)),
     xlab = "cal Odd", ylab = "cal Even", main = "C2M(sqrt(Qsim)) [-]",
     col = col, pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_SimO$C2M_invQ[1:7], Recap_SimO$C2M_invQ[8:14],
     xlim = c(min(Recap_SimO$C2M_invQ), max(Recap_SimO$C2M_invQ)),
     ylim = c(min(Recap_SimO$C2M_invQ), max(Recap_SimO$C2M_invQ)),
     xlab = "cal Odd", ylab = "cal Even", main = "C2M(1/Qsim) [-]", col = col,
     pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")

## Error Cal
layout(matrix(c(1, 2,
                3, 4), ncol = 2, byrow = TRUE))
plot(Recap_CalO$FO[1:7], Recap_CalO$FO[8:14],
     xlim = c(min(Recap_CalO$FO), max(Recap_CalO$FO)),
     ylim = c(min(Recap_CalO$FO), max(Recap_CalO$FO)),
     xlab = "cal Odd", ylab = "cal Even", main = "FO(Qcal) [-]", col = col,
     pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_CalO$C2M_Q[1:7], Recap_CalO$C2M_Q[8:14],
     xlim = c(min(Recap_CalO$C2M_Q), max(Recap_CalO$C2M_Q)),
     ylim = c(min(Recap_CalO$C2M_Q), max(Recap_CalO$C2M_Q)),
     xlab = "cal Odd", ylab = "cal Even", main = "C2M(Qcal) [-]", col = col,
     pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_CalO$C2M_sqrtQ[1:7], Recap_CalO$C2M_sqrtQ[8:14],
     xlim = c(min(Recap_CalO$C2M_sqrtQ), max(Recap_CalO$C2M_sqrtQ)),
     ylim = c(min(Recap_CalO$C2M_sqrtQ), max(Recap_CalO$C2M_sqrtQ)),
     xlab = "cal Odd", ylab = "cal Even", main = "C2M(sqrt(Qcal)) [-]",
     col = col, pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_CalO$C2M_invQ[1:7], Recap_CalO$C2M_invQ[8:14],
     xlim = c(min(Recap_CalO$C2M_invQ), max(Recap_CalO$C2M_invQ)),
     ylim = c(min(Recap_CalO$C2M_invQ), max(Recap_CalO$C2M_invQ)),
     xlab = "cal Odd", ylab = "cal Even", main = "C2M(1/Qcal) [-]", col = col,
     pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")

## Comp Cal vs sim
layout(matrix(c(1, 2,
                3, 4), ncol = 2, byrow = TRUE))
plot(Recap_CalO$FO, Recap_SimO$FO,
     xlim = c(min(c(Recap_CalO$FO, Recap_SimO$FO)),
              max(c(Recap_CalO$FO, Recap_SimO$FO))),
     ylim = c(min(c(Recap_CalO$FO, Recap_SimO$FO)),
              max(c(Recap_CalO$FO, Recap_SimO$FO))),
     xlab = "cal", ylab = "sim", main = "FO [-]", col = col, pch = 19, cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_CalO$C2M_Q, Recap_SimO$C2M_Q,
     xlim = c(min(c(Recap_CalO$C2M_Q, Recap_SimO$C2M_Q)),
              max(c(Recap_CalO$C2M_Q, Recap_SimO$C2M_Q))),
     ylim = c(min(c(Recap_CalO$C2M_Q, Recap_SimO$C2M_Q)),
              max(c(Recap_CalO$C2M_Q, Recap_SimO$C2M_Q))),
     xlab = "cal", ylab = "sim", main = "C2M(Q) [-]", col = col, pch = 19,
     cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_CalO$C2M_sqrtQ, Recap_SimO$C2M_sqrtQ,
     xlim = c(min(c(Recap_CalO$C2M_sqrtQ, Recap_SimO$C2M_sqrtQ)),
              max(c(Recap_CalO$C2M_sqrtQ, Recap_SimO$C2M_sqrtQ))),
     ylim = c(min(c(Recap_CalO$C2M_sqrtQ, Recap_SimO$C2M_sqrtQ)),
              max(c(Recap_CalO$C2M_sqrtQ, Recap_SimO$C2M_sqrtQ))),
     xlab = "cal", ylab = "sim", main = "C2M(sqrt(Q)) [-]", col = col, pch = 19,
     cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")
plot(Recap_CalO$C2M_invQ, Recap_SimO$C2M_invQ,
     xlim = c(min(c(Recap_CalO$C2M_invQ, Recap_SimO$C2M_invQ)),
              max(c(Recap_CalO$C2M_invQ, Recap_SimO$C2M_invQ))),
     ylim = c(min(c(Recap_CalO$C2M_invQ, Recap_SimO$C2M_invQ)),
              max(c(Recap_CalO$C2M_invQ, Recap_SimO$C2M_invQ))),
     xlab = "cal", ylab = "sim", main = "C2M(1/Q) [-]", col = col, pch = 19,
     cex = 2)
grid()
abline(a = 0, b = 1, lty = "dotted")

dev.off()
