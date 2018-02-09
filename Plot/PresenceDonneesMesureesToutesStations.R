BasinCode <- infobv$code[1]
load(paste0(DIR_DATA_INPUT_RDATA, "BasinData_Real/BasinData_", BasinCode,
            ".RData"))
VecX <- as.numeric(BasinData$TabDatesT)
VecY <- rep(1, length(BasinData$TabDatesT))
VecY[which(is.na(BasinData$TabObsQmm))] <- NA

for (iBasin in c(2:length(infobv$code))){
  ##__Choix_Basin_ou_Boucles_Sur_Plusieurs_Basins___________________________####
  BasinCode <- infobv$code[iBasin]
  
  ##__BasinData_____________________________________________________________####
  ## RData a creer en amont de ce script, voir script Prep_BasinData.R
  load(paste0(DIR_DATA_INPUT_RDATA, "BasinData_Real/BasinData_",
              BasinCode, ".RData"))
  
  VecXtemp <- as.numeric(BasinData$TabDatesT)
  VecYtemp <- rep(iBasin, length(BasinData$TabDatesT))
  VecYtemp[which(is.na(BasinData$TabObsQmm))] <- NA
  VecX <- c(VecX, VecXtemp)
  VecY <- c(VecY, VecYtemp)
}

Dateend <- c("20061231", "20101231", "20101231", "20111231", "20101231",
             "20101231", "20080630")
col <- c("black", "green", "red", "lightblue", "purple", "orange", "pink")
layout(matrix(1, ncol = 1, byrow = TRUE))
pdf(paste0(DIR_GRAPHE, "Presence_data_Real_debit.pdf"), paper = "a4")
plot(VecX, VecY, type = "l", cex = 0.5, yaxp = c(0, 7, 7), xlab = "Jours",
     ylab = "Bassins")
abline(v = "19680101", col = "red", lty = "dotted")
abline(v = "19900101", col = "red", lty = "dotted")
abline(v = "20111231", col = "red", lty = "dotted")
for (iBasin in 1:length(infobv$code)) {
  abline(v = Dateend[iBasin], col = col[iBasin])
}
dev.off()
