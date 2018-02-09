##____________________________________________________________________________##
##  Main Script to prepare BasinData for Basin from Banque Hydro              ##
##                                                                            ##
##  Pierre L'Hermite - 20171109 - Prep_BasinData.R                            ##
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
DIR_DATA_INPUT_DEBITS <- paste0(DIR_DATA_INPUT, "bvTxt/QJ_brute_csv_ls/")
DIR_DATA_INPUT_PLUIE  <- paste0(DIR_DATA_INPUT_RDATA, "PJ_bassin_reconstituee_zoo/")
if(substr(version$os, 1, 5) == "linux") {
  DIR_DATA_INPUT_SAFRAN <- "/home/RHAX/DONNEES/LOIEAU/SAFRAN/"
} else {
  DIR_DATA_INPUT_SAFRAN <- "Z:/DONNEES/LOIEAU/SAFRAN/"
}
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
library(rgdal); library(maptools); library(raster)

##__Definition_des_projections______________________________________________####
prjs.L2E <- CRS("+init=epsg:27572")

##__Lecture_Shapes_HYDRO____________________________________________________####
DsnReal <- paste0(DIR_DATA_INPUT, "contour")
LayerReal <- "BvsReal_limni"
## Chargement des shapes (en L2E)
Bvs_Real <- readOGR(dsn = DsnReal, layer = LayerReal, stringsAsFactors = FALSE)
Bv_data <- slot(Bvs_Real, "data")
rm(DsnReal, LayerReal); gc()

##__Basins_Loop_____________________________________________________________####
if(exists(paste0(DIR_DATA_INPUT_RDATA, "InfoBv/infobv_Real.RData"))) {
  load(paste0(DIR_DATA_INPUT_RDATA, "InfoBv/infobv_Real.RData"))
} else {
  infobv <- list()
  infobv$code <- Bv_data$ID[order(Bv_data$ID)]
  TabData <- read.table(paste0(DIR_DATA_INPUT, "bvTxt/stationlimni.csv"), sep = ";", header = TRUE,
                        stringsAsFactors = FALSE)
  infobv$nom <- TabData$Nom[TabData$Code.hydro %in% infobv$code]
  infobv$surf <- c(70.5, 9.22, 29.02, 8.48, 1.53, 12.42, 1.49)
  infobv$exu_L2E <- data.frame(Xexu = TabData$XL2E[TabData$Code.hydro %in% infobv$code],
                               Yexu = TabData$YL2E[TabData$Code.hydro %in% infobv$code],
                               code = infobv$code)
  coordinates(infobv$exu_L2E) <- c("Xexu", "Yexu")
  proj4string(infobv$exu_L2E) <- prjs.L2E
  infobv$centre_L2E <- data.frame(Xcentre = coordinates(Bvs_Real)[, 1],
                                  Ycentre = coordinates(Bvs_Real)[, 2],
                                  code = infobv$code)
  coordinates(infobv$centre_L2E) <- c("Xcentre", "Ycentre")
  proj4string(infobv$centre_L2E) <- prjs.L2E
  save(infobv, file = paste0(DIR_DATA_INPUT_RDATA, "InfoBv/infobv_Real.RData"))
  rm(TabData); gc()
}
ListBasinCode <- infobv$code

for(iBasin in 1:length(ListBasinCode)) {
  BasinCode <- ListBasinCode[iBasin]
  ## Recuperation des debits
  TabData <- read.table(paste0(DIR_DATA_INPUT_DEBITS, "QJ_", BasinCode, ".csv"), sep = ";", header = TRUE)
  for(i in 1:length(TabData$Date)){
    if(TabData$Debit[i] < 0 || is.na(TabData$Debit[i])){
      TabData$Debit[i] <- NA
    }
  }
  Qm3s <- TabData$Debit/1000
  DatesQ <- TabData$Date
  ## BasinData (a faire une fois et sauvegarder si besoin de tourner plusieurs fois)
  BasinData <- CreateBasinDataJfromSafran(Bvs_Real[Bv_data$ID == BasinCode, ],
                                          BasinCode, infobv$surf[iBasin],
                                          DIR_DATA_INPUT_SAFRAN, ETP = "Penman-Monteith",
                                          Name = infobv$nom[iBasin], Qjm3s = Qm3s,
                                          dateQ = DatesQ)
  ## Passage dans la moulinette pour le mensuel
  BasinDataMens <- CreateBasinDataLoiEau(BasinCode, infobv$nom[iBasin], infobv$surf[iBasin],
                                         BasinData$TabDatesT, BasinData$TabObsQm3s,
                                         BasinData$TabObsP, BasinData$TabObsE,
                                         BasinData$TabObsN, BasinData$TabObsT)
  ## Recuperation pluies de bassin
  load(paste0(DIR_DATA_INPUT_PLUIE, "PJ_", BasinCode, ".RData"))
  PJp <- coredata(PJ); Pjd <- index(PJ)
  PJok <- rep(NA, length(BasinData$TabDatesT))
  DebChron <- which(as.Date(BasinData$TabDatesR) == Pjd[1])
  FinChron <- which(as.Date(BasinData$TabDatesR) == Pjd[length(Pjd)])
  PJok[DebChron:FinChron] <- PJp
  
  BasinData <- list(BasinCode = BasinDataMens$BasinCode, BasinName = BasinDataMens$BasinName,
                    BasinSurf = BasinDataMens$BasinSurf, TabDatesT = BasinDataMens$TabDatesT,
                    TabDatesR = BasinDataMens$TabDatesR, TabObsQm3s = BasinDataMens$TabObsQm3s,
                    TabObsQmm = BasinDataMens$TabObsQmm, TabObsP = PJok,
                    TabObsPsafran = BasinDataMens$TabObsP, TabObsN = BasinDataMens$TabObsN,
                    TabObsT = BasinDataMens$TabObsT, TabObsE = BasinDataMens$TabObsE,
                    TabDatesRM = BasinDataMens$TabDatesRM, TabObsQmmM = BasinDataMens$TabObsQmmM)
  ## Sauvegarde
  save(BasinData, file = paste0(DIR_DATA_INPUT_RDATA, "BasinData_Real/BasinData_",
                                BasinCode, ".RData"))
  rm(BasinData, BasinCode); gc()
}
