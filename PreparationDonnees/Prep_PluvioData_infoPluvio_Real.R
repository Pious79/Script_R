##____________________________________________________________________________##
##  Main Script to prepare Pluvio for pluvio from Real Collobrier             ##
##                                                                            ##
##  Pierre L'Hermite - 20171109 - Prep_PluvioData_infoPluvio_Real.R           ##
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
library(rgdal); library(maptools); library(raster); library(zoo)

##__Definition_des_projections______________________________________________####
prjs.L2E <- CRS("+init=epsg:27572")

#InfoPluvio
infoPluvio <- list()
pluvio_code     = c("83043052","83043054","83043056","83043057","83043058","83043060","83043062","83043064","83043065",
                    "83043066","83043067","83043068","83043070","83043071","83043074")
pluvio_nom      = c("Babaou", "Bonaud", "Croix Anselme", "Cros de Guerin", "Cros de Mouton", "Col des Fourches",
                    "Lambert", "Louvieres", "Martels", "Meffrey", "Peyrol", "Portaniere", "Vaudreches",
                    "Bourdins", "Davids")
infoPluvio$nom <- pluvio_nom
infoPluvio$code <- pluvio_code

CoordPluvio <- read.table(file=paste0(DIR_DATA_INPUT, "bvTxt/Coord_Pluvio_L2E.csv"), sep=";", header=TRUE)
infoPluvio$coord <- data.frame(X_Pluvio = CoordPluvio$XL2E,
                               Y_Pluvio = CoordPluvio$YL2E,
                               Code = infoPluvio$code) 
coordinates(infoPluvio$coord) <- c("X_Pluvio", "Y_Pluvio")
proj4string(infoPluvio$coord) <- prjs.L2E

save(infoPluvio, file = paste0(DIR_DATA_INPUT_RDATA,"/InfoPluvio/infoPluvio_Real.RData"))

#PluvioData
load(paste0(DIR_DATA_INPUT_RDATA,"/InfoPluvio/infoPluvio_Real.RData"))
Date_cut <- rep("2007-12-31", 15)
Date_cut[8] <- "2006-12-31"

for (iPluvio in c(1:length(infoPluvio$code))){
  #Pluies brutes
  Pluvio_code <- infoPluvio$code[iPluvio]
  Pbrute <- read.table(paste0(DIR_DATA_INPUT,"Pluie_brute/", Pluvio_code, ".pj"),
                       header = TRUE, sep = ";")
  
  for(ilength in 1:length(Pbrute$Pluie)){
    if(is.na(Pbrute$Pluie[ilength]) || Pbrute$Pluie[ilength] < 0){
      Pbrute$Pluie[ilength] <- NA
    }
  }
  
  Pbrute_zoo <- zoo(Pbrute$Pluie, as.Date(strptime(Pbrute$Date, format = "%Y%m%d")))
  Pluie_cut <- Pbrute_zoo[which(index(Pbrute_zoo) == index(Pbrute_zoo[1])):
                            which(index(Pbrute_zoo) == Date_cut[iPluvio])]
  
  #Dates
  Dates <- seq.Date(as.Date("1958-08-01"), as.Date("2016-07-31"), "day")
  
  
  #Pluie de la longueur des pluies SAFRAN
  Plong <- zoo(NA, as.Date(Dates))
  Plong[which(index(Plong)==index(Pluie_cut[1])):
          which(index(Plong)==index(Pluie_cut[length(Pluie_cut)]))] <- Pluie_cut
  
  #Pluies completes par IDW3
  load("C:/Users/pierre.lhermite/Documents/Pierre/Workspace_R/DataInput/Tableau_pluie_IDW3.RData")
  Pluie_complete <- zoo(as.numeric(TabPluie[iPluvio, ]), Dates)
  
  #Liste
  PluvioData <- list(PluvioCode = infoPluvio$code[iPluvio],
                     PluvioName = infoPluvio$nom[iPluvio],
                     TabDatesR = as.Date(Dates),
                     TabObsP = coredata(Plong),
                     TabcompleteP = coredata(Pluie_complete)) 
  
  save(PluvioData, file = paste0(DIR_DATA_INPUT_RDATA, "/PluvioData_Real/PluvioData_",infoPluvio$code[iPluvio],".RData"))
}
