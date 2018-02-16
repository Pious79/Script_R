##____________________________________________________________________________##
##  Script to compare data with double cumul                                  ##
##  Pierre L'HERMITE - 20171107 - Double_cumul.R                              ##
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
library(zoo)

##__contenu_script__________________________________________________________####
#Coordonnees pluviometre
FileInfobv <- paste0(DIR_DATA_INPUT_RDATA, "InfoPluvio/infoPluvio_Real.RData")
load(FileInfobv); rm(FileInfobv); gc()

Date_cut <- rep("2015-09-01", 16)
Date_cut[1] <- "2015-10-01"
Date_cut[2] <- "2014-07-01"
Date_cut[10] <- "2015-04-01"
Date_cut[12] <- "2015-04-01"
Date_cut[13] <- "2015-08-01"
Date_cut[14] <- "2014-04-01"
Date_cut[15] <- "2014-04-01"

pdf(paste0(DIR_GRAPHE,"Double_cumul.pdf"), paper = "a4r", height = 0,
    width = 0)
for (iPluvio in 1:length(infoPluvio$nom)){
  ##__Choix_Basin_ou_Boucles_Sur_Plusieurs_Basins___________________________####
  PluvioCode <- infoPluvio$code[iPluvio]
  
  ##__BasinData_____________________________________________________________####
  ##Chargement des pluies brutes ##
  Pobs <- read.table(paste0(DIR_DATA_INPUT,"Pluie_brute/", PluvioCode, ".pj"),
                     sep = ";", header = TRUE)
  
  #Pluie observee
  #Ind_run <- which(!is.na(PluvioData$TabObsP))
  Pobs <- zoo(Pobs$Pluie, as.Date(strptime(Pobs$Date, format = "%Y%m%d")))
  Pobs_month <- fc.daily2monthly(Pobs, FUN = sum, threshold = 1, na.rm = TRUE)
  
  PObs_cut <- Pobs_month[which(index(Pobs_month) == "2006-09-01"):
                           which(index(Pobs_month) == Date_cut[iPluvio])]
  
  for(iLength in 1:length(PObs_cut)){
    if(PObs_cut[iLength] < 0){
      PObs_cut[iLength] <- NA
    }
  }
  Ind_run <- which(is.na(PObs_cut))
  PObs_cut[Ind_run] <- 0
  
  #Pluie antilope
  load(paste0("C:/Users/pierre.lhermite/Documents/Pierre/Workspace_R/Old_DataOutput/DataOutput/Pluie_antilope/Pluie_antilope_",
              PluvioCode,".RData"))
  Panti_month <- fc.daily2monthly(Pantilope, FUN = sum, threshold = 0.1, na.rm = TRUE)
  
  Panti_month[Ind_run] <- 0
  Panti_cut <- Panti_month[which(index(Panti_month) == "2006-09-01"):
                             which(index(Panti_month) == Date_cut[iPluvio])]
  
  Cumul_Pcut <- cumsum(PObs_cut)
  Cumul_Panti <- cumsum(Panti_cut)
  
  plot(Cumul_Pcut, Cumul_Panti, main =
         paste0("Double cumul entre les pluies antilopes et les pluies
              observees a ", infoPluvio$nom[iPluvio]),
       xlim = c(0, max(Cumul_Pcut, Cumul_Panti)),
       ylim = c(0, max(Cumul_Pcut, Cumul_Panti))) 
  abline( a = 0, b = 1, col="darkgrey")
}
dev.off()


