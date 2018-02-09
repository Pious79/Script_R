##____________________________________________________________________________##
##  Script to extract Antilope rain                                           ##
##  Pierre L'HERMITE - 2017-10-12 - Recup_data_Antilope.R                     ##
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
DIR_DATA_INPUT_RDATA    <- paste0(DIR_DATA_INPUT, "Pixel_Meteo_France/")
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

jour <- list.files(DIR_DATA_INPUT_RDATA, pattern = ".txt")

for (Delta in c(1, 3, 6, 9, 12, 24)){
  pdf(paste0(DIR_GRAPHE, "SPI_Pixel_MF_", Delta, ".pdf"), paper = "a4r",
      height = 0, width = 0)
  
  for(ipixel in 1:4){
    Pixel <- read.table(paste0(DIR_DATA_INPUT_RDATA, jour[ipixel]), sep =";",
                        header =  TRUE)
    pluie_MF <- zoo(Pixel$PJ.mm, as.Date(strptime(Pixel$date, format = "%Y%m%d")))
    save(pluie_MF,
         file = paste0(DIR_DATA_OUTPUT, "Pixel_MF_", substr(jour,4,11)[ipixel],".RData"))
    
    Pmonth <- fc.daily2monthly(pluie_MF, FUN = sum, na.rm= TRUE, threshold = 0.1)
    
    Res <- fc.SPI(Pmonth, Delta = Delta, Distribution = "gamma")
    
    fc.plot_MK_Sen_SQMK_Pettitt(Res$SPI,
                                type_data = paste0("Pluie du Pixel MF ", substr(jour,4,11)[ipixel]),
                                nom_axex = "Mois", nom_axey = "SPI [-]")
  }
  dev.off()
}
