  
 #load packages
  library("data.table"); library("dplyr"); library("openxlsx")
  
 #empty environment
  rm(list=ls())
  
 #load data
  setwd("C:/Surfdrive/2020 - Spaanse griep - v2/Gemeentes")
  munips <- fread("vergelijking HDNG + OpenArch.txt") #municipalities with observations for 1918 and baseline
  NLGIS <- fread("gemeenten.txt")
  
  
 #clean NLGIS
  head(NLGIS)
   #amsterdamse code
    colnames(NLGIS)[2] <- "ACODE"
   #begindatum
    NLGIS$begindatum <- gsub(" 0:00:00", "", NLGIS$begindatum)
    NLGIS$begindatum <- substr(NLGIS$begindatum, nchar(NLGIS$begindatum)-3, nchar(NLGIS$begindatum))
    NLGIS$begindatum <- as.numeric(NLGIS$begindatum)
   #einddatum
    NLGIS$einddatum <- gsub(" 0:00:00", "", NLGIS$einddatum)
    NLGIS$einddatum <- substr(NLGIS$einddatum, nchar(NLGIS$einddatum)-3, nchar(NLGIS$einddatum))
    NLGIS$einddatum <- as.numeric(NLGIS$einddatum)
   #Provincie
    NLGIS$Provincie <- ifelse(NLGIS$Provincie==1, "Groningen",
                              ifelse(NLGIS$Provincie==2, "Friesland",
                                     ifelse(NLGIS$Provincie==3, "Drenthe",
                                            ifelse(NLGIS$Provincie==4, "Overijssel",
                                                   ifelse(NLGIS$Provincie==5, "Gelderland",
                                                          ifelse(NLGIS$Provincie==6, "Utrecht",
                                                                 ifelse(NLGIS$Provincie==7, "Noord-Holland",
                                                                        ifelse(NLGIS$Provincie==8, "Zuid-Holland",
                                                                               ifelse(NLGIS$Provincie==9, "Zeeland",
                                                                                      ifelse(NLGIS$Provincie==10, "Noord-Brabant",
                                                                                             ifelse(NLGIS$Provincie==11, "Limburg",
                                                                                                    ifelse(NLGIS$Provincie==12, "Flevoland", NA))))))))))))
   #filter
    NLGIS <- NLGIS[which(NLGIS$begindatum<=1918 & NLGIS$einddatum>1918), c("ACODE", "gemeente", "Provincie", "EGG", "Corop")]
   #EGG
    #explore EGG
     length(which(is.na(NLGIS$EGG))) #4
     length(which(!duplicated(NLGIS$EGG))) #128 == 129 - Noordoostpolder & Flevoland + NA
    #complete EGG
     NLGIS$EGG[NLGIS$gemeente=="Kethel en Spaland"] <- 85
     NLGIS$EGG[NLGIS$gemeente=="Koudekerk [ZH]"] <- 73
     NLGIS$EGG[NLGIS$gemeente=="Rosendaal [Ge]"] <- 36
     NLGIS$EGG[NLGIS$gemeente=="Ter Neuzen"] <- 99
   #Corop
    #explore Corop
     length(which(is.na(NLGIS$Corop))) #251
     length(which(!duplicated(NLGIS$Corop))) #40 == 40 - Flevoland + NA
    #complete Corop
     #NLGIS <- read.xlsx("Corop.xlsx")
     
     
  #available in our data?
     NLGIS$Sampled <- ifelse(NLGIS$ACODE %in% munips$ACODE, "Sampled", "No data")
     
    #write.table
     NLGIS <- NLGIS[,c("ACODE", "gemeente", "Sampled", "EGG", "Corop", "Provincie")]
     write.table(NLGIS, file="Spatial units.txt", quote=F, sep ="\t", col.names=T, row.names=F)
     
     
     