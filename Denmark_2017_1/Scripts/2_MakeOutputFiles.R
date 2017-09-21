rm(list=ls())
library(data.table)

#- Set the working directory to the folder where you keep your code and data
sysPa       <-"C:/Users/jepol/Desktop/Repository/Denmark_2017_1/"
dataPath    <- paste(sysPa,"Data/",sep="")
#polygonPath <- paste(sysPa,"shapes",sep="")
resPath     <- paste(sysPa,"Results/",sep="")

mx <- fread(paste0(resPath, "MeasuresPrGearGroup.csv"))

#Find top 10 most valuable species
rate <-  melt(mx, id.vars = c("Scenario", "Year", "GearGroup", "NoVessels", "Effort_hrs"),
              measure.vars = c(grep("LE_EURO", colnames(mx), value = T)))
rate <- rate[, sum(value), by=variable]
setorder(rate, -V1)
keep <- gsub("LE_EURO_", "", as.character(rate$variable[1:11]))
#keep <- as.character(rate2$variable[1:11])
not_keep <- gsub("LE_EURO_", "", as.character(rate$variable[12:nrow(rate)]))

# Change format, find the 10 most valuable species and sum the rest into one category: Other.
top <- melt(mx, id.vars = c("Scenario", "Year", "GearGroup", "NoVessels", "Effort_hrs"),
           measure.vars = grep(paste0(keep, collapse="|"), colnames(mx), value = T))
not_top_EURO <- melt(mx, id.vars = c("Scenario", "Year", "GearGroup", "NoVessels", "Effort_hrs"),
           measure.vars = paste0("LE_EURO_", not_keep))
not_top_EURO <- not_top_EURO[,.(variable="LE_EURO_OTHER", value=sum(value), NoVessels=sum(NoVessels), 
                            Effort_hrs=sum(Effort_hrs)), by=c("Scenario", "GearGroup", "Year")]
not_top_KG <- melt(mx, id.vars = c("Scenario", "Year", "GearGroup", "NoVessels", "Effort_hrs"),
                     measure.vars = paste0("LE_KG_", not_keep))
not_top_KG <- not_top_KG[,.(variable="LE_KG_OTHER", value=sum(value), NoVessels=sum(NoVessels), 
                                Effort_hrs=sum(Effort_hrs)), by=c("Scenario", "GearGroup", "Year")]

top_other <- rbind(top, not_top_EURO, not_top_KG)

#Change format
px <- dcast(top_other,  Scenario+variable+GearGroup ~ Year, value.var = "value")

#Remove decimal numbers
int_col <- grep("0", colnames(px), value = T)
px[,(int_col):= lapply(.SD, as.integer), .SDcols = int_col]

#Make it readable - change specieskodes to speciesnames
LE_KGS <-c(grep("KG", colnames(mx), value = T), "LE_KG_OTHER")
LE_EUROS <-c(grep("EURO", colnames(mx), value = T), "LE_EURO_OTHER")

artsliste <- fread(paste0(dataPath, "ArtslisteNAER.csv"))
artsliste <- artsliste[,2:4]
setkey(artsliste, "FAO_kode")

kg <- px[variable %in% LE_KGS]
kg <- data.table(Specie=gsub("LE_KG_", "", kg$variable), kg)
kg[,.(Specie=gsub("LE_KG_", "", variable))]
setkey(kg, "Specie")
kg <- artsliste[kg]
kg[,c("FAO_kode", "variable"):=NULL]
setorder(kg, Scenario, GearGroup, -2016)
kg[is.na(kg)] = 0
setcolorder(kg, c("Scenario", "GearGroup", "DK_art", "GB_art", 2012:2016))

euro <- px[variable %in% LE_EUROS]
euro <- data.table(Specie=gsub("LE_EURO_", "", euro$variable), euro)
euro[,.(Specie=gsub("LE_EURO_", "", variable))]
setkey(euro, "Specie")
euro <- artsliste[euro]
euro[,c("FAO_kode", "variable"):=NULL]
setorder(euro, Scenario, GearGroup, -2016)
euro[is.na(euro)] = 0
setcolorder(kg, c("Scenario", "GearGroup", "DK_art", "GB_art", 2012:2016))

## Effort in hour in each area, based on year, area and geargroup ##
ef <- mx[,sum(Effort_hrs),by=c("Scenario", "GearGroup", "Year")]
ef2 <- dcast(ef,  Scenario+GearGroup ~ Year, value.var = "V1")
ef2[is.na(ef2)]=0
ef2[,(int_col):= lapply(.SD, as.integer), .SDcols = as.character(2012:2016)]

## Number of vessels in each area, based on year, area and geargroup ##
vno <- mx[,sum(NoVessels),by=c("Scenario", "GearGroup", "Year")]
vno2 <- dcast(vno,  Scenario+GearGroup ~ Year, value.var = "V1")
vno2[is.na(vno2)]=0
vno2[,(int_col):= lapply(.SD, as.integer), .SDcols = as.character(2012:2016)]

#Write to files
fwrite(kg, paste0(resPath, "CatchInKg.csv"))
fwrite(euro, paste0(resPath, "CatchInEURO.csv"))
fwrite(ef2, paste0(resPath, "EffortInHours.csv"))
fwrite(vno2, paste0(resPath, "NoVessel.csv"))