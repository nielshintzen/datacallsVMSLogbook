#- Clear workspace
rm(list=ls());

library(vmstools)#- code.google.com/p/vmstools
library('sp'); library('rgdal')

#- Define relevant paths (change accordingly)
eflaloPath  <- "W:/IMARES/Data/VMS_/BASEFILES/"                                               #path were clean eflalo stands
tacsatPath  <- "W:/IMARES/Data/VMS_/BASEFILES/"                                               #path were clean tacsat stands
GridPath    <- "W:/IMARES/DenHelder/Common/Projecten/Flyshoot Doggerbank/VMS/Data/Script1.0/" #Path were grid is stored
outPath     <- "W:/IMARES/DenHelder/Common/Projecten/Flyshoot Doggerbank/VMS/Data/Script2.0/" #output folder of this project

#- define years and areas of interest
years         <- 2013:2019 
gears         <- c('SSC','SDN')  #selection of gill net types, might be different then the ones selected here!
ICESrect      <- c('37F2','38F2','38F3','39F3','39F4','40F3','40F4')
FnCleanEflalo <- "cleanEflalo"                    #filename of the cleanEflalo files.
FnCleanTacsat <- "cleanTacsat"                    #filename of the cleanTacsat files.

#-------------------------------------------------------------------------------
#- 1a) load grid, tacsat and eflalo data from files. files should be "Cleaned", 
#------------------------------------------------------------------------------- 
#load grid
load(file=paste0(GridPath,'grd.RData'))
year <- 2013
for(year in years)
  {
  print(year)
  #load data
  load(file.path(eflaloPath,paste(FnCleanEflalo,year,".RData",sep="")))  #named eflalo as an object
  load(file.path(tacsatPath,paste(FnCleanTacsat,year,".RData",sep="")))  #named tacsat as an object
  
  #make sure datasets are formated corretly
  eflalo <- formatEflalo(eflalo)
  tacsat <- formatTacsat(tacsat)
  
  #select relevant gears and fishing areas 
  eflalo   <- subset(eflalo, LE_GEAR %in% gears & LE_RECT %in% ICESrect)
  
  #merge eflalo and tacsat
  tacsatp         <- mergeEflalo2Tacsat(eflalo,tacsat)
  tacsatp         <- tacsatp[tacsatp$FT_REF != 0,] 
  tacsatp$LE_GEAR <- eflalo$LE_GEAR[match(tacsatp$FT_REF,eflalo$FT_REF)]
  tacsatp$LE_MET  <- eflalo$LE_MET[match(tacsatp$FT_REF,eflalo$FT_REF)]
  tacsatp$VE_LEN  <- eflalo$VE_LEN[match(tacsatp$FT_REF,eflalo$FT_REF)]
  
  #Keep only relevant columns.
  tacsatp <- tacsatp[,names(tacsatp) %in% c("VE_COU","VE_ID","VE_REF","VE_LEN", "SI_LATI","SI_LONG","SI_DATE",
                                            "SI_TIME","SI_HE","SI_SP","SI_DATIM","TSID","FT_REF","LE_GEAR","LE_MET") ]
  # if (year==2014)  eflalo.yrs      <- eflalo
  # if (!year==2014) eflalo.yrs      <- rbind(eflalo.yrs,eflalo) 
  if (year==2013)  tacsatp.yrs     <- tacsatp
  if (!year==2013) tacsatp.yrs     <- rbind(tacsatp,tacsatp.yrs) 
}

#-------------------------------------------------------------------------------
#- Determine interval time
#-------------------------------------------------------------------------------
tacsatp.yrs           <- sortTacsat(tacsatp.yrs) #sort the data by vessel and time
tacsatp.yrs$INTV      <- intervalTacsat(tacsatp.yrs,level="trip",weight=c(1,1),fill.na=TRUE)$INTV
idx               <- which(is.na(tacsatp.yrs$INTV)==TRUE)
tacsatp.yrs$INTV[idx] <- intervalTacsat(tacsatp.yrs,level="vessel",weight=c(1,1),fill.na=TRUE)$INTV[idx]
tacsatp.yrs           <- tacsatp.yrs[!is.na(tacsatp.yrs$INTV),]
tacsatp.yrs           <- tacsatp.yrs[tacsatp.yrs$INTV>0,]
tacsatp.yrs$INTV[tacsatp.yrs$INTV >230] <- 115

#-------------------------------------------------------------------------------
#- Determine SI_STATE and select only fishing pings
#-------------------------------------------------------------------------------
#Determin SI_STATE self according to own parameters, activityTacsat...

#select only fishing pings
tacsatp.yrs <- tacsatp.yrs[tacsatp.yrs$SI_STATE == 'f',]

#add benthis gearWidhtParams for mertiers  in dataset (LE_MET) (might differ from parameter values used here!)
tacsatp.yrs$LE_MET <- substr(tacsatp.yrs$LE_MET,1,3)

gearWidthParams <- rbind(c("SDN",a=1948.8347,b=0.2363, unit="VE_ABS",  av=6536.64, propSubSurf=0.05),
                         c("SSC",a=4461.2700,b=0.1176, unit="VE_ABS",  av=6454.21, propSubSurf=0.14))
gearWidthParams <- data.frame(gearWidthParams,stringsAsFactors=FALSE)
colnames(gearWidthParams)[1]  <- "LE_MET"
gearWidthParams$a             <- as.numeric(gearWidthParams$a)
gearWidthParams$b             <- as.numeric(gearWidthParams$b)
gearWidthParams$av            <- as.numeric(gearWidthParams$av)
gearWidthParams$propSubSurf   <- as.numeric(gearWidthParams$propSubSurf)

tacsatp.yrs <- merge(tacsatp.yrs,gearWidthParams,by="LE_MET",all.x=TRUE)

#calculate swept area surf and subsurf
tacsatp.yrs$LE_WIDTH   <- (pi*(((tacsatp.yrs$a * tacsatp.yrs$VE_LEN ^ tacsatp.yrs$b)/1000)/(2*pi))^2)
tacsatp.yrs$LE_SURF    <- (tacsatp.yrs$LE_WIDTH * tacsatp.yrs$INTV/60/1.912500) * 1.5
tacsatp.yrs$LE_SUBSURF <- tacsatp.yrs$LE_SURF * tacsatp.yrs$propSubSurf

#define year
tacsatp.yrs$SI_YEAR <- format(tacsatp.yrs$SI_DATIM ,"%Y")

#-------------------------------------------------------------------------------
#- sum INTV, SURF and SUBSURF values per gridcell and year and store result in grd
#-------------------------------------------------------------------------------
coordsDat   <- coordinates(tacsatp.yrs[,c("SI_LONG","SI_LATI")]) 
spCoordsDat <- SpatialPoints(coordsDat)

tacsatp.yrs$idx <- over(spCoordsDat, grd)[,1]
tacsatp.yrs     <- tacsatp.yrs[!is.na(tacsatp.yrs$idx),]
 
Aggd            <- aggregate(tacsatp.yrs$LE_SURF,by=list(tacsatp.yrs$SI_YEAR, tacsatp.yrs$idx),FUN=sum)
names(Aggd)     <- c('SI_YEAR','idx','LE_SURF')
Aggd$LE_SUBSURF <- aggregate(tacsatp.yrs$LE_SUBSURF,by=list(tacsatp.yrs$SI_YEAR, tacsatp.yrs$idx),FUN=sum)[,3]
Aggd$INTV       <- aggregate(tacsatp.yrs$INTV,by=list(tacsatp.yrs$SI_YEAR, tacsatp.yrs$idx),FUN=sum)[,3]

#store result in spatialgriddatafame
for (year in years)
{
  for(par in c('LE_SURF','LE_SUBSURF','INTV'))
  {
    grd@data$NEW <- NA
    temp <- Aggd[Aggd$SI_YEAR == year,]
    grd@data$NEW[match(temp$idx,grd@data$cellID)]  <- temp[,par]
    names(grd@data)[which(names(grd@data)=='NEW')] <- paste(year,par,sep='_')
  }
}

#-------------------------------------------------------------------------------
#- Save result
#-------------------------------------------------------------------------------
save(grd,file=paste(outPath,'result.RData',sep=''))


