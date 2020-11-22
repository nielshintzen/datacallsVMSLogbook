#- Clear workspace
rm(list=ls());

library(vmstools)#- code.google.com/p/vmstools
library(sp);library(rgdal);library(raster); library(lubridate)

#- Define relevant paths
gitPath    <- "C:/Repositories/datacallsVMSLogbook/Netherlands_2020_N2000"      #to be changed by lab
inPath     <- file.path(gitPath,"output")
shapePath  <- file.path(gitPath,"shapes")                                       #path were shape files are stored.
outPath    <- file.path(gitPath,"shareOutput")
TacsateflaloPath  <- "W:/IMARES/Data/VMS_/BASEFILES/"                           #path were tacsatEflalo stands if used to define activities

#define country
Country       <- 'NLD'                                                          #adjust accordingly!
ICESrect      <- c('40F3','40F3','39F4','39F3','38F4','38F3','38F2','37F6','37F5','37F4','37F3','37F2','36F6','36F5','36F4','36F3','36F2','35F6','35F5','35F4','34F4')
years         <- 2014:2019
includeNBves  <- T                                                              # Number of vessels: change if you don't want to include the nb of vessels
#-------------------------------------------------------------------------------
#- 1a) load tacsat and eflalo files combined from the time period (output script 1.1)
#------------------------------------------------------------------------------- 
load(file=file.path(inPath,'tacsatp.yrs.RData',sep=''))
load(file=file.path(inPath,'eflalo.yrs.RData',sep=''))
load(file=file.path(shapePath,"shapes.RData",sep=""))

#-------------------------------------------------------------------------------
#- 1b) Define activitity Based on your prefered method "f" is fishing, "s" is steaming
#------------------------------------------------------------------------------- 
actMethod <- "tacsatActivity"
#- Define activitity based on vesselspeed

if (actMethod=="speed"){
  tacsatp.yrs               <- tacsatp.yrs[!is.na(tacsatp.yrs$SI_SP),]
  
  #- Read in fixed speed bounds for different gears
  # !!! Make sure that all your gears are added to this speedarr.csv list, some may be lacking !!! #
  
  speedarr              <- read.csv(file=paste(inPath,"/speedarr.csv",sep=""))
  colnames(speedarr)    <- c("LE_GEAR","min","max")
  speedarr$LE_GEAR      <- ac(speedarr$LE_GEAR)
  speedarr              <- speedarr[speedarr$LE_GEAR%in%unique(tacsatp.yrs$LE_GEAR),]
  gears                 <- unique(tacsatp.yrs$LE_GEAR)
  tacsatp.yrs$SI_STATE<- "s"
  for (mm in gears) {
    tacsatp.yrs[tacsatp.yrs$LE_GEAR==mm& tacsatp.yrs$SI_SP >= an(speedarr[speedarr$LE_GEAR==mm,"min"])& tacsatp.yrs$SI_SP <= an(speedarr[speedarr$LE_GEAR==mm,"max",]),]$SI_STATE<-"f"
  }
}
#- Use activity defined in national dataset

if (actMethod =="tacsatActivity"){
  for (year in years){
    load(file.path(TacsateflaloPath,paste("tacsatActivity",year,".RData",sep="")))  #tacsat file with SI_STATE already defined
    if (year==years[1]) act.yrs <- subset(tacsatp,FT_REF %in% tacsatp.yrs$FT_REF,c(VE_REF,SI_LATI,SI_LONG,SI_DATE,SI_TIME,SI_STATE))
    if(year>years[1]) act.yrs <- rbind(act.yrs,subset(tacsatp,FT_REF %in% tacsatp.yrs$FT_REF,c(VE_REF,SI_LATI,SI_LONG,SI_DATE,SI_TIME,SI_STATE)))
  }
  tacsatp.yrs <- merge(tacsatp.yrs,act.yrs,all.x=TRUE)
}


#-------------------------------------------------------------------------------
#- 2a) Allocate catch and value
#------------------------------------------------------------------------------- 

#-------------------------------------------------------------------------------
#- Determine interval time
#-------------------------------------------------------------------------------
tacsatp.yrs           <- sortTacsat(tacsatp.yrs) #sort the data by vessel and time
tacsatp.yrs$INTV      <- intervalTacsat(tacsatp.yrs,level="trip",weight=c(1,1),fill.na=TRUE)$INTV
idx                   <- which(is.na(tacsatp.yrs$INTV)==TRUE)
tacsatp.yrs$INTV[idx] <- intervalTacsat(tacsatp.yrs,level="vessel",weight=c(1,1),fill.na=TRUE)$INTV[idx]
tacsatp.yrs           <- tacsatp.yrs[!is.na(tacsatp.yrs$INTV),]
tacsatp.yrs           <- tacsatp.yrs[tacsatp.yrs$INTV>0,]
tacsatp.yrs$INTV[tacsatp.yrs$INTV >230] <- 115

#- Split eflalo up in two sets, one set that cannot be merged to tacsat and one which matches
eflaloNM              <- subset(eflalo.yrs,!FT_REF %in% unique(tacsatp.yrs$FT_REF))
eflaloM               <- subset(eflalo.yrs, FT_REF %in% unique(tacsatp.yrs$FT_REF))

#select only fishing pings
tacsatp.yrs           <- tacsatp.yrs[tacsatp.yrs$SI_STATE == 'f',]

#- Split effort and total landing among pings
tacsatEflalo          <- (splitAmongPings(tacsat=subset(tacsatp.yrs,FT_REF %in% unique(eflaloM$FT_REF)),eflalo=subset(eflaloM),variable="all",level="day",by="INTV",conserve=T))
cat("total value in eflalo\n")
print(colSums(eflaloM[c(grep("KG",colnames(eflaloM)))],na.rm=T))
cat("total value in tacsatEflalo\n")
print(colSums(tacsatEflalo[c(grep("KG",colnames(tacsatEflalo)))],na.rm=T))

#-------------------------------------------------------------------------------
#- 2b) Produce  table containing number of vessels, effort, value and catch, by subareas per year.
#-------------------------------------------------------------------------------
#- Read in polygon data of the proposed areas'
polnames               <-c("DB_withoutUK.RData","DB_withoutUK_ices.RData","DB_mngtareas_withoutUK.RData")

#- Categorize vessel lengths
tacsatEflalo$LENCAT    <- cut(tacsatEflalo$VE_LEN,breaks=c(0,12,18,24,100),labels=c("0-12","12-18","18-24",">24"))
eflaloNM$LENCAT        <- cut(eflaloNM$VE_LEN,breaks=c(0,12,18,24,100),labels=c("0-12","12-18","18-24",">24"))

for (i in c(1:length(polnames))) {
  load(file.path(shapePath,paste(polnames[i],sep="")))  #
  #- determine which tacsateflalo records are located in the area and select these
  pings  <- SpatialPoints(tacsatEflalo[c('SI_LONG','SI_LATI')],proj4string=CRS("+proj=longlat +ellps=WGS84"))
  area   <- over(pings,shape)$Name
  
  tacsatEflalo$Area            <- area
  
  table                             <- aggregate(tacsatEflalo[c(grep("KG",colnames(tacsatEflalo)),grep("EURO",colnames(tacsatEflalo)))],
                                                 list(YEAR=an(format(tacsatEflalo$SI_DATIM, format = "%Y")),
                                                      GEAR=tacsatEflalo$LE_GEAR,
                                                      LENCAT=tacsatEflalo$LENCAT,
                                                      Area=tacsatEflalo$Area
                                                 ),sum,na.rm=T)
 if (includeNBves) 
   table                             <- merge(table,aggregate(tacsatEflalo["VE_REF"],list(YEAR=an(format(tacsatEflalo$SI_DATIM, format = "%Y")),GEAR=tacsatEflalo$LE_GEAR,LENCAT=tacsatEflalo$LENCAT,Area=tacsatEflalo$Area),function(x) length(unique(x))))
  table$Area                      <- paste(polnames[i],table$Area,sep="-")
  if (i==1) table.Efl             <- table
  if (i>1) table.Efl              <- rbind(table.Efl,table)
}
#- Generate table of eflaloNM effort

table                             <- aggregate(eflaloNM[c(grep("KG",colnames(eflaloNM)),grep("EURO",colnames(eflaloNM)))],
                                               list(Area=eflaloNM$LE_RECT,
                                                    YEAR=year(eflaloNM$FT_DDATIM),
                                                    GEAR=eflaloNM$LE_GEAR,
                                                    LENCAT=eflaloNM$LENCAT
                                               ),sum,na.rm=T)
if (includeNBves) 
  table                             <- merge(table, aggregate(eflaloNM["VE_REF"],
                                                            list(Area=eflaloNM$LE_RECT,
                                                                 YEAR=year(eflaloNM$FT_DDATIM),
                                                                 GEAR=eflaloNM$LE_GEAR,
                                                                 LENCAT=eflaloNM$LENCAT
                                                            ),function(x) length(unique(x))))
#- Combine table of eflaloNM with tacsat-table
final.table                       <- rbind(table.Efl,table[colnames(table.Efl)])
save(final.table,file=paste(outPath,"/final.table.",Country,".Rdata",sep=""))

summary(final.table)
table(final.table$Area)
#-------------------------------------------------------------------------------
#- 3) Calculate swept area
#------------------------------------------------------------------------------- 

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
#- Save ecological impact result
#-------------------------------------------------------------------------------
save(grd,file=paste(outPath,'/Ecolresult_grid_',Country,'.RData',sep=''))





