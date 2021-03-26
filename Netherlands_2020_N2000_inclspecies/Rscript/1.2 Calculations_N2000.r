#- Clear workspace
rm(list=ls());

library(vmstools)#- code.google.com/p/vmstools
library(sp);library(rgdal);library(raster); library(lubridate)

#- Define relevant paths
gitPath    <- "C:/Users/hamon004/Github/VMSwork/datacallsVMSLogbook/Netherlands_2020_N2000_inclspecies"      #to be changed by lab
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
load(file=file.path(inPath,'tacsatp.yrs.RData'))
load(file=file.path(inPath,'eflalo.yrs.RData'))
load(file=file.path(shapePath,"shapes.RData"))
#load(file=file.path(shapePath,"grd.RData"))

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
# grid16                 <- createGrid(xrange = c(floor(min(bbox(grd)["x",])),ceiling(max(bbox(grd)["x",]))),
#                                      yrange = c(floor(min(bbox(grd)["y",])),ceiling(max(bbox(grd)["y",]))),
#                                      resx   = 1/4,
#                                      resy   = 1/8,
#                                      exactBorder=T,type="SpatialGridDataFrame")
# grid16@data$LE_RECT    <- ICESrectangle(data.frame(SI_LONG=coordinates(grid16)[,1],SI_LATI=coordinates(grid16)[,2]))
# grid16@data$SI_LONG    <- coordinates(grid16)[,1]
# grid16@data$SI_LATI    <- coordinates(grid16)[,2]
# grid16@data$ID         <- 1:nrow(grid16@data)
# grid16                 <- as(grid16,"SpatialPolygonsDataFrame")
# proj4string(grid16)    <- CRS("+proj=longlat +datum=WGS84 +no_defs")
# 
#- Categorize vessel lengths
tacsatEflalo$LENCAT    <- cut(tacsatEflalo$VE_LEN,breaks=c(0,12,18,24,100),labels=c("0-12","12-18","18-24",">24"))
eflaloNM$LENCAT        <- cut(eflaloNM$VE_LEN,breaks=c(0,12,18,24,100),labels=c("0-12","12-18","18-24",">24"))
tacsatEflalo$VE_KW     <- eflaloM$VE_KW[match(tacsatEflalo$FT_REF,eflaloM$FT_REF)]
tacsatEflalo$KWCAT     <- cut(tacsatEflalo$VE_KW,breaks=c(0,225,10000),labels=c("0-300",">300"))
eflaloNM$KWCAT         <- cut(eflaloNM$VE_KW,breaks=c(0,225,10000),labels=c("0-300",">300"))

#- determine which tacsateflalo records are located in the area and select these
pings   <- SpatialPoints(tacsatEflalo[c('SI_LONG','SI_LATI')],proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
inArea  <- as.character(shapes@data$Gebied)[over(pings,as(shapes,"SpatialPolygons"))]
# inGrid9 <- as.character(grd@data$sub_code)[over(pings,as(grd,"SpatialPolygons"))]
# inGrid16<- as.character(grid16@data$ID)[over(pings,as(grid16,"SpatialPolygons"))]

#- Perform aggregation for Natura 2000 area
tacsatEflalo$Area                 <- inArea
tableN2000                        <- aggregate(tacsatEflalo[c(grep("KG",colnames(tacsatEflalo),value=T),grep("EURO",colnames(tacsatEflalo),value=T),"INTV")],
                                               list(YEAR=an(format(tacsatEflalo$SI_DATIM, format = "%Y")),
                                                    MONTH=an(format(tacsatEflalo$SI_DATIM, format = "%m")),
                                                    GEAR=tacsatEflalo$LE_GEAR,
                                                    LENCAT=tacsatEflalo$LENCAT,
                                                    KWCAT=tacsatEflalo$KWCAT,
                                                    Area=tacsatEflalo$Area
                                               ),sum,na.rm=T)
if (includeNBves) 
 table.N2000                      <- merge(tableN2000,aggregate(tacsatEflalo["VE_REF"],list(YEAR=an(format(tacsatEflalo$SI_DATIM, format = "%Y")),MONTH=an(format(tacsatEflalo$SI_DATIM, format = "%m")),GEAR=tacsatEflalo$LE_GEAR,LENCAT=tacsatEflalo$LENCAT,KWCAT=tacsatEflalo$KWCAT,Area=tacsatEflalo$Area),function(x) length(unique(x))))

# #- Perform aggregation for 1/9th grid
# tacsatEflalo$Area                 <- inGrid9
# tableGrid9                        <- aggregate(tacsatEflalo[c(grep("KG",colnames(tacsatEflalo)),grep("EURO",colnames(tacsatEflalo)),"INTV")],
#                                                list(YEAR=an(format(tacsatEflalo$SI_DATIM, format = "%Y")),
#                                                     MONTH=an(format(tacsatEflalo$SI_DATIM, format = "%m")),
#                                                     GEAR=tacsatEflalo$LE_GEAR,
#                                                     LENCAT=tacsatEflalo$LENCAT,
#                                                     KWCAT=tacsatEflalo$KWCAT,
#                                                     Area=tacsatEflalo$Area
#                                                ),sum,na.rm=T)
# if (includeNBves)
#  table.Grid9                      <- merge(tableGrid9,aggregate(tacsatEflalo["VE_REF"],list(YEAR=an(format(tacsatEflalo$SI_DATIM, format = "%Y")),MONTH=an(format(tacsatEflalo$SI_DATIM, format = "%m")),GEAR=tacsatEflalo$LE_GEAR,LENCAT=tacsatEflalo$LENCAT,KWCAT=tacsatEflalo$KWCAT,Area=tacsatEflalo$Area),function(x) length(unique(x))))
# 
# #- Perform aggregation for 1/16th grid
# tacsatEflalo$Area                 <- inGrid16
# tableGrid16                       <- aggregate(tacsatEflalo[c(grep("KG",colnames(tacsatEflalo)),grep("EURO",colnames(tacsatEflalo)),"INTV")],
#                                                list(YEAR=an(format(tacsatEflalo$SI_DATIM, format = "%Y")),
#                                                     MONTH=an(format(tacsatEflalo$SI_DATIM, format = "%m")),
#                                                     GEAR=tacsatEflalo$LE_GEAR,
#                                                     LENCAT=tacsatEflalo$LENCAT,
#                                                     KWCAT=tacsatEflalo$KWCAT,
#                                                     Area=tacsatEflalo$Area
#                                                ),sum,na.rm=T)
# if (includeNBves)
#  table.Grid16                     <- merge(tableGrid16,aggregate(tacsatEflalo[,"VE_REF"],list(YEAR=an(format(tacsatEflalo$SI_DATIM, format = "%Y")),MONTH=an(format(tacsatEflalo$SI_DATIM, format = "%m")),GEAR=tacsatEflalo$LE_GEAR,LENCAT=tacsatEflalo$LENCAT,KWCAT=tacsatEflalo$KWCAT,Area=tacsatEflalo$Area),function(x) length(unique(x))))
# 
#- Generate table of eflaloNM effort
table.NM                          <- aggregate(eflaloNM[c(grep("KG",colnames(eflaloNM),value=T),grep("EURO",colnames(eflaloNM),value=T),"INTV")],
                                               list(YEAR=year(eflaloNM$FT_DDATIM),
                                                    MONTH=month(eflaloNM$FT_DDATIM),
                                                    GEAR=eflaloNM$LE_GEAR,
                                                    LENCAT=eflaloNM$LENCAT,
                                                    KWCAT=eflaloNM$KWCAT,
                                                    Area=eflaloNM$LE_RECT
                                               ),sum,na.rm=T)
if (includeNBves) 
  table.NM                        <- merge(table.NM, aggregate(eflaloNM["VE_REF"],
                                              list(YEAR=year(eflaloNM$FT_DDATIM),
                                                   MONTH=month(eflaloNM$FT_DDATIM),
                                                   GEAR=eflaloNM$LE_GEAR,
                                                   LENCAT=eflaloNM$LENCAT,
                                                   KWCAT=eflaloNM$KWCAT,
                                                   Area=eflaloNM$LE_RECT
                                              ),function(x) length(unique(x))))

#- Combine table of eflaloNM with tacsat-table
final.table                       <- rbind(table.N2000,table.NM)
#final.table                       <- rbind(table.N2000,table.Grid9,table.Grid16,table.NM)
save(final.table,file=paste(outPath,"/final.table.",Country,".Rdata",sep=""))

summary(final.table)
table(final.table$Area)




