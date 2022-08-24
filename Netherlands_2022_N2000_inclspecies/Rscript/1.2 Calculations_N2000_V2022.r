#- Clear workspace
rm(list=ls());

library(vmstools)#- code.google.com/p/vmstools
library(sp);library(rgdal);library(raster); library(lubridate)

#- Define relevant paths
gitPath     <- "C:/Users/hamon004/Github/VMSwork/datacallsVMSLogbook/Netherlands_2022_N2000"             #to be changed by lab
inPath     <- file.path(gitPath,"output")
shapePath  <- file.path(gitPath,"shapes")                                       #path were shape files are stored.
outPath    <- file.path(gitPath,"shareOutput")
#TacsateflaloPath  <- "W:/IMARES/Data/VMS_/BASEFILES/"                           #path were tacsatEflalo stands if used to define activities

#define country
Country       <- 'NLD'                                                          #adjust accordingly!
ICESrect      <- c('40F3','39F4','39F3','38F5','38F4','38F3','38F2','37F6','37F5','37F4','37F3','37F2','36F6','36F5','36F4','36F3','36F2','35F7','35F6','35F5','35F4','35F3','34F4','34F3','33F2','33F3','33F4','32F2','32F3','32F4','31F3','31F4')
years         <- 2014:2021
includeNBves  <- T                                                              # Number of vessels: change if you don't want to include the nb of vessels
#-------------------------------------------------------------------------------
#- 1a) load tacsat and eflalo files combined from the time period (output script 1.1)
#------------------------------------------------------------------------------- 
load(file=file.path(inPath,'eflaloNM.yrs.RData'))
load(file=file.path(inPath,'tacsatEflalo.yrs.RData'))
load(file=file.path(shapePath,"shapes.RData"))
#load(file=file.path(shapePath,"grd.RData"))


#-------------------------------------------------------------------------------
#- 2b) Produce  table containing number of vessels, effort, value and catch, by subareas per year.
#-------------------------------------------------------------------------------
grid16                 <- createGrid(xrange = c(2,8),
                                     yrange = c(51,56),
                                     resx   = 1/4,
                                     resy   = 1/8,
                                     exactBorder=T,type="SpatialGridDataFrame")
grid16@data$LE_RECT    <- ICESrectangle(data.frame(SI_LONG=coordinates(grid16)[,1],SI_LATI=coordinates(grid16)[,2]))
grid16@data$SI_LONG    <- coordinates(grid16)[,1]
grid16@data$SI_LATI    <- coordinates(grid16)[,2]
grid16@data$ID         <- 1:nrow(grid16@data)
grid16                 <- subset(grid16,LE_RECT %in% ICESrect)
grid16                 <- as(grid16,"SpatialPolygonsDataFrame")
proj4string(grid16)    <- CRS("+proj=longlat +datum=WGS84 +no_defs")

#- Categorize vessel lengths
tacsatEflalo$LENCAT    <- cut(tacsatEflalo$VE_LEN,breaks=c(0,12,18,24,100),labels=c("0-12","12-18","18-24",">24"))
eflaloNM$LENCAT        <- cut(eflaloNM$VE_LEN,breaks=c(0,12,18,24,100),labels=c("0-12","12-18","18-24",">24"))
tacsatEflalo$VE_KW     <- eflaloM$VE_KW[match(tacsatEflalo$FT_REF,eflaloM$FT_REF)]
tacsatEflalo$KWCAT     <- cut(tacsatEflalo$VE_KW,breaks=c(0,225,10000),labels=c("0-300",">300"))
eflaloNM$KWCAT         <- cut(eflaloNM$VE_KW,breaks=c(0,225,10000),labels=c("0-300",">300"))

#- determine which tacsateflalo records are located in the area and select these
pings   <- SpatialPoints(tacsatEflalo[c('SI_LONG','SI_LATI')],proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))
inArea  <- as.character(shapes@data$Gebied)[over(pings,as(shapes,"SpatialPolygons"))]
inGrid16<- as.character(grid16@data$ID)[over(pings,as(grid16,"SpatialPolygons"))]

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

# #- Perform aggregation for 1/16th grid
tacsatEflalo$Area                 <- inGrid16
tableGrid16                       <- aggregate(tacsatEflalo[c(grep("KG",colnames(tacsatEflalo)),grep("EURO",colnames(tacsatEflalo)),"INTV")],
                                               list(YEAR=an(format(tacsatEflalo$SI_DATIM, format = "%Y")),
                                                    MONTH=an(format(tacsatEflalo$SI_DATIM, format = "%m")),
                                                    GEAR=tacsatEflalo$LE_GEAR,
                                                    LENCAT=tacsatEflalo$LENCAT,
                                                    KWCAT=tacsatEflalo$KWCAT,
                                                    Area=tacsatEflalo$Area
                                               ),sum,na.rm=T)
if (includeNBves)
 table.Grid16                     <- merge(tableGrid16,aggregate(tacsatEflalo[,"VE_REF"],list(YEAR=an(format(tacsatEflalo$SI_DATIM, format = "%Y")),MONTH=an(format(tacsatEflalo$SI_DATIM, format = "%m")),GEAR=tacsatEflalo$LE_GEAR,LENCAT=tacsatEflalo$LENCAT,KWCAT=tacsatEflalo$KWCAT,Area=tacsatEflalo$Area),function(x) length(unique(x))))

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
final.table                       <- rbind(table.N2000,table.Grid16,table.NM)
save(final.table,file=paste(outPath,"/final.table.",Country,".Rdata",sep=""))

summary(final.table)
table(final.table$Area)




