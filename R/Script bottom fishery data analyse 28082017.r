rm(list=ls())
library(vmstools)
require(sp)
#-------------------------------------------------------------------------------
#- Settings: make sure paths and gears are correct for each country specific
#-------------------------------------------------------------------------------

#- Set the working directory to the folder where you keep your code and data
sysPa       <-"W:/IMARES/Data/VMS_/2017/MKBA_extension/"
#- Settings paths
eflaloPath  <- paste(sysPa,"../../BASEFILES/",sep="")
tacsatPath  <- paste(sysPa,"../../BASEFILES/",sep="")
dataPath    <- paste(sysPa,"Data/",sep="")
polygonPath <- paste(sysPa,"Data/",sep="")
outPath     <- paste(sysPa,"Results/",sep="")
resPath     <- paste(sysPa,"Results/",sep="")

#- Set the country abbreviation to e.g. deu, gbr, dnk
country     <- "nld"
interval    <- 115 #set interval time applicable for your country VMS

#- Set the gearnames according to the ones you use for bottom-trawling and gillnetting
BotGears    <- c("TBB", # Beanm trawl
                 "OTB", # Otter trawl
                 "OTT", # Otter twin-trawl
                 "SSC", # Scottisch seine
                 "SDN", # Danish seine
                 "PTB", # Pair bottom trawl
                 "HMD", # Mechanical dredge
                 "DRB") # Dredge
GilGears    <- c("GN",  # Gillnet
                 "GNS", # Gillnet
                 "GTR", # Gillnet
                 "GTN") # Gillnet
Gears       <- c(BotGears,GilGears)

#- We request data for the years 2010 - 2016

#-------------------------------------------------------------------------------
#- Load the data and process them in a simple manner
#-------------------------------------------------------------------------------
for (Year in c(2010:2016)) {
  cat(Year,"\n")
  Years     <- Year
  #-read Eflalo & Tacsat files; Seperate scripts for cleaning is included
  load(file.path(paste(eflaloPath,"cleanEflalo",Years,".RData",sep="")))  #named eflalo as an object
  load(file.path(paste(tacsatPath,"cleanTacsat",Years,".RData",sep="")))  #named eflalo as an object

  tacsat <- formatTacsat(tacsat) # format each of the columns to the specified class
  eflalo <- formatEflalo(eflalo) # format each of the columns to the specified class
  
  #- Relevant country
  tacsat <- tacsat[grep(country, tacsat$VE_COU),]
  eflalo <- eflalo[grep(country, eflalo$VE_COU),]

  #- Relevant ICES rectangles
  ICESrect    <- c("35F2","35F3","35F4","35F5",
                   "36F2","36F3","36F4","36F5",
                   "37F2","37F3","37F4","37F5",
                   "38F2","38F3","38F4","38F5",
                   "39F2","39F3","39F4","39F5")

  #- Relevant gears
  eflaloRef   <- subset(eflalo,LE_GEAR %in% Gears & LE_RECT %in% ICESrect)
  eflalo      <- subset(eflalo,FT_REF %in% unique(eflaloRef$FT_REF))

  # select the top 10 species in the ICES rect (hopefully over the years the top 5 are in the annual top 10)
  top10spp <- names(sort(-colSums(eflalo[c(grep("KG",colnames(eflalo)))],na.rm=T))[1:10])
  top10spp <- c(top10spp,paste0("LE_EURO_",substr(top10spp,7,9)))

  #Calculate total landings
  eflalo$LE_KG_TOT  <- rowSums(eflalo[c(grep("KG",colnames(eflalo)))],na.rm=T)
  eflalo$LE_EURO_TOT<- rowSums(eflalo[c(grep("EURO",colnames(eflalo)))],na.rm=T)
  eflalo      <- cbind(eflalo[c("FT_REF","VE_REF","VE_ID","VE_FLT","VE_COU","VE_LEN","VE_KW","VE_TON",
                                "FT_DCOU","FT_DHAR","FT_DDAT","FT_DTIME","FT_LCOU","FT_LHAR","FT_LDAT","FT_LTIME",
                                "LE_ID","LE_CDAT","LE_STIME","LE_ETIME","LE_SLAT","LE_SLON","LE_ELAT","LE_ELON",
                                "LE_GEAR","LE_WIDTH","LE_MSZ","LE_RECT","LE_DIV","LE_SUBDIV")],
                       eflalo[c("LE_KG_TOT","LE_EURO_TOT",top10spp)])

  eflalo$FT_DDATIM  <- as.POSIXct(paste(eflalo$FT_DDAT,eflalo$FT_DTIME, sep = " "),
                                  tz = "GMT", format = "%d/%m/%Y  %H:%M")
  eflalo$FT_LDATIM  <- as.POSIXct(paste(eflalo$FT_LDAT,eflalo$FT_LTIME, sep = " "),
                                  tz = "GMT", format = "%d/%m/%Y  %H:%M")

#-------------------------------------------------------------------------------
#- Calculate effort (days)
#-------------------------------------------------------------------------------
  eflalo$INTV           <- difftime(eflalo$FT_LDATIM,eflalo$FT_DDATIM,units="days")
  eflalo$dummy          <- 1
  res                   <- aggregate(eflalo$dummy,by=list(eflalo$FT_REF),FUN=sum,na.rm=T)
  colnames(res)         <- c("FT_REF","nrRecord")
  eflalo                <- merge(eflalo,res,by="FT_REF")
  eflalo$INTV           <- eflalo$INTV / eflalo$nrRecord
  eflalo$LE_KG_DAS      <- eflalo$INTV

  #- Merge eflalo and tacsat together
  tacsatp               <- mergeEflalo2Tacsat(eflalo,tacsat)
  #- Assign gear and length to tacsat
  tacsatp$LE_GEAR       <- eflalo$LE_GEAR[match(tacsatp$FT_REF,eflalo$FT_REF)]
  tacsatp$VE_LEN        <- eflalo$VE_LEN[match(tacsatp$FT_REF,eflalo$FT_REF)]

  #- Only keep Eflalo gears from tacsat
  tacsatp               <- subset(tacsatp,FT_REF != 0)
  #- Get time between pings
  tacsatp               <- intervalTacsat(tacsatp,level="trip",fill.na=TRUE)
  tacsatp$INTV[is.na(tacsatp$INTV)] <- interval
  tacsatp$INTV[tacsatp$INTV>230]    <- 2*interval
  eflalo$INTV           <- as.numeric(eflalo$INTV*24*60)
  eflalo$LE_KG_DAS      <- as.numeric(eflalo$LE_KG_DAS)
  
  #- Define activitity based on vesselspeed
  tacsatp               <- tacsatp[!is.na(tacsatp$SI_SP),]

  #- Read in fixed speed bounds for different gears
  # !!! Make sure that all your gears are added to this speedarr.csv list, some may be lacking !!! #
  
  speedarr              <- read.csv(file=paste(dataPath,"/speedarr.csv",sep=""))
  colnames(speedarr)    <- c("LE_GEAR","min","max")
  speedarr$LE_GEAR      <- ac(speedarr$LE_GEAR)
  speedarr              <- speedarr[speedarr$LE_GEAR%in%unique(tacsatp$LE_GEAR),]

  #-------------------------------------------------------------------------------
  #- Assign the activity per gear
  #-------------------------------------------------------------------------------
  gears                 <- unique(tacsatp$LE_GEAR)
  tacsatp$SI_STATE<-0
  for (mm in gears) {
    tacsatp[tacsatp$LE_GEAR==mm& tacsatp$SI_SP >= an(speedarr[speedarr$LE_GEAR==mm,"min"])& tacsatp$SI_SP <= an(speedarr[speedarr$LE_GEAR==mm,"max",]),]$SI_STATE<-1
  }

  tacsatp               <- tacsatp[!is.na(tacsatp$INTV),]
  #- Split eflalo up in two sets, one set that cannot be merged to tacsat and one which matches
  eflaloNM              <- subset(eflalo,!FT_REF %in% unique(tacsatp$FT_REF))
  eflaloM               <- subset(eflalo, FT_REF %in% unique(tacsatp$FT_REF))
  
  #- Split effort and total landing among pings
  tacsatEflalo          <- (splitAmongPings(tacsat=subset(tacsatp,FT_REF %in% unique(eflaloM$FT_REF)),eflalo=subset(eflaloM),variable="KG",level="day",by="INTV",conserve=T))
  cat("total value in eflalo\n")
  print(colSums(eflaloM[c(grep("KG",colnames(eflaloM)))],na.rm=T))
  cat("total value in tacsatEflalo\n")
  print(colSums(tacsatEflalo[c(grep("KG",colnames(tacsatEflalo)))],na.rm=T))
  cat("ratio total value\n")
  print(colSums(eflaloM[c(grep("KG",colnames(eflaloM)))],na.rm=T)/ colSums(tacsatEflalo[c(grep("KG",colnames(tacsatEflalo)))],na.rm=T))


    #- Combine both merged and non-merged datasets of the year
  if (Year==2010) {
    tacsatEflalo.Comb   <- tacsatEflalo
    eflaloNM.Comb       <- eflaloNM
    eflaloM.Comb        <- eflaloM
  }
  if (!Year==2010) {
    tacsatEflalo.Comb   <- rbindTacsat(tacsatEflalo.Comb,tacsatEflalo)
    eflaloNM.Comb       <- rbindEflalo(eflaloNM.Comb,eflaloNM)
    eflaloM.Comb        <- rbindEflalo(eflaloM.Comb,eflaloM)
  }
  rm(tacsatEflalo); rm(eflalo); rm(tacsat); rm(eflaloNM); rm(eflaloM); gc(); flush.console()
}

#- Retain only data inside ICES rectangles
tacsatEflalo.Comb           <- subset(tacsatEflalo.Comb,LE_RECT %in% ICESrect)
eflaloNM.Comb               <- subset(eflaloNM.Comb,LE_RECT %in% ICESrect)
eflaloM.Comb                <- subset(eflaloM.Comb,LE_RECT %in% ICESrect)

#- Categorize vessel lengths
tacsatEflalo.Comb$LENCAT    <- cut(tacsatEflalo.Comb$VE_LEN,breaks=c(0,12,18,24,100),labels=c("0-12","12-18","18-24",">24"))
eflaloNM.Comb$LENCAT        <- cut(eflaloNM.Comb$VE_LEN,breaks=c(0,12,18,24,100),labels=c("0-12","12-18","18-24",">24"))
eflaloM.Comb$LENCAT         <- cut(eflaloM.Comb$VE_LEN, breaks=c(0,12,18,24,100),labels=c("0-12","12-18","18-24",">24"))

#- Get some statistics on merged and non-merged data
tacsatEflalo.Comb$SI_YEAR <- an(format(tacsatEflalo.Comb$SI_DATIM, format = "%Y"))
tapply(tacsatEflalo.Comb$LE_KG_DAS,list(tacsatEflalo.Comb$SI_YEAR),FUN=sum)
tapply(eflaloNM.Comb$LE_KG_DAS,list(format(eflaloNM.Comb$FT_DDATIM,"%Y")),FUN=sum)

#- Save the objects
save(tacsatEflalo.Comb, file=file.path(outPath,paste("tacsatEflalo.Comb.Rdata",sep="")))  #named eflalo as an object
save(eflaloNM.Comb,     file=file.path(outPath,paste("eflaloNM.Comb.Rdata",    sep="")))  #named eflalo as an object
save(eflaloM.Comb,      file=file.path(outPath,paste("eflaloM.Comb.Rdata",     sep="")))  #named eflalo as an object


#-------------------------------------------------------------------------------
#- Start overlaying procedure with shapefiles
#-------------------------------------------------------------------------------

#- Load the files
load(file.path(outPath,paste("tacsatEflalo.Comb.Rdata",sep="")))  #named eflalo as an object
load(file.path(outPath,paste("eflaloNM.Comb.Rdata",sep="")))  #named eflalo as an object
load(file.path(outPath,paste("eflaloM.Comb.Rdata",sep="")))  #named eflalo as an object

#- Read in polygon data of the proposed areas'
polnames <- c("FrysianFront",
              "CleaverBank",
              "FrysianFrontCentralOyster",
              "ICES")
load(file.path(polygonPath,"shapes28082017.RData"))

# Analyse VMS for bottomtrawling
pings  <- SpatialPoints(tacsatEflalo.Comb[c('SI_LONG','SI_LATI')],proj4string=CRS("+proj=longlat +ellps=WGS84"))
tacsatEflalo.Comb$Area              <- NA

for (i in c(1:length(polnames))) {

  shape  <- shapes[[polnames[i]]]
  proj4string(shape) <- proj4string(pings)
  #- determine which tacsateflalo records are located in the area and select these
  inArea <- which(is.na(over(pings,shape))==F)

  if(length(inArea)>0){
    tacsatEflalo.Comb[inArea,"Area"]  <- polnames[i]

    table                             <- aggregate(tacsatEflalo.Comb[c(grep("KG",colnames(tacsatEflalo.Comb)),grep("EURO",colnames(tacsatEflalo.Comb)))],
                                                 list(YEAR=an(format(tacsatEflalo.Comb$SI_DATIM, format = "%Y")),
                                                      GEAR=tacsatEflalo.Comb$LE_GEAR,
                                                      LENCAT=tacsatEflalo.Comb$LENCAT,
                                                      Area=tacsatEflalo.Comb$Area
                                                      ),sum,na.rm=T)
    if (i==1) table.TaEfl           <- table
    if (i>1) table.TaEfl            <- rbind(table.TaEfl,table)
    tacsatEflalo.Comb$Area          <- NA #reset
  }
}
table.TaEfl$source                <- "tacsatEflalo"

#- Generate table of eflaloNM effort
table.NMEfl                       <- aggregate(eflaloNM.Comb[c(grep("KG",colnames(eflaloNM.Comb)),grep("EURO",colnames(eflaloNM.Comb)))],
                                               list(Area=eflaloNM.Comb$LE_RECT,
                                                    YEAR=format(eflaloNM.Comb$FT_DDATIM,"%Y"),
                                                    GEAR=eflaloNM.Comb$LE_GEAR,
                                                    LENCAT=eflaloNM.Comb$LENCAT
                                               ),sum,na.rm=T)
table.NMEfl$source                <- "eflaloNM"

# Analyse VMS & Logbook for gillnetting
for (i in c(1:length(polnames))[grep("FrysianFrontCentralOyster",polnames)]) {

  shape <- shapes[[polnames[i]]]
  #- determine which tacsateflalo records are located in the area and select these\
  tacsatEflalo.Comb                 <- subset(tacsatEflalo.Comb,an(format(tacsatEflalo.Comb$SI_DATIM,"%m")) %in% c(6:11) & LE_GEAR %in% GilGears)
  eflaloNM.Comb                     <- subset(eflaloNM.Comb,    an(format(eflaloNM.Comb$FT_LDATIM,"%m"))    %in% c(6:11) & LE_GEAR %in% GilGears)
  eflaloM.Comb                      <- subset(eflaloM.Comb,     an(format(eflaloM.Comb$FT_LDATIM,"%m"))     %in% c(6:11) & LE_GEAR %in% GilGears)
  pings                             <- SpatialPoints(tacsatEflalo.Comb[c('SI_LONG','SI_LATI')],proj4string=CRS("+proj=longlat +ellps=WGS84"))
  inArea                            <- which(is.na(over(pings,shape))==F)

  tacsatEflalo.Comb$Area            <- NA
  if(length(inArea)>0)
    tacsatEflalo.Comb[inArea,"Area"]<- polnames[i]
  eflaloNM.Comb$Area                <- eflaloNM.Comb$LE_RECT
  eflaloM.Comb$Area                 <- eflaloM.Comb$LE_RECT

  table1                            <- aggregate(tacsatEflalo.Comb[c(grep("KG",colnames(tacsatEflalo.Comb)),grep("EURO",colnames(tacsatEflalo.Comb)))],
                                               list(YEAR=an(format(tacsatEflalo.Comb$SI_DATIM, format = "%Y")),
                                                    GEAR=tacsatEflalo.Comb$LE_GEAR,
                                                    LENCAT=tacsatEflalo.Comb$LENCAT,
                                                    Area=tacsatEflalo.Comb$Area
                                                    ),sum,na.rm=T)
  table1                            <- cbind(table1,source="tacsatEflalo")

  table2                            <- aggregate(eflaloNM.Comb[c(grep("KG",colnames(eflaloNM.Comb)),grep("EURO",colnames(eflaloNM.Comb)))],
                                               list(YEAR=an(format(eflaloNM.Comb$FT_LDATIM, format = "%Y")),
                                                    GEAR=eflaloNM.Comb$LE_GEAR,
                                                    LENCAT=eflaloNM.Comb$LENCAT,
                                                    Area=eflaloNM.Comb$Area
                                                    ),sum,na.rm=T)
  table2                            <- cbind(table2,source="eflaloNM")


  table3                            <- aggregate(eflaloM.Comb[c(grep("KG",colnames(eflaloM.Comb)),grep("EURO",colnames(eflaloM.Comb)))],
                                               list(YEAR=an(format(eflaloM.Comb$FT_LDATIM, format = "%Y")),
                                                    GEAR=eflaloM.Comb$LE_GEAR,
                                                    LENCAT=eflaloM.Comb$LENCAT,
                                                    Area=eflaloM.Comb$Area
                                                    ),sum,na.rm=T)
  table3                            <- cbind(table3,source="eflaloM")
  table.Gi                          <- rbind(table1,table2,table3)

  table.Gi$Area                     <- paste0(polnames[i],"_gillnet")
}

#- Combine table of eflaloNM with tacsat-table
final.table                       <- rbind(table.TaEfl,table.NMEfl[colnames(table.TaEfl)],table.Gi[colnames(table.TaEfl)])
save(final.table,file=paste(resPath,"final.table.",country,".Rdata",sep=""))
summary(final.table)


#-------------------------------------------------------------------------------
#- Get data for maps
#-------------------------------------------------------------------------------

#- Load the data again as some subsetting has taken place before
load(file.path(outPath,paste("tacsatEflalo.Comb.Rdata",sep="")))  #named eflalo as an object
load(file.path(outPath,paste("eflaloNM.Comb.Rdata",sep="")))  #named eflalo as an object
load(file.path(outPath,paste("eflaloM.Comb.Rdata",sep="")))  #named eflalo as an object

#- create a 1/16th ICES rectangle grid to aggregate the values over
xrange                            <- c(2,6)
yrange                            <- c(53,55.5)
resx                              <- 1/4
resy                              <- 1/8
grd                               <- createGrid(xrange,yrange,resx,resy,type="SpatialGrid",exactBorder=T)
#- add grid ID to tacsat file
spTa                              <- SpatialPoints(data.frame(SI_LONG=tacsatEflalo.Comb$SI_LONG,SI_LATI=tacsatEflalo.Comb$SI_LATI))
tacsatEflalo.Comb$grID            <- over(spTa,grd)
coordsGrd                         <- coordinates(grd)
colnames(coordsGrd)               <- c("SI_LONG","SI_LATI")
coordsGrd                         <- as.data.frame(cbind(coordsGrd,LE_RECT=ICESrectangle(coordsGrd)))
coordsGrd$grID                    <- 1:nrow(coordsGrd)
#- add grid ID to eflalo files
eflaloNM.Comb                     <- merge(eflaloNM.Comb,coordsGrd[,c("LE_RECT","grID")],by="LE_RECT",all.x=T)
eflaloNM.Comb[,kgeur(colnames(eflaloNM.Comb))] <- eflaloNM.Comb[,kgeur(colnames(eflaloNM.Comb))]/16
eflaloM.Comb                      <- merge(eflaloM.Comb,coordsGrd[,c("LE_RECT","grID")],by="LE_RECT",all.x=T)
eflaloM.Comb[,kgeur(colnames(eflaloM.Comb))]   <- eflaloM.Comb[,kgeur(colnames(eflaloM.Comb))]/16

aggTE                             <- aggregate(tacsatEflalo.Comb[,c("LE_KG_TOT","LE_EURO_TOT","LE_KG_DAS")],
                                               by=list(YEAR=an(format(tacsatEflalo.Comb$SI_DATIM, format = "%Y")),
                                                       GEAR=tacsatEflalo.Comb$LE_GEAR,
                                                       grID=tacsatEflalo.Comb$grID
                                                       ),sum,na.rm=T)
aggENM                            <- aggregate(eflaloNM.Comb[,c("LE_KG_TOT","LE_EURO_TOT","LE_KG_DAS")],
                                               by=list(YEAR=an(format(eflaloNM.Comb$FT_LDATIM, format = "%Y")),
                                                       GEAR=eflaloNM.Comb$LE_GEAR,
                                                       grID=eflaloNM.Comb$grID
                                                       ),sum,na.rm=T)
aggEM                             <- aggregate(eflaloM.Comb[,c("LE_KG_TOT","LE_EURO_TOT","LE_KG_DAS")],
                                               by=list(YEAR=an(format(eflaloM.Comb$FT_LDATIM, format = "%Y")),
                                                       GEAR=eflaloM.Comb$LE_GEAR,
                                                       grID=eflaloM.Comb$grID
                                                       ),sum,na.rm=T)
                                                       
combAgg                           <- rbind(aggTE,aggENM)
aggTot                            <- aggregate(combAgg[,c("LE_KG_TOT","LE_EURO_TOT","LE_KG_DAS")],
                                               by=as.list(combAgg[,c("YEAR","GEAR","grID")]),FUN=sum,na.rm=T)
save(aggTot,aggTe,aggENM,aggEM,file=file.path(outPath,paste0("mapdata_",country,".RData")))