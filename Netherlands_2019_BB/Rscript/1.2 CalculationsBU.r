#- Clear workspace
rm(list=ls());

library(vmstools)#- code.google.com/p/vmstools
library(sp);library(rgdal);library(raster); library(lubridate)

#- Define relevant paths
gitPath    <- "D:/Repository/datacallsVMSLogbook/Netherlands_2019_BB"             #to be changed by lab
inPath     <- file.path(gitPath,"output")
shapePath  <- file.path(gitPath,"shapes")                                         #path were shape files are stored.
outPath    <- file.path(gitPath,"shareOutput")

#define country
Country    <- 'NLD'                                                      #adjust accordingly!
ICESrect   <- c('34F2','33F2','34F3','33F3','34F4','33F4','32F3','35F4') #Selection of study area
ICESrectBR <- c('33F3','34F3','33F2')                                    #Selection of ICES rectangles with overlap in Brown Ridge
BBareas    <- c('A1_1','A1_2','A1_3','A2','B1','Total')
years      <- 2014:2017 

#-------------------------------------------------------------------------------
#- 1a) load tacsat and eflalo files combined from the time period (output script 1.1)
#------------------------------------------------------------------------------- 
load(file=paste(inPath,'tacsatp.yrs.RData',sep=''))
load(file=paste(inPath,'eflalo.yrs.RData',sep=''))
load(file=paste(shapePath,'BBtot.RData',sep=''))
load(file=paste(shapePath,'ICESR.RData',sep=''))
load(file=paste(shapePath,"Rect16.RData",sep=''))
load(file=paste(shapePath,"icesstatsq_ext.rda",sep=""))
load(file=paste(shapePath,"grdOverlabBR.RData",sep=""))

#-------------------------------------------------------
#- Trim eflalo to unique trips records dataset
#-------------------------------------------------------
eflalo.trp1           <- (aggregate(list(eflalo.yrs$LE_KG_BSS,eflalo.yrs$LE_KG_MUL,eflalo.yrs$LE_KG_SOL,eflalo.yrs$LE_KG_COD,eflalo.yrs$LE_KG_TOT,eflalo.yrs$INTV),
                                    list(eflalo.yrs$VE_REF,eflalo.yrs$FT_REF,eflalo.yrs$FT_DDATIM,eflalo.yrs$FT_LDATIM),sum,na.rm=T))
colnames(eflalo.trp1) <- c("VE_REF","FT_REF","FT_DDATIM","FT_LDATIM","LE_KG_BSS","LE_KG_MUL","LE_KG_SOL","LE_KG_COD","LE_KG_TOT","S.INTV")
eflalo.trp2           <- (aggregate(list(eflalo.yrs$LE_MSZ,eflalo.yrs$LE_WIDTH,eflalo.yrs$INTV),
                                    list(eflalo.yrs$VE_REF,eflalo.yrs$FT_REF,eflalo.yrs$FT_DDATIM,eflalo.yrs$FT_LDATIM),max,na.rm=T))
colnames(eflalo.trp2) <- c("VE_REF","FT_REF","FT_DDATIM","FT_LDATIM","LE_MSZ","LE_WIDTH","VE_DAS")
eflalo.trp            <- merge(eflalo.trp1,eflalo.trp2)
eflalo.trp            <- merge(eflalo.trp,MSZ,by="VE_REF")

#-------------------------------------------------------
#- Define gillnet trips categories based on MSZ and catch composition
#
#  Cat 1: Mullet & Seabass 
#  Cat 2: Sole
#  Cat 3: Cod 
#
#-------------------------------------------------------
#Make sure that LE_MSZ are defined 
#check
if(sum(is.na(eflalo.trp$LE_MSZ))>0) stop("missing meshsizes")
if(sum(eflalo.trp$LE_MSZ==-Inf)>0) stop("missing meshsizes")

eflalo.trp$CAT        <- NA
eflalo.trp$CAT[eflalo.trp$LE_KG_COD>eflalo.trp$LE_KG_SOL&eflalo.trp$LE_MSZ>=130]                        <- 3
eflalo.trp$CAT[eflalo.trp$LE_KG_SOL>=eflalo.trp$LE_KG_COD&eflalo.trp$LE_MSZ>=130]                       <- 2
eflalo.trp$CAT[eflalo.trp$LE_KG_SOL>=(eflalo.trp$LE_KG_BSS+eflalo.trp$LE_KG_MUL)&is.na(eflalo.trp$CAT)] <- 2
eflalo.trp$CAT[eflalo.trp$LE_KG_SOL<(eflalo.trp$LE_KG_BSS+eflalo.trp$LE_KG_MUL)&is.na(eflalo.trp$CAT)]  <- 1
eflalo.trp$CNT        <-1

#-------------------------------------------------------
#- Add gear width (=gillnet length) to dataset and fill NA's with assumptions; eg. CAT=2: 10-25 km gillnets
#-------------------------------------------------------
eflalo.trp[eflalo.trp$LE_WIDTH==-Inf,]$LE_WIDTH<-NA
if(eflalo.trp$LE_WIDTH > 25000) warnings("This means you have a gear length larger than 25km, not allowed for Dutch vessels, but perhaps for your country OK")

#gear width from logbooks
eflalo.trp$Min.net<-eflalo.trp$Max.net<-eflalo.trp$LE_WIDTH
#gear width assumptions for categories
eflalo.trp$Min.net[is.na(eflalo.trp$Min.net)&eflalo.trp$CAT==1]  <- 50   
eflalo.trp$Min.net[is.na(eflalo.trp$Min.net)&eflalo.trp$CAT==2]  <- 10000
eflalo.trp$Min.net[is.na(eflalo.trp$Min.net)&eflalo.trp$CAT==3]  <- 50
eflalo.trp$Max.net[is.na(eflalo.trp$Max.net)&eflalo.trp$CAT==1]  <- 2500
eflalo.trp$Max.net[is.na(eflalo.trp$Max.net)&eflalo.trp$CAT==2]  <- 25000
eflalo.trp$Max.net[is.na(eflalo.trp$Max.net)&eflalo.trp$CAT==3]  <- 5000

eflalo.trp     <- eflalo.trp[order(eflalo.trp$VE_REF,eflalo.trp$FT_DDATIM),]
eflalo.trp.yrs <- merge(eflalo.trp,unique(eflalo.yrs[,c("VE_REF","FT_REF","FT_DHAR","FT_LHAR")]),by=c("VE_REF","FT_REF"),all.y=F)

#-------------------------------------------------------------------------------
#- Calculate effort
#-------------------------------------------------------------------------------
eflalo.trp.yrs$TripDuration <- an(difftime(eflalo.trp.yrs$FT_LDATIM,eflalo.trp.yrs$FT_DDATIM,unit="day")*20/24)
eflalo.trp.yrs$effort.d     <- eflalo.trp.yrs$TripDuration
eflalo.trp.yrs$effort.min   <- eflalo.trp.yrs$effort.d*eflalo.trp.yrs$Min.net/1000
eflalo.trp.yrs$effort.max   <- eflalo.trp.yrs$effort.d*eflalo.trp.yrs$Max.net/1000

#-------------------------------------------------------------------------------
#- Merge calculations back to original eflalo dataset
#-------------------------------------------------------------------------------
eflalo.M4               <- (merge(eflalo.yrs[c("VE_REF","VE_ID","VE_FLT","VE_KW","FT_REF","FT_DHAR","FT_DDAT","FT_DTIME","FT_LDAT","FT_LTIME","LE_CDAT","LE_ID","LE_GEAR","LE_MSZ","LE_RECT",
                                               "LE_KG_TOT","LE_KG_BSS","LE_KG_MUL","LE_KG_SOL","LE_KG_COD","LE_KG_DAS",
                                               "LE_EURO_TOT","LE_EURO_SOL","LE_EURO_COD","LE_EURO_MUL",'LE_EURO_BSS',
                                               "INTV","nrRecords")],
                                  eflalo.trp.yrs[c("VE_REF","FT_REF","FT_DDATIM","FT_LDATIM","CAT","TripDuration","effort.d","effort.min","effort.max")],all.y=T))

eflalo.M4$LE_KG_TRP     <- an(eflalo.M4$TripDuration / eflalo.M4$nrRecords)
eflalo.M4$LE_KG_ED      <- an(eflalo.M4$effort.d     / eflalo.M4$nrRecords)
eflalo.M4$LE_KG_EMN     <- an(eflalo.M4$effort.min   / eflalo.M4$nrRecords)
eflalo.M4$LE_KG_EMX     <- an(eflalo.M4$effort.max   / eflalo.M4$nrRecords)

tacsatp.yrs$SI_YEAR     <- year(tacsatp.yrs$SI_DATIM)
tacsatp.yrs$SI_MONTH    <- month(tacsatp.yrs$SI_DATIM)
tacsatp.yrs$CAT         <- eflalo.M4$CAT[match(tacsatp.yrs$FT_REF,eflalo.M4$FT_REF)]
tacsatp.yrs             <- intervalTacsat(tacsatp.yrs,level="trip",fill.na=TRUE)
tacsatp.yrs$INTV[tacsatp.yrs$INTV>230] <- 115

eflalo.M4$INTV          <- eflalo.M4$INTV*24*60
eflalo.M4$SI_YEAR       <- year(eflalo.M4$FT_DDATIM)
eflalo.M4$SI_MONTH      <- month(eflalo.M4$FT_DDATIM)
eflalo.M4$LE_KG_DAS     <- eflalo.M4$effort.d

#-------------------------------------------------------------------------------
#- Distribute landings over fishing vms pings
#-------------------------------------------------------------------------------
#Make sure SI_STATE is defined! and fishing pings have '1'.
table(tacsatp.yrs$SI_STATE)
if(class(tacsatp.yrs$SI_STATE)=="character"){
  tacsatp.yrs$SI_STATE[which(tacsatp.yrs$SI_STATE=="f")] <- 1
  tacsatp.yrs$SI_STATE[which(tacsatp.yrs$SI_STATE!="f")] <- 0
}

#construct logbook datasets for VMS and none-VMS vessels.
tacsatp.yrs     <- tacsatp.yrs[!is.na(tacsatp.yrs$INTV),]
eflaloNM        <- subset(eflalo.M4,!FT_REF %in% unique(tacsatp.yrs$FT_REF))
eflaloM         <- subset(eflalo.M4, FT_REF %in% unique(tacsatp.yrs$FT_REF))

#Distribute landings over fishing pings
tacsatEflalo        <- (splitAmongPings(tacsat=subset(tacsatp.yrs,FT_REF %in% unique(eflaloM$FT_REF)),eflalo=eflaloM,variable="all",level="day",by="INTV",conserve=T))
head(tacsatEflalo);  tail(tacsatEflalo)
#check result
sum(tacsatEflalo$LE_KG_DAS)==sum(eflaloM$LE_KG_DAS)

#Identify tacsat (VMS) pings located in the Brown Ridge variants
tacsatEflaloSpatial <- SpatialPointsDataFrame(cbind(x=an(ac(tacsatEflalo$SI_LONG)),y=an(ac(tacsatEflalo$SI_LATI))),data=tacsatEflalo,proj4string =CRS("+proj=longlat +ellps=WGS84"))
tacsatEflalo$A1_1  <- !is.na(over(as(tacsatEflaloSpatial,"SpatialPoints"),as(BBtot[BBtot$Id=='A1_1',],"SpatialPolygons")))
tacsatEflalo$A1_2  <- !is.na(over(as(tacsatEflaloSpatial,"SpatialPoints"),as(BBtot[BBtot$Id=='A1_2',],"SpatialPolygons")))
tacsatEflalo$A1_3  <- !is.na(over(as(tacsatEflaloSpatial,"SpatialPoints"),as(BBtot[BBtot$Id=='A1_3',],"SpatialPolygons")))
tacsatEflalo$A2    <- !is.na(over(as(tacsatEflaloSpatial,"SpatialPoints"),as(BBtot[BBtot$Id=='A2',],"SpatialPolygons")))
tacsatEflalo$B1    <- !is.na(over(as(tacsatEflaloSpatial,"SpatialPoints"),as(BBtot[BBtot$Id=='B1',],"SpatialPolygons")))
tacsatEflalo$Total <- !is.na(over(as(tacsatEflaloSpatial,"SpatialPoints"),as(BBtot[BBtot$Id=='Total',],"SpatialPolygons")))
sum(tacsatEflalo$Total);sum(tacsatEflalo$A1_1);sum(tacsatEflalo$A1_2);sum(tacsatEflalo$A1_3);sum(tacsatEflalo$A2);sum(tacsatEflalo$B1);

#-------------------------------------------------------------------------------
#- Distribute effort of 'not merged' eflalo at 1/16th ICESrect scale
#-------------------------------------------------------------------------------

#- Delete records with NA's on places that are needed
idx         <- which(is.na(eflaloNM$VE_REF) == T | is.na(eflaloNM$LE_RECT) == T | is.na(eflaloNM$FT_DDATIM) == T)
if(length(idx)>0)  eflaloNM  <- eflaloNM[-idx,]

#-------------------------------------------------------------------------------
#- Calculate for each month the effort, KG and EUR (species & tot) per vessel & gear Category
#-------------------------------------------------------------------------------
eflaloAggNM <-  aggregate(cbind(eflaloNM$LE_KG_BSS  ,eflaloNM$LE_KG_MUL  ,eflaloNM$LE_KG_SOL  ,eflaloNM$LE_KG_COD  ,eflaloNM$LE_KG_TOT,
                                     eflaloNM$LE_EURO_BSS,eflaloNM$LE_EURO_MUL,eflaloNM$LE_EURO_SOL,eflaloNM$LE_EURO_COD,eflaloNM$LE_EURO_TOT,
                                     eflaloNM$LE_KG_DAS,eflaloNM$LE_KG_TRP,eflaloNM$LE_KG_ED,eflaloNM$LE_KG_EMN,eflaloNM$LE_KG_EMX),
                               by=list(eflaloNM$VE_ID,eflaloNM$CAT,eflaloNM$LE_RECT,eflaloNM$SI_YEAR, eflaloNM$SI_MONTH),FUN=sum,na.rm=T)
colnames(eflaloAggNM) <- c("VE_ID","CAT","LE_RECT","SI_YEAR","SI_MONTH",
                                "LE_KG_BSS","LE_KG_MUL","LE_KG_SOL","LE_KG_COD","LE_KG_TOT",
                                "LE_EURO_BSS","LE_EURO_MUL","LE_EURO_SOL","LE_EURO_COD","LE_EURO_TOT",
                                "LE_KG_DAS","LE_KG_TRP","LE_KG_ED","LE_KG_EMN","LE_KG_EMX")
#-------------------------------------------------------------------------------
#- Merge the value per rect, vessel, year with the proportion at sea
#-------------------------------------------------------------------------------
propSea                   <- Rect16@data[,c("ICESNAME","GridID","CENTER.LAT","CENTER.LON","propsea")]
colnames(propSea)         <- c("LE_RECT","LE_RECTID","SI_LATI","SI_LONG","propsea")
propSeaRect               <- aggregate(Rect16@data[,"propsea"],by=list(Rect16@data[,"ICESNAME"]),sum)
colnames(propSeaRect)     <- c("LE_RECT","propSeaRect")
propSeaRect$propSeaRect   <- propSeaRect$propSeaRect
propSea                   <- merge(propSea,propSeaRect,by="LE_RECT")
tacsatNMeflalo            <- merge(eflaloAggNM,propSea,by="LE_RECT",all.x=T,all.y=F)

for(iCol in c('LE_KG_DAS',"LE_KG_EMN","LE_KG_EMX",'LE_KG_BSS','LE_KG_MUL','LE_KG_SOL','LE_KG_COD','LE_KG_TOT',
                                                 'LE_EURO_BSS','LE_EURO_MUL','LE_EURO_SOL','LE_EURO_COD','LE_EURO_TOT')){
  idx                     <- which(names(tacsatNMeflalo) == iCol)
  tacsatNMeflalo[,idx]    <- tacsatNMeflalo[,idx] /tacsatNMeflalo$propSeaRect * tacsatNMeflalo$propsea
}

###Combine none-vms eflalo record with vms-eflalo records
col2keep  <- c("VE_ID","CAT","LE_KG_DAS","LE_KG_EMN","LE_KG_EMX",'LE_KG_BSS','LE_KG_MUL','LE_KG_SOL','LE_KG_COD','LE_KG_TOT',
               'LE_EURO_BSS','LE_EURO_MUL','LE_EURO_SOL','LE_EURO_COD','LE_EURO_TOT',
               "SI_LATI","SI_LONG","SI_YEAR","SI_MONTH")

TEcomb       <-rbindEflalo(tacsatNMeflalo[,col2keep],tacsatEflalo[,col2keep])
#define origin (tacsat or eflalo) for rows TEcomb
TEcomb$orign <-c(rep('eflalo',nrow(tacsatNMeflalo)),rep('tacsat',nrow(tacsatEflalo)))
#Add ices rectangle
TEcomb$RECT  <-ICESrectangle(TEcomb)
idx          <-which(icesstatsq[['label']] %in%ICESrect)
areaRef      <-lonLat2SpatialPolygons(lst=lapply(as.list(idx),
                                                function(x){data.frame(SI_LONG=icesstatsq$coordinates[[x]]$x,SI_LATI=icesstatsq$coordinates[[x]]$y)}))
bbox         <- bbox(areaRef)
spatBound    <- list(xrange = c(floor(range(bbox["x",])[1]),ceiling(range(bbox["x",])[2])),
                     yrange = c(floor(range(bbox["y",])[1]),ceiling(range(bbox["y",])[2])))

#- Define grid cell area
resx             <- 0.25
resy             <- 0.125
grd              <- createGrid(spatBound$x,spatBound$y,resx,resy,type="SpatialGridDataFrame",exactBorder=T)
subTEcomb        <- TEcomb[TEcomb$RECT%in%ICESrect,]
coords           <- SpatialPointsDataFrame(cbind(x=an(ac(subTEcomb$SI_LONG)),y=an(ac(subTEcomb$SI_LATI))),data=subTEcomb)
idx              <- over(as(coords,"SpatialPoints"),as(grd,"SpatialGrid"))
subTEcomb$gridID <- NA
subTEcomb$gridID <- idx
#add overlap brown ridge per grid block
subTEcomb <- merge(grdOverlabBR,subTEcomb, by='gridID', all.x=T,all.y=T)
subTEcomb[,c(names(grdOverlabBR[2:ncol(grdOverlabBR)]))][is.na(subTEcomb[,c(names(grdOverlabBR[2:ncol(grdOverlabBR)]))])] <- 0

#- Aggregate data on grid per origin (tacsat or eflalo), cat, gridID, year and month
result<-aggregate(list(subTEcomb$LE_KG_DAS,subTEcomb$LE_KG_EMN,subTEcomb$LE_KG_EMX,
                       subTEcomb$LE_KG_BSS,subTEcomb$LE_KG_MUL,subTEcomb$LE_KG_SOL,subTEcomb$LE_KG_COD,subTEcomb$LE_KG_TOT,
                       subTEcomb$LE_EURO_BSS,subTEcomb$LE_EURO_MUL,subTEcomb$LE_EURO_SOL,subTEcomb$LE_EURO_COD,subTEcomb$LE_EURO_TOT),
                  by=list(subTEcomb$orign, subTEcomb$CAT,subTEcomb$gridID,subTEcomb$SI_YEAR,subTEcomb$SI_MONTH),FUN=sum,na.rm=T)
names(result) <- c('orign','CAT','gridID','SI_YEAR','SI_MONTH','LE_KG_DAS','LE_KG_EMN','LE_KG_EMX',
                   'LE_KG_BSS','LE_KG_MUL','LE_KG_SOL','LE_KG_COD','LE_KG_TOT',
                   'LE_EURO_BSS','LE_EURO_MUL','LE_EURO_SOL','LE_EURO_COD','LE_EURO_TOT')
grdcoor       <-as.data.frame(coordinates(grd))
grdcoor$gridID<-rownames(grdcoor)
# load(file=paste(shapePath,'ShapeIcesRec.Rdata',sep=''))
# plot(Sices);axis(1);axis(2)
# text(grdcoor$s1,grdcoor$s2,grdcoor$gridID,cex=0.7)
# range(result$gridID)

#-------------------------------------------------------------------------------
#- Produce  table containing number of vessels, effort, and catch for total, subareas, all years and per year.
#-------------------------------------------------------------------------------
#Sum KG and EURO BSS and MUL in the different datafiles
TEcomb$LE_KG_BSSMUL         <- apply(TEcomb[c('LE_KG_BSS','LE_KG_MUL')],1,sum, na.rm=T)
TEcomb$LE_EURO_BSSMUL       <- apply(TEcomb[c('LE_EURO_BSS','LE_EURO_MUL')],1,sum, na.rm=T)
subTEcomb$LE_KG_BSSMUL      <- apply(subTEcomb[c('LE_KG_BSS','LE_KG_MUL')],1,sum, na.rm=T)
subTEcomb$LE_EURO_BSSMUL    <- apply(subTEcomb[c('LE_EURO_BSS','LE_EURO_MUL')],1,sum, na.rm=T)
tacsatEflalo$LE_KG_BSSMUL   <- apply(tacsatEflalo[c('LE_KG_BSS','LE_KG_MUL')],1,sum, na.rm=T)
tacsatEflalo$LE_EURO_BSSMUL <- apply(tacsatEflalo[c('LE_EURO_BSS','LE_EURO_MUL')],1,sum, na.rm=T)

#define columns
sumColsEffort <- c("LE_KG_DAS","LE_KG_EMN","LE_KG_EMX")
sumColsCatch  <- c("LE_KG_BSSMUL","LE_KG_SOL","LE_KG_COD","LE_KG_TOT","LE_EURO_BSSMUL","LE_EURO_SOL","LE_EURO_COD","LE_EURO_TOT")

#aggregate per brown ridge variant the effort per fishing cat, year and month using exact vms positions and logbook proportional overlap of gridID with BR variant
BRdata <- expand.grid(BBareas = ac(BBareas),year=ac(years),month = an(1:12), CAT = 1:3)
iC <- sumColsEffort[1]
for (iC in c(sumColsEffort,sumColsCatch))
{
  BRdata$NEW <- 0
  for (iR in 1:nrow(BRdata))
  {
    idxlog <- subTEcomb$orign == 'eflalo' & subTEcomb[,which(names(subTEcomb) == BRdata$BBareas[iR])] >0 & subTEcomb$CAT == BRdata$CAT[iR] & 
              subTEcomb$SI_YEAR == BRdata$year[iR] & subTEcomb$SI_MONTH == BRdata$month[iR]
    idxBRV <- which(names(subTEcomb) == BRdata$BBareas[iR])
    
    BRdata$NEWLog[iR] <- sum(subTEcomb[idxlog,iC] * subTEcomb[idxlog,idxBRV]) #logbook part, gridID * overlap GridID with Brown Ridge variant
    BRdata$NEWVMS[iR] <- sum(tacsatEflalo[tacsatEflalo$SI_YEAR == BRdata$year[iR] & tacsatEflalo$SI_MONTH == BRdata$month[iR] & 
                                            tacsatEflalo[,which(names(tacsatEflalo) ==BRdata$BBareas[iR])],iC],na.rm=T)
    BRdata$NEWTOT[iR] <- BRdata$NEWLog[iR]+BRdata$NEWVMS[iR]
    print(paste(iC,iR))
  }
  names(BRdata)[which(names(BRdata) == 'NEWLog')] <- paste('Log_',iC,sep='')
  names(BRdata)[which(names(BRdata) == 'NEWVMS')] <- paste('Vms_',iC,sep='')
  names(BRdata)[which(names(BRdata) == 'NEWTOT')] <- paste('both_',iC,sep='')
}

#-make a data frame to store effort and catch for total period and per year
Stat <- data.frame(Year=ac(NA),Country=ac(NA),Area=ac(NA),
                   VMSships=an(NA),VMSdays=an(NA),VMS_EMN=an(NA),VMS_EMX=an(NA),
                   LOGships=an(NA),LOGdays=an(NA),LOG_EMN=an(NA),LOG_EMX=an(NA),
                   COMships=an(NA),COMdays=an(NA),COM_EMN=an(NA),COM_EMX=an(NA),
                   KG_BSSMUL=an(NA),KG_SOL = an(NA),KG_COD=an(NA),KG_TOT = an(NA),
                   EURO_BSSMUL=an(NA),EURO_SOL = an(NA),EURO_COD=an(NA),EURO_TOT = an(NA))
Stat$Year <- as.character(Stat$Year);Stat$Country <- ac(Stat$Country);Stat$Area <- ac(Stat$Area)

#store totals
idxvms <- TEcomb$orign == 'tacsat'
idxlog <- TEcomb$orign == 'eflalo'
Stat[nrow(Stat)+1,] <- append(append(append(append(append(append(list('all_yrs',Country,'GreaterNorthSea',an(length(unique(TEcomb$VE_ID[idxvms])))),
                                                                 unname(apply(TEcomb[idxvms,sumColsEffort],2,sum,na.rm=T))),
                                                          an(length(unique(TEcomb$VE_ID[idxlog])))),unname(apply(TEcomb[idxlog,sumColsEffort],2,sum,na.rm=T))),
                                            an(length(unique(TEcomb$VE_ID)))),unname(apply(TEcomb[sumColsEffort],2,sum,na.rm=T))),
                              unname(apply(TEcomb[sumColsCatch],2,sum,na.rm=T)))

for (iY in years)
{
  idxvms <- TEcomb$orign == 'tacsat' & TEcomb$SI_YEAR == iY
  idxlog <- TEcomb$orign == 'eflalo' & TEcomb$SI_YEAR == iY
  idxtot <- TEcomb$SI_YEAR == iY
  Stat[nrow(Stat)+1,] <- append(append(append(append(append(append(list(ac(iY),Country,'GreaterNorthSea',an(length(unique(TEcomb$VE_ID[idxvms])))),
                                                                   unname(apply(TEcomb[idxvms,sumColsEffort],2,sum,na.rm=T))),
                                                            an(length(unique(TEcomb$VE_ID[idxlog])))),unname(apply(TEcomb[idxlog,sumColsEffort],2,sum,na.rm=T))),
                                              an(length(unique(TEcomb$VE_ID[idxtot])))),unname(apply(TEcomb[idxtot,sumColsEffort],2,sum,na.rm=T))),
                                unname(apply(TEcomb[idxtot,sumColsCatch],2,sum,na.rm=T)))
}
#Store ICES areas - eflalo
Lareas <- list(ICESrectBR,ICESrect)
for (iL in 1:length(Lareas))
{
  idxvms <- subTEcomb$RECT %in% Lareas[[iL]] & subTEcomb$orign == 'tacsat'
  idxlog <- subTEcomb$RECT %in% Lareas[[iL]] & subTEcomb$orign == 'eflalo'
  idxtot <- subTEcomb$RECT %in% Lareas[[iL]]
  Stat[nrow(Stat)+1,] <- append(append(append(append(append(append(list('all_yrs',Country,paste(Lareas[[iL]],collapse = "_"),an(length(unique(subTEcomb$VE_ID[idxvms])))),
                                                                   unname(apply(subTEcomb[idxvms,sumColsEffort],2,sum,na.rm=T))),
                                                            an(length(unique(subTEcomb$VE_ID[idxlog])))),unname(apply(subTEcomb[idxlog,sumColsEffort],2,sum,na.rm=T))),
                                              an(length(unique(subTEcomb$VE_ID[idxtot])))),unname(apply(subTEcomb[idxtot,sumColsEffort],2,sum,na.rm=T))),
                                unname(apply(subTEcomb[idxtot,sumColsCatch],2,sum,na.rm=T)))
  
  for (iY in years)
  {
    idxvms <- subTEcomb$RECT %in% Lareas[[iL]] & subTEcomb$orign == 'tacsat' & subTEcomb$SI_YEAR ==iY
    idxlog <- subTEcomb$RECT %in% Lareas[[iL]] & subTEcomb$orign == 'eflalo'& subTEcomb$SI_YEAR ==iY
    idxtot <- subTEcomb$RECT %in% Lareas[[iL]] & subTEcomb$SI_YEAR ==iY
    Stat[nrow(Stat)+1,] <- append(append(append(append(append(append(list(ac(iY),Country,paste(Lareas[[iL]],collapse = "_"),an(length(unique(subTEcomb$VE_ID[idxvms])))),
                                                                     unname(apply(subTEcomb[idxvms,sumColsEffort],2,sum,na.rm=T))),
                                                              an(length(unique(subTEcomb$VE_ID[idxlog])))),unname(apply(subTEcomb[idxlog,sumColsEffort],2,sum,na.rm=T))),
                                                an(length(unique(subTEcomb$VE_ID[idxtot])))),unname(apply(subTEcomb[idxtot,sumColsEffort],2,sum,na.rm=T))),
                                  unname(apply(subTEcomb[idxtot,sumColsCatch],2,sum,na.rm=T)))
  }}
#Store Brown Ridge areas - tacsat
iL <- 'A1_1'
for (iL in BBareas)
{
  idxvms   <- tacsatEflalo[,which(names(tacsatEflalo) == iL)]
  idxlog   <- subTEcomb$orign == 'eflalo'
  idxBRV   <- which(names(subTEcomb) == iL)
  mOverlap <- mean(grdOverlabBR[,which(names(grdOverlabBR) ==iL)],na.rm=T)
  Stat[nrow(Stat)+1,] <- append(append(append(append(list('all_yrs',Country,iL,
                                                          an(length(unique(tacsatEflalo$VE_ID[idxvms])))),
                                                     unname(apply(tacsatEflalo[idxvms,sumColsEffort],2,sum,na.rm=T))),
                                              
                                              an(length(unique(subTEcomb$VE_ID[idxlog & subTEcomb[,idxBRV]>0]))*mOverlap)),
                                       unname(apply(subTEcomb[idxlog,sumColsEffort]* subTEcomb[idxlog,idxBRV],2,sum,na.rm=T))),
                                
                                rep(an(NA),1+length(sumColsEffort)+length(sumColsCatch)))
  
  for (iY in years)
  {
    idxvms   <- tacsatEflalo[,which(names(tacsatEflalo) == iL)] & tacsatEflalo$SI_YEAR == iY
    idxlog   <- subTEcomb$orign == 'eflalo' & subTEcomb$SI_YEAR == iY
    idxtot   <- subTEcomb$SI_YEAR == iY
    idxBRV   <- which(names(subTEcomb) == iL)
    mOverlap <- mean(grdOverlabBR[,which(names(grdOverlabBR) ==iL)],na.rm=T)
    Stat[nrow(Stat)+1,] <- append(append(append(append(list(ac(iY),Country,iL,
                                                            an(length(unique(tacsatEflalo$VE_ID[idxvms])))),
                                                       unname(apply(tacsatEflalo[idxvms,sumColsEffort],2,sum,na.rm=T))),
                                                
                                                an(length(unique(subTEcomb$VE_ID[idxlog & subTEcomb[,idxBRV]>0]))*mOverlap)),
                                         unname(apply(subTEcomb[idxlog,sumColsEffort]* subTEcomb[idxlog,idxBRV],2,sum,na.rm=T))),
                                  
                                  rep(an(NA),1+length(sumColsEffort)+length(sumColsCatch)))
  }}
#Remove rows with only NA
Stat <- Stat[apply(is.na(Stat),1,sum) != ncol(Stat),]

#sum VMS and logbook in Brown ridge variants
idx                  <- Stat$Area %in% BBareas
#effort
Stat$COMships[idx]   <- Stat$VMSships[idx] + Stat$LOGships[idx] 
Stat$COMdays[idx]    <- Stat$VMSdays[idx]  + Stat$LOGdays[idx]
Stat$COM_EMN[idx]    <- Stat$VMS_EMN[idx]  + Stat$LOG_EMN[idx]
Stat$COM_EMX[idx]    <- Stat$VMS_EMX[idx]  + Stat$LOG_EMX[idx]
#catch
for (iL in BBareas)
{
  #totalen
  idxvms   <- tacsatEflalo[,which(names(tacsatEflalo) == iL)]
  idxlog   <- subTEcomb$orign == 'eflalo' &  subTEcomb[,which(names(subTEcomb) == iL)]>0
  idxBRV   <- which(names(subTEcomb) == iL)
  Stat[Stat$Year == 'all_yrs' & Stat$Area == iL,substr(sumColsCatch,4,nchar(sumColsCatch))] <- apply(tacsatEflalo[idxvms,sumColsCatch],2,sum,na.rm=T) +  apply(subTEcomb[idxlog,sumColsCatch]* subTEcomb[idxlog,idxBRV],2,sum,na.rm=T)
  
  for (iY in years)
  {
    idxvms   <- tacsatEflalo[,which(names(tacsatEflalo) == iL)] & tacsatEflalo$SI_YEAR == iY
    idxlog   <- subTEcomb$orign == 'eflalo' &  subTEcomb[,which(names(subTEcomb) == iL)]>0 & subTEcomb$SI_YEAR == iY
    idxBRV   <- which(names(subTEcomb) == iL)
    Stat[Stat$Year == ac(iY) & Stat$Area == iL,substr(sumColsCatch,4,nchar(sumColsCatch))] <- apply(tacsatEflalo[idxvms,sumColsCatch],2,sum,na.rm=T) +  apply(subTEcomb[idxlog,sumColsCatch]* subTEcomb[idxlog,idxBRV],2,sum,na.rm=T)
  }}

#remove ships in logbook with records in vms
Stat$LOGships <- Stat$COMships-Stat$VMSships

#### Save results
save(result,file=paste(outPath,"Results",Country,".RData",sep=''))
save(BRdata,file=paste(outPath,"BRdata",Country,".RData",sep=''))
save(Stat,file=paste(outPath,"Stat",Country,".RData",sep=''))

