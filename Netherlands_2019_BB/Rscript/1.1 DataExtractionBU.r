#- Clear workspace
rm(list=ls());
.libPaths("D:/WMR/Rpackages"); #alleen op comp SG.
library(vmstools)#- code.google.com/p/vmstools
library(lubridate)

#- Define relevant paths
#!make directories if they do not yet exist
cntry       <- "NLD"                  #to be changed by lab
gitPath     <- "D:/Repository/datacallsVMSLogbook/Netherlands_2019_BB"             #to be changed by lab

eflaloPath  <- ""                                                                  #path were clean eflalo stands
tacsatPath  <- ""                                                                  #path were clean tacsat stands
shapePath   <- file.path(gitPath,"shapes")                                         #path were shape files are stored.
outPath     <- file.path(gitPath,"output")                                         #output folder of this script. Files are used in script '1.2 Calculations.R'

#- Set years, gears and study area (ICES rectangles)
years         <- 2014:2017                        #for study relevant years
gears         <- c('GN','GND','GNS','GTN','GTR')  #selection of gill net types, might be different then the ones selected here!
load(file=paste(shapePath,'ICESR.RData',sep=''))
ICESrect      <-  ac(unique(Sices$ICESNAME_1[Sices$Ecoregion== 'Greater North Sea']))

#-------------------------------------------------------------------------------
#- 1a) load ICES rectange, tacsat and eflalo data from files. files should be "Cleaned", 
#------------------------------------------------------------------------------- 
for(year in years){
  print(year)
  #load data
  load(file.path(eflaloPath,paste("eflalo",year,".RData",sep="")))  #named eflalo as an object
  load(file.path(tacsatPath,paste("tacsat",year,".RData",sep="")))  #named tacsat as an object

  #make sure datasets are formated corretly
  eflalo <- formatEflalo(eflalo)
  tacsat <- formatTacsat(tacsat)

  #select relevant gears and fishing areas 
  eflalo   <- subset(eflalo, LE_GEAR %in% gears & LE_RECT %in% ICESrect)
  
  #calculate total landings and value
  eflalo$LE_KG_TOT   <- rowSums(eflalo[c(grep('KG',colnames(eflalo)))],na.rm=T)
  eflalo$LE_EURO_TOT <- rowSums(eflalo[c(grep('EURO',colnames(eflalo)))],na.rm=T)
  
  #Keep only relevant columns.
  eflalo <-(cbind(eflalo[,c("FT_REF","VE_REF","VE_ID","VE_FLT","VE_COU","VE_LEN","VE_KW","VE_TON",
                           "FT_DCOU","FT_DHAR","FT_DDAT","FT_DTIME","FT_LCOU","FT_LHAR","FT_LDAT","FT_LTIME",
                           "LE_ID","LE_CDAT","LE_STIME","LE_ETIME","LE_SLAT","LE_SLON","LE_ELAT","LE_ELON",
                           "LE_GEAR","LE_WIDTH","LE_MSZ","LE_RECT","LE_DIV","LE_MET")],
                 eflalo[,c("LE_KG_TOT","LE_KG_SOL","LE_KG_COD","LE_KG_MUL","LE_KG_BSS",
                           "LE_EURO_TOT","LE_EURO_SOL","LE_EURO_COD","LE_EURO_MUL",'LE_EURO_BSS')]))
  #merge eflalo and tacsat
  tacsatp         <- mergeEflalo2Tacsat(eflalo,tacsat)
  tacsatp         <- tacsatp[tacsatp$FT_REF != 0,] 
  tacsatp$LE_GEAR <- eflalo$LE_GEAR[match(tacsatp$FT_REF,eflalo$FT_REF)]
  tacsatp         <- sortTacsat(tacsatp)
   
  #Keep only relevant columns.
  tacsatp <- tacsatp[,names(tacsatp) %in% c("VE_COU","VE_ID","VE_REF","SI_LATI","SI_LONG","SI_DATE",
                                            "SI_TIME","SI_HE","SI_SP","SI_DATIM","TSID","FT_REF","LE_GEAR") ]
  if (year==2014)  eflalo.yrs      <- eflalo
  if (!year==2014) eflalo.yrs      <- rbindEflalo(eflalo.yrs,eflalo) 
  if (year==2014)  tacsatp.yrs     <- tacsatp
  if (!year==2014) tacsatp.yrs     <- rbindTacsat(tacsatp,tacsatp.yrs) 
}# End of year loop

#-------------------------------------------------------------------------------
#- Select only vessels with average effort over de years exceeding 1 day/year
#-------------------------------------------------------------------------------
#- Calculate effort (days)
eflalo.yrs$FT_DDATIM  <- as.POSIXct(paste(eflalo.yrs$FT_DDAT,eflalo.yrs$FT_DTIME, sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
eflalo.yrs$FT_LDATIM  <- as.POSIXct(paste(eflalo.yrs$FT_LDAT,eflalo.yrs$FT_LTIME, sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
eflalo.yrs$INTV       <- an(difftime(eflalo.yrs$FT_LDATIM,eflalo.yrs$FT_DDATIM,units="days"))
eflalo.yrs$dummy      <- 1
res                   <- aggregate(eflalo.yrs$dummy,by=list(eflalo.yrs$FT_REF),FUN=sum,na.rm=T)
colnames(res)         <- c("FT_REF","nrRecords")
eflalo.yrs            <- merge(eflalo.yrs,res,by="FT_REF",all=T)
eflalo.yrs$LE_KG_DAS  <- eflalo.yrs$INTV / eflalo.yrs$nrRecords #DAS stands for 'Days at Sea'.

#- Select only vessels with average effort over de years exceeding 1 day/year
eflalo.yrs$SI_YEAR  <- an(format(eflalo.yrs$FT_DDATIM, format='%Y'))
vessel              <- aggregate(eflalo.yrs$LE_KG_DAS, by=list(eflalo.yrs$VE_REF,eflalo.yrs$SI_YEAR), FUN= sum)
vessel              <- aggregate(vessel$x, by=list(vessel$Group.1),FUN=mean)
eflalo.yrs          <- eflalo.yrs[eflalo.yrs$VE_REF %in% vessel$Group.1[vessel$x>1],]
tacsatp.yrs         <- tacsatp.yrs[tacsatp.yrs$VE_REF %in% vessel$Group.1[vessel$x>1],]
tacsatp.yrs$SI_YEAR <- year(tacsatp.yrs$SI_DATIM)

#-------------------------------------------------------------------------------
#- Save results for further processing
#-------------------------------------------------------------------------------
save(tacsatp.yrs,file=paste(outPath,'tacsatp.yrs.RData',sep=''))
save(eflalo.yrs,file=paste(outPath,'eflalo.yrs.Rdata',sep=''))
     