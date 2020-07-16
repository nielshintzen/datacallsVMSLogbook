#- Clear workspace
rm(list=ls());

library(vmstools)#- code.google.com/p/vmstools
library('sp'); library('rgdal')

#- Define relevant paths (change accordingly)
gitPath     <- "C:/Users/hamon004/Github/VMSwork/datacallsVMSLogbook/Netherlands_2020_DB"             #to be changed by lab

eflaloPath  <- "S:/Onderzoek/Projecten VMS/BASEFILES/"                             #path were clean eflalo stands
tacsatPath  <- "S:/Onderzoek/Projecten VMS/BASEFILES/"                            #path were clean tacsat stands
shapePath   <- file.path(gitPath,"shapes")                                         #path were shape files are stored.
outPath     <- file.path(gitPath,"output")                                         #output folder of this script. 

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
load(file.path(shapePath,'grd.RData'))
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
  
  # select the top 10 species in the ICES rect (hopefully over the years the top 5 are in the annual top 10)
  top10spp <- names(sort(-colSums(eflalo[c(grep("KG",colnames(eflalo)))],na.rm=T))[1:10])
  
  
  #Calculate total landings
  eflalo$LE_KG_TOT  <- rowSums(eflalo[c(grep("KG",colnames(eflalo)))],na.rm=T)
  eflalo$LE_EURO_TOT  <- rowSums(eflalo[c(grep("EURO",colnames(eflalo)))],na.rm=T)
  
  eflalo      <- cbind(eflalo[c("FT_REF","VE_REF","VE_ID","VE_FLT","VE_COU","VE_LEN","VE_KW","VE_TON","LE_MET",
                                "FT_DCOU","FT_DHAR","FT_DDAT","FT_DTIME","FT_LCOU","FT_LHAR","FT_LDAT","FT_LTIME",
                                "LE_ID","LE_CDAT","LE_STIME","LE_ETIME","LE_SLAT","LE_SLON","LE_ELAT","LE_ELON",
                                "LE_GEAR","LE_WIDTH","LE_MSZ","LE_RECT","LE_DIV","LE_SUBDIV")],
                       eflalo[c("LE_KG_TOT","LE_EURO_TOT",top10spp)])
  
  #Calculate seadays
  eflalo$FT_DDATIM  <- as.POSIXct(paste(eflalo$FT_DDAT,eflalo$FT_DTIME, sep = " "),
                                  tz = "GMT", format = "%d/%m/%Y  %H:%M")
  eflalo$FT_LDATIM  <- as.POSIXct(paste(eflalo$FT_LDAT,eflalo$FT_LTIME, sep = " "),
                                  tz = "GMT", format = "%d/%m/%Y  %H:%M")
  eflalo$INTV           <- difftime(eflalo$FT_LDATIM,eflalo$FT_DDATIM,units="days")
  eflalo$dummy          <- 1
  res                   <- aggregate(eflalo$dummy,by=list(eflalo$FT_REF),FUN=sum,na.rm=T)
  colnames(res)         <- c("FT_REF","nrRecord")
  eflalo                <- merge(eflalo,res,by="FT_REF")
  eflalo$INTV           <- eflalo$INTV / eflalo$nrRecord
  eflalo$LE_KG_DAS      <- eflalo$INTV
  eflalo$LE_KG_DAS      <- as.numeric(eflalo$LE_KG_DAS)
  
  #merge eflalo and tacsat
  tacsatp         <- mergeEflalo2Tacsat(eflalo,tacsat)
  tacsatp         <- tacsatp[tacsatp$FT_REF != 0,] 
  tacsatp$LE_GEAR <- eflalo$LE_GEAR[match(tacsatp$FT_REF,eflalo$FT_REF)]
  tacsatp$LE_MET  <- eflalo$LE_MET[match(tacsatp$FT_REF,eflalo$FT_REF)]
  tacsatp$VE_LEN  <- eflalo$VE_LEN[match(tacsatp$FT_REF,eflalo$FT_REF)]
  
  #Keep only relevant columns.
  tacsatp <- tacsatp[,names(tacsatp) %in% c("VE_COU","VE_ID","VE_REF","VE_LEN", "SI_LATI","SI_LONG","SI_DATE",
                                            "SI_TIME","SI_HE","SI_SP","SI_DATIM","TSID","FT_REF","LE_GEAR","LE_MET") ]
  if (year==years[1])  eflalo.yrs      <- eflalo
  if (year>years[1]) eflalo.yrs      <- rbindEflalo(eflalo.yrs,eflalo) 
  if (year==years[1])  tacsatp.yrs     <- tacsatp
  if (year>years[1]) tacsatp.yrs     <- rbindTacsat(tacsatp,tacsatp.yrs) 
}

save(tacsatp.yrs,file=file.path(outPath,'tacsatp.yrs.RData',sep=''))
save(eflalo.yrs,file=file.path(outPath,'eflalo.yrs.RData',sep=''))


