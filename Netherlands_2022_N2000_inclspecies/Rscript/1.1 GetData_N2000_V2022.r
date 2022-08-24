#- Clear workspace
rm(list=ls());gc()

library(vmstools)#- code.google.com/p/vmstools
library('sp'); library('rgdal');library(SOAR)

#- Define relevant paths (change accordingly)
gitPath     <- "C:/Users/hamon004/Github/VMSwork/datacallsVMSLogbook/Netherlands_2022_N2000"             #to be changed by lab
eflaloPath  <- "W:/IMARES/Data/VMS_/BASEFILES/restricted"                             #path were clean eflalo stands
tacsatPath  <- "W:/IMARES/Data/VMS_/BASEFILES/restricted"                             #path were clean tacsat stands
shapePath   <- file.path(gitPath,"shapes")                                  #path were shape files are stored.
outPath     <- file.path(gitPath,"output")                                  #output folder of this script.

#- Set method used to define activity
actMethod           <- "tacsatActivity"                                        #choose speed for fixed speedrule or use alternative tacsat file with SI_STATE column included
tacsatStatePath     <- "W:/IMARES/Data/VMS_/BASEFILES/restricted"                        #path were tacsat with SI_STATE column stands, NULL otherwise

#- define years and areas of interest
years               <- 2014:2021

ICESrect            <- c('40F3','39F4','39F3','38F5','38F4','38F3','38F2','37F6','37F5','37F4','37F3','37F2','36F6','36F5','36F4','36F3','36F2','35F7','35F6','35F5','35F4','35F3','34F4','34F3','33F2','33F3','33F4','32F2','32F3','32F4','31F3','31F4')
FnCleanEflalo       <- "cleanEflalo"                    #filename of the cleanEflalo files.
FnCleanTacsat       <- "cleanTacsat"                    #filename of the cleanTacsat files.
FnCleanTacsatEflalo <- "tacsatEflaloM"              #if available: filename of the cleanTacsatEflalo files, otherwise NULL.

#-------------------------------------------------------------------------------
#- 1a) load grid, tacsat and eflalo data from files. files should be "Cleaned",
#-------------------------------------------------------------------------------
for(year in years){
  print(year)
  #load data
  load(file.path(eflaloPath,paste(FnCleanEflalo,year,".RData",sep="")))  #named eflalo as an object
  if(is.null(FnCleanTacsatEflalo))
    load(file.path(tacsatPath,paste(FnCleanTacsat,year,".RData",sep="")))  #named tacsat as an object
  if(!is.null(FnCleanTacsatEflalo))     
    tacsatEflalo <-get(load(file.path(tacsatPath,paste(FnCleanTacsatEflalo,year,".RData",sep=""))))  #named tacsatEflalo as an object   
       
  #make sure datasets are formated corretly
  eflalo    <- formatEflalo(eflalo)
  if(is.null(FnCleanTacsatEflalo))
    tacsat  <- formatTacsat(tacsat)

  #select relevant gears and fishing areas
  eflalo    <- subset(eflalo, LE_RECT %in% ICESrect)

  # select the top 10 species in the ICES rect (hopefully over the years the top 5 are in the annual top 10)
  top10spp  <- names(sort(-colSums(eflalo[c(grep("KG",colnames(eflalo)))],na.rm=T))[1:10])

  #Calculate total landings, but only if columns not exist yet
  if(length(grep("LE_KG_TOT",colnames(eflalo)))==0)
    eflalo$LE_KG_TOT    <- rowSums(eflalo[c(grep("KG",colnames(eflalo)))],na.rm=T)
  if(length(grep("LE_EURO_TOT",colnames(eflalo)))==0)    
    eflalo$LE_EURO_TOT  <- rowSums(eflalo[c(grep("EURO",colnames(eflalo)))],na.rm=T)

  eflalo      <- cbind(eflalo[c("FT_REF","VE_REF","VE_ID","VE_FLT","VE_COU","VE_LEN","VE_KW","VE_TON","LE_MET",
                                "FT_DCOU","FT_DHAR","FT_DDAT","FT_DTIME","FT_LCOU","FT_LHAR","FT_LDAT","FT_LTIME",
                                "LE_ID","LE_CDAT","LE_STIME","LE_ETIME","LE_SLAT","LE_SLON","LE_ELAT","LE_ELON",
                                "LE_GEAR","LE_WIDTH","LE_MSZ","LE_RECT","LE_DIV","LE_SUBDIV")],
                       eflalo[c("LE_KG_TOT","LE_EURO_TOT",top10spp)])

  # calculate total landings for tacsatEflalo if not available and then reduce number of columns
  if(!is.null(FnCleanTacsatEflalo)){
    if(length(grep("LE_KG_TOT",colnames(tacsatEflalo)))==0)
      tacsatEflalo$LE_KG_TOT    <- rowSums(tacsatEflalo[c(grep("KG",colnames(tacsatEflalo)))],na.rm=T)
    if(length(grep("LE_EURO_TOT",colnames(tacsatEflalo)))==0)    
      tacsatEflalo$LE_EURO_TOT  <- rowSums(tacsatEflalo[c(grep("EURO",colnames(tacsatEflalo)))],na.rm=T)
    
    tacsatEflalo      <- tacsatEflalo[c("FT_REF","VE_REF","VE_ID","SI_LATI","SI_LONG","VE_FLT","VE_COU","VE_LEN","VE_KW","SI_DATIM","LE_MET","SI_STATE",
                                              "LE_GEAR","LE_WIDTH","LE_MSZ","LE_RECT","LE_KG_TOT","LE_EURO_TOT",top10spp,"INTV")]
    
  }     
    
  
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
  eflalo$INTV           <- as.numeric(eflalo$INTV / eflalo$nrRecord)

  #merge eflalo and tacsat
  if(is.null(FnCleanTacsatEflalo)){
    tacsatp         <- mergeEflalo2Tacsat(eflalo,tacsat)
    tacsatp         <- tacsatp[tacsatp$FT_REF != 0,]
    tacsatp$LE_GEAR <- eflalo$LE_GEAR[match(tacsatp$FT_REF,eflalo$FT_REF)]
    tacsatp$LE_MET  <- eflalo$LE_MET[match(tacsatp$FT_REF,eflalo$FT_REF)]
    tacsatp$VE_LEN  <- eflalo$VE_LEN[match(tacsatp$FT_REF,eflalo$FT_REF)]
    #Keep only relevant columns.
    tacsatp <- tacsatp[,names(tacsatp) %in% c("VE_COU","VE_ID","VE_REF","VE_LEN", "SI_LATI","SI_LONG","SI_DATE",
                                            "SI_TIME","SI_HE","SI_SP","SI_DATIM","TSID","FT_REF","LE_GEAR","LE_MET") ]
  }
  if(!is.null(FnCleanTacsatEflalo)){
    tacsatEflalo         <- tacsatEflalo[tacsatEflalo$FT_REF != 0,]
    tacsatEflalo$LE_GEAR <- eflalo$LE_GEAR[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
    tacsatEflalo$LE_MET  <- eflalo$LE_MET[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
    tacsatEflalo$VE_LEN  <- eflalo$VE_LEN[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
  }    
  

  #-------------------------------------------------------------------------------
  #- 1b) Define activitity Based on your prefered method "f" is fishing, "s" is steaming
  #------------------------------------------------------------------------------- 
  if(is.null(FnCleanTacsatEflalo)){
  
    #- Define activitity based on vesselspeed
    if (actMethod=="speed"){
      tacsatp               <- tacsatp[!is.na(tacsatp$SI_SP),]
      
      #- Read in fixed speed bounds for different gears
      # !!! Make sure that all your gears are added to this speedarr.csv list, some may be lacking !!! #
      
      speedarr              <- read.csv(file=paste(inPath,"/speedarr.csv",sep=""))
      colnames(speedarr)    <- c("LE_GEAR","min","max")
      speedarr$LE_GEAR      <- ac(speedarr$LE_GEAR)
      speedarr              <- speedarr[speedarr$LE_GEAR%in%unique(tacsatp.yrs$LE_GEAR),]
      gears                 <- unique(tacsatp$LE_GEAR)
      tacsatp$SI_STATE  <- "s"
      for (mm in gears) {
        tacsatp[tacsatp$LE_GEAR==mm& tacsatp$SI_SP >= an(speedarr[speedarr$LE_GEAR==mm,"min"])& tacsatp$SI_SP <= an(speedarr[speedarr$LE_GEAR==mm,"max",]),]$SI_STATE<-"f"
      }
    }
    
    #- Use activity defined in national dataset
    if (actMethod =="tacsatActivity"){
        tacsatpNoState      <- tacsatp
        load(file.path(tacsatStatePath,paste("tacsatActivity",year,".RData",sep="")))  #tacsat file with SI_STATE already defined
        act.yrs <- subset(tacsatp,FT_REF %in% tacsatp$FT_REF,c(VE_REF,SI_LATI,SI_LONG,SI_DATE,SI_TIME,SI_STATE))  
    }
    tacsatp   <- merge(tacsatpNoState,act.yrs,all.x=TRUE)

    #-------------------------------------------------------------------------------
    #- 2a) Allocate catch and value
    #------------------------------------------------------------------------------- 

    #-------------------------------------------------------------------------------
    #- Determine interval time
    #-------------------------------------------------------------------------------
    tacsatp           <- sortTacsat(tacsatp) #sort the data by vessel and time
    tacsatp$INTV      <- intervalTacsat(tacsatp,level="trip",weight=c(1,1),fill.na=TRUE)$INTV
    idx                   <- which(is.na(tacsatp$INTV)==TRUE)
    tacsatp$INTV[idx] <- intervalTacsat(tacsatp,level="vessel",weight=c(1,1),fill.na=TRUE)$INTV[idx]
    tacsatp           <- tacsatp[!is.na(tacsatp$INTV),]
    tacsatp           <- tacsatp[tacsatp$INTV>0,]
    tacsatp$INTV[tacsatp$INTV >230] <- 115
  }

  #- Split eflalo up in two sets, one set that cannot be merged to tacsat and one which matches
  if(is.null(FnCleanTacsatEflalo)){
    eflaloNM              <- subset(eflalo,!FT_REF %in% unique(tacsatp$FT_REF))
    eflaloM               <- subset(eflalo, FT_REF %in% unique(tacsatp$FT_REF))
  }
  if(!is.null(FnCleanTacsatEflalo)){  
    eflaloNM              <- subset(eflalo,!FT_REF %in% unique(tacsatEflalo$FT_REF))
    eflaloM               <- subset(eflalo, FT_REF %in% unique(tacsatEflalo$FT_REF))
  }

  if(is.null(FnCleanTacsatEflalo)){
    #select only fishing pings
    tacsatp           <- tacsatp[tacsatp$SI_STATE == 'f',]

    #- Split effort and total landing among pings
    tacsatEflalo          <- (splitAmongPings(tacsat=subset(tacsatp,FT_REF %in% unique(eflaloM$FT_REF)),eflalo=subset(eflaloM),variable="all",level="day",by="INTV",conserve=T))
    cat("total value in eflalo\n")
    print(colSums(eflaloM[c(grep("KG",colnames(eflaloM)))],na.rm=T))
    cat("total value in tacsatEflalo\n")
    print(colSums(tacsatEflalo[c(grep("KG",colnames(tacsatEflalo)))],na.rm=T))
  }
  if(!is.null(FnCleanTacsatEflalo))
    tacsatEflalo          <- subset(tacsatEflalo,SI_STATE>0)
  
  # tacsatEflalo      <- tacsatEflalo[c("FT_REF","VE_REF","VE_ID","SI_LATI","SI_LONG","VE_FLT","VE_COU","VE_LEN","VE_KW","SI_DATIM","VE_TON","LE_MET",
  #                               "LE_GEAR","LE_WIDTH","LE_MSZ","LE_RECT","LE_DIV","LE_SUBDIV","LE_KG_TOT","LE_EURO_TOT",top10spp,"INTV")]
  # 

  if (year==years[1])  eflaloNM.yrs      <- eflaloNM
  if (year>years[1])   eflaloNM.yrs      <- rbindEflalo(eflaloNM.yrs,eflaloNM)
  if (year==years[1])  tacsatEflalo.yrs  <- tacsatEflalo
  if (year>years[1])   tacsatEflalo.yrs  <- rbindEflalo(tacsatEflalo.yrs,tacsatEflalo)  
  Store(tacsatEflalo.yrs);gc()
}

save(tacsatEflalo.yrs,file=file.path(outPath,'tacsatEflalo.yrs.RData',sep=''))
save(eflaloNM.yrs,file=file.path(outPath,'eflaloNM.yrs.RData',sep=''))
