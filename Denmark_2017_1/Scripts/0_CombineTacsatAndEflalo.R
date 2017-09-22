### Combine Tacsat And Eflalo - proposal ###

# Before running this code the following needs to be done:
### - All vms points needs to be cleaned, so that all points are:
###     - Not inside Harbor, on earth, not on land, not duplicate, not pseudoDupliacate, no foreign vessels
###   cleaned vms files named tacsat should be saved as cleanTacsat_"Year".Rdata (eg. cleanTacsat_2016.Rdata) in RdataPath.
### - All logbook data need to be cleaned, so all records are: 
###     - Not duplicated, not NoCatch, no landings before departures, no overlapping trips, no dates before 1st of January, 
###       not wrong mesh sizes, not wrong vessel lengths
###   Cleaned logbook data named eflalo should be saved as cleanEflalo_"Year".Rdata (eg. cleanEflalo_2016.Rdata) in RdataPath.
### - Make a file called speedarr.csv, which should contain speed thresholds for all gears in your fleet. Place it in dataPath

### See https://github.com/nielshintzen/vmstools/wiki for details.

### This code uses static speed thresholds, if automatic thresholds are requested, please change code accordingly.

rm(list=ls())
library(vmstools)
library(data.table)

#- Set the working directory to the folder where you keep your code and data
sysPath       <-"W:/jepol/home/17-09-07_ProposedWorkflows/"
RdataPath   <- paste0(sysPath,"Rdata/") ## Put cleaned tacsat and eflalo files in this folder
dataPath    <- paste0(sysPath,"Data/")  ## Put file speedarr.csv in this folder

# If directories not exist, create them
dir.create(sysPath, showWarnings = FALSE)
dir.create(RdataPath, showWarnings = FALSE)
dir.create(dataPath, showWarnings = FALSE)

setwd(sysPath)

#- Set the country abbreviation to e.g. deu, gbr, dnk
country     <- "dnk"
interval    <- 60 #set interval time applicable for your country VMS

for (Year in c(2012:2016)) {
  print(Year)  
  
  load(file.path(paste(RdataPath,"cleanEflalo_ ",Year," .RData",sep="")))
  load(file.path(paste(RdataPath,"cleanTacsat_ ",Year," .RData",sep="")))
  
  tacsat <- formatTacsat(tacsat) # format each of the columns to the specified class
  eflalo <- formatEflalo(eflalo) # format each of the columns to the specified class
  
  #Create as.POSIXct columns for date and time
  eflalo$FT_DDATIM  <- as.POSIXct(paste(eflalo$FT_DDAT,eflalo$FT_DTIME, sep = " "),
                                  tz = "GMT", format = "%d/%m/%Y  %H:%M")
  eflalo$FT_LDATIM  <- as.POSIXct(paste(eflalo$FT_LDAT,eflalo$FT_LTIME, sep = " "),
                                  tz = "GMT", format = "%d/%m/%Y  %H:%M")
 
  #- Merge eflalo and tacsat together
  tacsatp               <- mergeEflalo2Tacsat(eflalo,tacsat)
  
  #Report VMS points that could not be coupled with any logbook
  x              <- subset(tacsatp,FT_REF == 0)
  print(paste("Percentage VMS points that could not be combined with any logbooks:", round(nrow(x)/nrow(tacsat)*100, digits = 2), "%"))
  
  #Only use vms points that can be merged with logbooks
  tacsatp <- subset(tacsatp,FT_REF != 0)
  
  # Assign Gears to tacsatp in accordance with the logbook
  tacsatp$LE_GEAR        <- eflalo$LE_GEAR[match(tacsatp$FT_REF,eflalo$FT_REF)]
  
  # If there is no duration on a ping (e.g. because ?? - dont know where this line came from), 
  # set it to the default interval of the country
  tacsatp$INTV[is.na(tacsatp$INTV)] <- interval
  
  # If the length of a ping duration is more than twice of the normal interval (e.g. becuase 
  # there has been no signal for 6 hours, or the ping is at the start of a trip), set it 
  # to the deafult interval of the country
  tacsatp$INTV[tacsatp$INTV>(interval*2)]    <- interval
  
  #- Read in fixed speed bounds for different gears - Place speedarr.csv in dataPath
  # !!! Make sure that all your gears are added to this speedarr.csv list, some may be lacking !!! #
  
  speedarr              <- read.csv(file=paste(dataPath,"/speedarr.csv",sep=""))
  colnames(speedarr)    <- c("LE_GEAR","min","max", "redsk")
  speedarr$LE_GEAR      <- ac(speedarr$LE_GEAR)
  speedarr              <- speedarr[speedarr$LE_GEAR%in%unique(tacsatp$LE_GEAR),]
  
    # Assign activity to the column SI_STATE. If speed is within threshold, set it to 1, else set it to 0
  gears                 <- unique(tacsatp$LE_GEAR)
  tacsatp$SI_STATE<-0
  for (mm in gears) {
    if(length(tacsatp[tacsatp$LE_GEAR==mm& tacsatp$SI_SP >= an(speedarr[speedarr$LE_GEAR==mm,"min"])& tacsatp$SI_SP <= an(speedarr[speedarr$LE_GEAR==mm,"max",]),]$SI_STATE
              >0)){
      tacsatp[tacsatp$LE_GEAR==mm& tacsatp$SI_SP >= an(speedarr[speedarr$LE_GEAR==mm,"min"])& tacsatp$SI_SP <= an(speedarr[speedarr$LE_GEAR==mm,"max",]),]$SI_STATE<-1
    }  
  }
  
  
  # This code is similar to vmstools:splitAmongPings, but with a few differences:
  # - This code only combine logbook data with vms data on a day level. If there is no pings
  #   on a day where there is logbook data, that logbook data is not used. That differs from the 
  #   splitAmongPings, which can combine also at trip level, meaning that if there are no pings on
  #   a certain day, the kg and euro will be split out on the rest of the trip. 
  # - This code does not combine using C-Squares, since danish fishermen will only report ICES-squares
  #   where most of the fishing takes place. splitAmongPings will take out pings not in the ICES-square 
  #   reported and only distribute kg and euro to pings inside the reported ICES-square. 
  # 
  # Furthermore, this script can handle all data, so no need to subset on area and "most important 
  # species". 
  
  t <- data.table(tacsatp)
  e <- data.table(eflalo)
  
  #Add a SI_DATE to merged eflalo, for easier merging
  e[,SI_DATE:=LE_CDAT]
  
  #Add id for merging
  t[,id:=paste0(FT_REF, SI_DATE)]
  e[,id:=paste0(FT_REF, SI_DATE)]
  
  #find all column names with KG or EURO in them
  kg_euro <- grep("KG|EURO", colnames(e), value = T)
  
  #Sum all eflalo records with the same day and id
  n <- e[,lapply(.SD,sum),by=.(id),
         .SDcols=kg_euro]
  
  #Setkey for merging
  setkey(t, id)
  setkey(n, id)
  
  #find all vms points that cannot be merged
  tx <- t[!n, on=c("id")]
  paste("Percentage cleaned VMS points not able to be merged with Logbooks, using date and FT_REF:", round(nrow(tx)/nrow(t)*100,2), "%")
  
  #Add kg and euro to vms points
  ts <- merge(t, n)
  
  #set key for merging
  setkey(ts, id)
  
  #Set a weight on each point for later multiply
  ts[,Weight:=INTV/sum(INTV), by=.(id)]
  #Multiply the weight to each KG and EURO column 
  ts[,(kg_euro):= lapply(.SD, function(x) x * Weight), .SDcols=kg_euro]
  
  #Find eflalo records with no matching vms points
  `%!in%` <- function(a,b) ! a %in% b
  ex <- e[id%!in%unique(ts$id)]
  
  paste("Percentage cleaned logbook entries not able to be merged with VMS data, using date and FT_REF:", round(nrow(ex)/nrow(e)*100,2), "%")
  
  #Remove columns with sum zero
  #zeroes <- names(colSums(Filter(is.numeric, ts))
  #                 [colSums(Filter(is.numeric, ts))==0])
  #ts[,(zeroes):=NULL]
  
  #sum(ex$LE_KG_COD)+sum(ts$LE_KG_COD)
  #sum(e$LE_KG_COD)
  
  
  #Only use vms points with fishing
  #t <- t[SI_STATE==1]
  
  
  ## Here we could distribute eflalo records that matches trip, but not day ###
  ## not desired by Denmark, but useful for others?                         ###
  
  
  ##############################################################################
  
  tacsatEflalo <- data.frame(ts)
  save(tacsatEflalo, file=file.path(paste(RdataPath,"tacsatEflalo_",Year,".RData",sep="")))
}
