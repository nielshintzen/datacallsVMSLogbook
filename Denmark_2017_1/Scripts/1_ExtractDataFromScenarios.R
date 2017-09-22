### Extract data from all Scenarios according to the lookup table ###

# Before running this code the following needs to be done:
### - The file lookup.csv should be placed in folder dataPath
### - Cleaned tacsatEflalo files for all the years should be in the RdataPath. Each file 
###   should be saved as tacsatEflalo_"Year".Rdata (eg. tacsatEflalo_2016.Rdata)
###   These files  should be in the standard tacsatEflalo format and contain merged kg and EURO for all
###   species. This can be obtained using the contrys own procesdure or using 
###   the 0_CreateTacsatEflalo.R script. 
###   Furthermore the Cleaned tacsatEflalo should contain these columns:
###   "VE_REF", "SI_LATI", "SI_LONG", "SI_DATE", "SI_TIME", "SI_SP", "SI_HE", "SI_HARB", "SI_STATE", "SI_FT", "SI_DATIM", "INTV", "FT_REF"
### - Cleaned eflalo files should be in the RdataPath, saved as cleanEflalo_"Year".csv (eg. cleanEflalo_2016.Rdata)

rm(list=ls())
library(vmstools)
library(rgdal)
library(data.table)


#- Set the working directory to the folder where you keep your code and data
sysPath       <-"C:/Users/jepol/Desktop/Repository/Denmark_2017_1/"
RdataPath   <- paste0(sysPath,"Rdata/")
dataPath    <- paste0(sysPath,"Data/")
polygonPath <- paste0(sysPath,"shapes")
resPath     <- paste0(sysPath,"Results/")

# If directories not exist, create them
dir.create(sysPath, showWarnings = FALSE)
dir.create(RdataPath, showWarnings = FALSE)
dir.create(dataPath, showWarnings = FALSE)
dir.create(polygonPath, showWarnings = FALSE)
dir.create(resPath, showWarnings = FALSE)

setwd(sysPath)

# Read in lookup table
lookup <- fread(paste0(dataPath, "Lookup.csv"), blank.lines.skip=T)

#Define gears used in workflow
Dredge <- c("DRB", "DRO", "DRC", "BMS")
BottomTrwl <- c("OTB", "OTT", "PTB", "TBN", "TBS")
BeamTrwl <- "TBB"
PelagicTrwl <- c("OTM", "PTM")
Lines <- c("LH", "LHP", "LL", "LLD", "LLS", "LX")
Traps <- c("FPO", "FYK", "FPN")
Nets <- c("GTR", "GNS", "GND", "GN")
AnchoredSeine <- "SDN"
FlyShootingSeine <- "SSC"
PurseSeine <- "PS"

All_Gears <- c(Dredge, BottomTrwl, BeamTrwl, PelagicTrwl, Lines, 
               Traps, Nets, AnchoredSeine, FlyShootingSeine, PurseSeine)

#Add all shapes to R environment, and give them names according to lookup table
shapes <- unique(lookup$Shape)
for(j in shapes){
  x <- readOGR(dsn = polygonPath, layer = j)
  assign(j, x)
}

for (Year in c(2012:2016)) {
  load(file=file.path(paste(RdataPath,"tacsatEflalo_",Year,".RData",sep="")))
  load(file.path(paste(RdataPath,"cleanEflalo_ ",Year," .RData",sep="")))  #named eflalo as an object
  
  ## Add icessquares and Csquares to tacsatEflalo
  tacsatEflalo$REF_ICES <- ICESrectangle(tacsatEflalo)
  tacsatEflalo$CSquare  <- CSquare(tacsatEflalo$SI_LONG, tacsatEflalo$SI_LATI, 0.05)
  
  # Add relevant information to tacsatEflalo from eflalo
  tacsatEflalo$LE_MSZ <- eflalo$LE_MSZ[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
  tacsatEflalo$LE_MET <- eflalo$LE_MET[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
  tacsatEflalo$VE_LEN        <- eflalo$VE_LEN[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
  tacsatEflalo$LE_GEAR        <- eflalo$LE_GEAR[match(tacsatEflalo$FT_REF,eflalo$FT_REF)]
  
  # Make tacsatEflalo into a data.table for faster processing
  te <- data.table(tacsatEflalo)
  # Calculate total kg and EURO
  te[,LE_KG_TOT:=rowSums(.SD, na.rm = TRUE), .SDcols = grep("KG", names(te))]
  te[,LE_EURO_TOT:=rowSums(.SD, na.rm = TRUE), .SDcols = grep("EURO", names(te))]
  
  # Add column Gear Group to tacsateflalo data.table
  te[LE_GEAR %in% Dredge, GearGroup:="Dredge"]
  te[LE_GEAR %in% BottomTrwl, GearGroup:="BottomTrwl"]
  te[LE_GEAR %in% BeamTrwl, GearGroup:="BeamTrwl"]
  te[LE_GEAR %in% PelagicTrwl, GearGroup:="PelagicTrwl"]
  te[LE_GEAR %in% Lines, GearGroup:="Lines"]
  te[LE_GEAR %in% Traps, GearGroup:="Traps"]
  te[LE_GEAR %in% Nets, GearGroup:="Nets"]
  te[LE_GEAR %in% AnchoredSeine, GearGroup:="AnchoredSeine"]
  te[LE_GEAR %in% FlyShootingSeine, GearGroup:="FlyShootingSeine"]
  te[LE_GEAR %in% PurseSeine, GearGroup:="PurseSeine"]
  te[is.na(GearGroup), GearGroup:="Other"]
  
  #Only use vms points with fishing
  te <- te[SI_STATE==1]
  
  # make spatial points for each occurence in the tacsatEflalo file
  pings  <- SpatialPoints(te[,.(SI_LONG, SI_LATI)],proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  #Calculate output data according to each row in the lookup table and save it in the RdataPath
for (i in 1:nrow(lookup)) {
  
  #Get pings in relevant C-Squares
  #Squares <- c("43F9", "42F7", "42F6", "42F7", "44F9", "43F7", "43F8")
  
  #points <- te[REF_ICES %in% Squares]
  
  #Get data inside shape
  if(!is.na(lookup$Shape[i])){
    shape <- get(lookup$Shape[i])
    #Get the rigth shape and ensure that it is in the right projection
    shape <- spTransform(shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    #Get all data within shape
    sub <- te[!is.na(over(pings, as(shape,"SpatialPolygons")))]
  }
  
  #Write all points inside measure to file
  points <- sub[,c("LE_GEAR", "SI_LATI", "SI_LONG")]
  points <- data.table(Year, points)
  fwrite(points, file=paste0(RdataPath, "Points_", lookup$Measure[i], "_", Year, ".csv"))
  
  #Get data within timeframe
  if(!(is.na(lookup$Time_Start[i])| is.na(lookup$Time_End[i])) ){
    sub <- sub[strptime(sub$SI_DATE, "%d/%m/%Y") >= strptime(paste(lookup$Time_Start[i], Year, sep = "-"),  "%d-%m-%Y") &
                 strptime(sub$SI_DATE, "%d/%m/%Y") <= strptime(paste(lookup$Time_End[i], Year, sep = "-"),  "%d-%m-%Y") ]
  }
  
  # Get relevant gears - remember to define them first
  if(!is.na(lookup$Gears[i])){
    sub <- sub[LE_GEAR %in% get(lookup$Gears[i])]
  }
  
  # Get relevant Metiers - remember to define them first  
  if(!is.na(lookup$Metiers[i])){
    sub <- sub[LE_MET %in% get(lookup$Metiers[i])]
  }
  
  #Remove species columns with sum zero
  zeroes <- names(colSums(Filter(is.numeric, sub))
                  [colSums(Filter(is.numeric, sub))==0])
  try(sub[,(zeroes):=NULL], silent = T)
  
  #Aggregate data on gear level
  out <- sub[, c(NoVessels=uniqueN(VE_REF), Effort_hrs=sum(INTV/60), lapply(.SD, sum)),
             by=.(Gear=LE_GEAR),
             .SDcols=grepl("LE_KG|LE_EURO", names(sub))]
  
  #Add Scenario name and year to data table
  out <- data.table(Scenario=lookup$Measure[i], Year=Year, out)
  #Write to csv file
  fwrite(out, file=paste0(RdataPath, "MeasurePrGear_", lookup$Measure[i], "_", Year, ".csv"))
  
  #Aggregate data on gear group level
  out_group <- sub[, c(NoVessels=uniqueN(VE_REF), Effort_hrs=sum(INTV/60), lapply(.SD, sum)),
             by=.(GearGroup=GearGroup),
             .SDcols=grepl("LE_KG|LE_EURO", names(sub))]
  
  #Add Scenario name and year to data table
  out_group <- data.table(Scenario=lookup$Measure[i], Year=Year, out_group)
  #Write to csv file
  fwrite(out_group, file=paste0(RdataPath, "MeasurePrGearGroup_", lookup$Measure[i], "_", Year, ".csv"))
  
}}

# Get data from all measures / years and gear into one data table.
m_list1 <- list.files(path = RdataPath, pattern= "MeasurePrGear_", full.names = T)
MeasuresPrGear <- rbindlist(lapply(m_list1, fread), fill=T)
#Set NA's to 0 (rbindlist fills out NA's when a column is not in the first file)
MeasuresPrGear[is.na(MeasuresPrGear)] = 0
#setcolorder(measures, c("Year", "Scenario", "GEAR", "LE_KG_TOT", "LE_EURO_TOT"))
fwrite(MeasuresPrGear, paste0(resPath, "MeasuresPrGear.csv"))

# Get data from all measures / years and gearGroup into one data table.
m_list2 <- list.files(path = RdataPath, pattern= "MeasurePrGearGroup_", full.names = T)
MeasuresPrGearGroup <- rbindlist(lapply(m_list2, fread), fill=T)
#Set NA's to 0 (rbindlist fills out NA's when a column is not in the first file)
MeasuresPrGearGroup[is.na(MeasuresPrGearGroup)] = 0
#setcolorder(measures, c("Year", "Scenario", "GEAR", "LE_KG_TOT", "LE_EURO_TOT"))
fwrite(MeasuresPrGearGroup, paste0(resPath, "MeasuresPrGearGroup.csv"))


# Get point data from all measures and years into one data table.
# Works in this case, but if there are overlapping polygons some points will
# be duplicate. Then one shapefile containing all the polygons should be included. 
pt_lst <- list.files(path = RdataPath, pattern= "Points", full.names = T)
points <- rbindlist(lapply(pt_lst, fread), fill=T)
fwrite(points, paste0(resPath, "Points.csv"))
