#- Clear workspace
rm(list=ls());                        

#required libraries
library(vmstools)                     
library(sf)                           
library(lubridate)                    
library(tidyverse)                        

#- Define relevant paths
gitPath     <- "C:/Users/hamon004/Local_Git_repos/DataCall"             #to be changed by lab

inPath     <- file.path(gitPath,"output")
shapePath  <- file.path(gitPath,"shapes")
outPath    <- file.path(gitPath,"shareOutput")

dir.create(outPath,recursive=TRUE,showWarnings=FALSE)#Create the output folder 

#define country
Country       <- 'NLD'                #Country label used in the output filename
ICESrect      <- c("36F5","39F3","39F4","40F3","40F4")#ICES rectangles to retain for the grid and unmatched logbooks
years         <- 2018:2025            #Years to include in the analysis

#-------------------------------------------------------------------------------
#- 1a) Load tacsat and eflalo files combined from the time period (output script 1.1)
#-------------------------------------------------------------------------------
load(file=file.path(inPath,'eflaloNM.yrs.RData'))#unmatched logbook records from script 1.1
load(file=file.path(inPath,'tacsatEflalo.yrs.RData'))#VMS pings with allocated logbook catch and value
load(file=file.path(shapePath,"shapes.RData"))#the protected-area polygons, saved as 'shapes'

eflaloNM              <- as.data.frame(eflaloNM.yrs)#Make a regular data frame 
tacsatEflalo           <- as.data.frame(tacsatEflalo.yrs)

#- Transform protected-area polygons to WGS84 to match the VMS coordinates
shapes                <- st_transform(shapes,4326)

#- Select requested years
tacsatEflalo           <- subset(tacsatEflalo,year(SI_DATIM) %in% years)
eflaloNM              <- subset(eflaloNM,year(FT_DDATIM) %in% years &
                                  LE_RECT %in% ICESrect)#Keep unmatched records with a trip departure date in the selected years + a logbook rectangle in ICESrect

tacsatEflalo$YEAR      <- year(tacsatEflalo$SI_DATIM)#create year variable from the vms data
eflaloNM$YEAR         <- year(eflaloNM$FT_DDATIM)#and from the non matched

#Create one shared vessel-year lookup for both datasets independently assigned numbers each YEAR
vessel_ids            <- bind_rows(tacsatEflalo %>% select(YEAR,VE_REF),
                                   eflaloNM %>% select(YEAR,VE_REF)) %>%
  distinct(YEAR,VE_REF) %>%
  filter(!is.na(YEAR),!is.na(VE_REF)) %>%
  mutate(VE_REF=factor(VE_REF)) %>% 
  group_by(YEAR) %>%
  mutate(ANON_ID=paste(Country,YEAR,as.numeric(VE_REF),sep="_")) %>%
  ungroup()

#The same vessel receives the same ID in both datasets within a year, but a different ID across years.
tacsatEflalo           <- tacsatEflalo %>%
  select(-any_of("ANON_ID")) %>%
  mutate(VE_REF=as.character(VE_REF)) %>%
  left_join(vessel_ids,by=c("YEAR","VE_REF"))#combined tacstatEFlalo with vessel ids

eflaloNM              <- eflaloNM %>%
  select(-any_of("ANON_ID")) %>%
  mutate(VE_REF=as.character(VE_REF)) %>%
  left_join(vessel_ids,by=c("YEAR","VE_REF"))#combined non matched observations with vessel ids

rm(vessel_ids)

#-------------------------------------------------------------------------------
#- 2) Produce table containing number of vessels, effort, value and catch,
#-    by subareas per year and month.
#-------------------------------------------------------------------------------
# Create 1/16th ICES rectangle grid 
grid16                <- createGrid(xrange=c(2,8),
                                    yrange=c(51,56),
                                    resx=1/4,
                                    resy=1/8,
                                    exactBorder=TRUE,type="Grid")

#- Extract cell-centre coordinates before assigning the geographic CRS
grid_centres          <- st_coordinates(st_centroid(grid16))

#- Combine cell-centre coordinates with grid polygons
grid16                <- sf::st_sf(geometry=grid16)
grid16$SI_LONG        <- grid_centres[,1]
grid16$SI_LATI        <- grid_centres[,2]

#- Identify the ICES rectangle containing each cell centre
grid16$LE_RECT        <- ICESrectangle(data.frame(SI_LONG=grid16$SI_LONG,
                                                  SI_LATI=grid16$SI_LATI))

#- Assign cell IDs before selecting the relevant rectangles
grid16$ID             <- seq_len(nrow(grid16))
grid16                <- subset(grid16,LE_RECT %in% ICESrect)

#- Assign WGS84 longitude/latitude to the grid
grid16                <- st_set_crs(grid16,4326)

#- Check that each selected ICES rectangle contains 16 cells
table(grid16$LE_RECT)

#Determine the ICES rectangle containing each cell centre
grid16$ID             <- seq_len(nrow(grid16))#Assign each cell a unique number before filtering
grid16                <- subset(grid16,LE_RECT %in% ICESrect)#Keep only cells belonging to the selected ICES rectangles

#- Categorize vessel lengths
tacsatEflalo$LENCAT    <- cut(replace(tacsatEflalo$VE_LEN,tacsatEflalo$VE_LEN <=0,NA),breaks=c(0,10, 12,18,24,40, Inf),labels=c("0-10","10-12","12-18","18-24","24-40",">=40"), right = FALSE)#Assign each ping a vessel-length category in metres
eflaloNM$LENCAT       <- cut(replace(eflaloNM$VE_LEN,eflaloNM$VE_LEN <=0,NA),breaks=c(0,10, 12,18,24,40, Inf),labels=c("0-10","10-12","12-18","18-24","24-40",">=40"), right = FALSE)#Assign the same categories to unmatched logbook records
#missing values, lengths <=0 become NA


#- Match each point to a polygon 
first_polygon <- function(points,polygons){
  old_s2 <- sf_use_s2(FALSE)  #- Use planar matching and restore the previous setting afterwards
  on.exit(sf_use_s2(old_s2),add=TRUE)
  
  st_over(points,st_geometry(st_make_valid(polygons)))}#- Return matching polygon indices using st_over()

#- Convert VMS coordinates to sf points
pings   <- st_as_sf(tacsatEflalo[,c("SI_LONG","SI_LATI")], coords=c("SI_LONG","SI_LATI"),crs=4326)

#- Assign protected-area names and grid-cell IDs to the pings
inArea  <- as.character(shapes$Gebied)[first_polygon(pings,shapes)]
grid_index <- first_polygon(pings,grid16)
grid_ids  <- as.character(grid16$ID)
inGrid16  <- grid_ids[grid_index]

#- Define grouping columns 
#including ANON_ID to retain individual vessels in every aggregation; records without a valid vessel ID are excluded by the filters below.
group_cols            <- c("ANON_ID","YEAR","MONTH","GEAR","LENCAT","Area")

#- Perform aggregation for Natura 2000 area
tacsatEflalo$Area      <- inArea       #Sets Area to the protected-area name assigned to each ping

table.Art11           <- tacsatEflalo%>%
  mutate(YEAR  = year(SI_DATIM),       #Extracts the year from the ping timestamp
         MONTH = month(SI_DATIM),     #Extracts the month from the ping timestamp
         GEAR  = LE_GEAR)%>%         #Copies the fishing gear into the output grouping column
  filter(if_all(all_of(group_cols),~ !is.na(.x))) %>%  #excludes pings outside the protected areas (Area = NA)
  group_by(YEAR,MONTH,GEAR,LENCAT,Area) %>% #vessels are grouped for each area, year, month and length 
  summarise(across(matches("KG|EURO") | all_of("INTV"),#sum KG, value and effort for each year, month, gear, len 
                   ~ sum(.x,na.rm=TRUE)),
            N_vessels = paste(unique(ANON_ID),collapse=";"),
            .groups="drop") 


#- Perform aggregation for 1/16th grid
tacsatEflalo$Area      <- inGrid16    

table.Grid16          <- tacsatEflalo%>%
  mutate(YEAR  = year(SI_DATIM), MONTH = month(SI_DATIM), GEAR  = LE_GEAR)%>%         
  filter(if_all(all_of(group_cols),~ !is.na(.x)))%>%
  group_by(YEAR,MONTH,GEAR,LENCAT,Area)%>%
  summarise(across(matches("KG|EURO") | all_of("INTV"),~ sum(.x,na.rm=TRUE)),
            N_vessels = paste(unique(ANON_ID),collapse=";"),
            .groups="drop")

#- Generate table of eflaloNM effort
table.NM              <- eflaloNM%>%
  mutate(YEAR  = year(FT_DDATIM), MONTH = month(FT_DDATIM),GEAR  = LE_GEAR, Area  = as.character(LE_RECT))%>%
  filter(if_all(all_of(group_cols),~ !is.na(.x)))%>%
  group_by(YEAR,MONTH,GEAR,LENCAT,Area)%>%
  summarise(across(matches("KG|EURO") | all_of("INTV"),
                   ~ sum(.x,na.rm=TRUE)),
            N_vessels = paste(unique(ANON_ID),collapse=";"),
            .groups="drop")

#- Combine table of eflaloNM with tacsat-table
#- Match columns by name; missing species columns are filled with NA
final.table           <- bind_rows(table.Art11,table.Grid16,table.NM)%>% as.data.frame()                    
save(final.table,file=file.path(outPath,paste0("final.table.",Country,".Rdata")))#save table with country name

#- Save grid polygons with their IDs for subsequent mapping
save(grid16,file=file.path(outPath,"grid16.RData"))

summary(final.table)                  #Display a statistical summary of each output column
table(final.table$Area)               #Count output rows per area, not pings or vessels
###############################################END OF DATA CALL SCRIPT################################################
###################################below code is just for your own interest /checking the aggregated data############

#- Plot fishing effort and protected-area boundaries to check 
# this is not part of the data call but just for each country to check if their effort makes sense
library(ggplot2)

grid16    <- st_set_crs(grid16,4326)

#- Total fishing hours per cell
grid_effort <- table.Grid16 %>%
  group_by(Area, YEAR) %>%
  summarise(fishing_hours=sum(INTV,na.rm=TRUE)/60,
            .groups="drop")

#- Create grid_plot data
grid_plot <- grid16 %>%
  mutate(Area=as.character(ID)) %>%
  left_join(grid_effort,by="Area")# 

#below code plots of effort by grid cell per year with designed new protected areas indicated, this is just for each country if they would like to check the effort indicated
p <- ggplot() +
  geom_sf(data=grid_plot,
          aes(fill=fishing_hours),
          colour=NA) +
  geom_sf(data=shapes,
          aes(colour=Gebied),
          fill=NA,linewidth=0.8) +
  facet_wrap(~YEAR)+
  scale_fill_viridis_c(
    option="C",
    trans=scales::pseudo_log_trans(sigma=100),
    breaks=c(0,100,500,1000,5000,10000,30000),
    labels=scales::label_comma(),
    na.value="grey95",
    name="Fishing effort\n(hours)") +
  scale_colour_discrete(name="Areas of interest") +
  coord_sf(xlim=range(st_bbox(grid16)[c("xmin","xmax")]),
           ylim=range(st_bbox(grid16)[c("ymin","ymax")]),
           expand=FALSE) +
  labs(title="Total fishing effort",
       x="Longitude",y="Latitude") +
  theme_bw(base_size=11) +
  theme(legend.position="right")

print(p)