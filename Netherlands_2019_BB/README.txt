Data request Gillnet fishery 

Goal; determine effort and catch of the gillnet fishery in relation to conservation objectives of sea birds in the Brown Ridge. Several variants of Brown Ridge Marine Protective Area options, which differ in shape and size, are under investigation. 

Client; Dutch Ministry of Economic Affairs, (Sk. K. Lubbe, s.k.lubbe@minez.nl).

The current project is an update from study, Jongbloed et al., 2015 in which the years 2012 till 2014 were included. Next to Dutch Fishery information, Germany and Denmark have contributed fishery data. In this update the fishery intensity and catch in the area is investigated for the timeframe 2014 till 2017.


The following information is required;
Effort (in days at sea, km-net days) and catch (in KG and EURO) at for the complete fleet in the Greater North Sea, at 1/16 ICES rectangles and in the different MPA options. 

By running two scripts the required information can be extracted from (cleaned) tacsat and eflalo datasets. The following data files (provided) are required:

Datafiles;
1.	BBtot.RData 		– contains shapes of all Brown Ridge variants.
2.	grd.RData 		- contains grid of relevant 1/16 ICES rectangles
3.	grdOverlabBR.RData	- contains %overlap of gridcells with 1/16 ICES rectangles
4.	ICESR.RData		- 
5.	icesstatq.ext.rda		- Needed?
6.	Rect16.RData		- % overlap of land per 1/16 ICES rectangle


Script ‘1.1 DataExtractionBU.R’
Selects, from clean eflalo and tacsat datasets, gillnet fishery conducted in the ‘Greater North Sea’ for the years 2014 till 2017. Only vessels with an average yearly effort exceeding 1 day are included. 
!make sure to select all appropriate gear types as these might differ per country

Script ‘1.2 CalculationsBU.RData’. 
Calculates the effort in days at sea, minimal- and maximal km-net days, and total catch in kg and EURO as well as separate for SOL, COD, MUL and BSS. Based on catch and meshsize the following gillnet fishery types are defined; (1) Mullets and Sea bass, (2) Sole and (3) Cod. Results includes monthly effort and catch at 1/16 ICES rectangle spatial scales (‘results.RData’), effort and catch for specific Brown Ridge variants (‘BRdata.RData’) and for several more broad spatial selections (‘Stat.RData’).


PLEASE RETURN THE FILES IN THE shareOutput FOLDER

 



Jongbloed, R.H., M.A.M., Machiels, J.T. Wal van der., K.G. Hamon, J.A.E. Oostenbrugge (2015). “Assessment of the impact of gillnet fishery on conservation objectives of seabirds in the Brown Ridge, WMR report C182?15, pp 43.
