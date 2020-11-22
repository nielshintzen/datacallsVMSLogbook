Data request Seine fishery on the Doggerbank

Goal; determine who, how much and for what species the Dutch and German part of the Doggerbank are fished with Danish and Scottish seines. 
In the North Sea agreement, the Dutch part of the Doggerbank is proposed to be closed to seines. This report will be used for discussion in the Scheveningen group 

Client; Dutch Ministry of Economic Affairs, (J.B.F. Vonk,  <j.b.f.vonk@minlnv.nl>).

The current project is an update from study, Hamon et al., 2018 in which the years 2010 till 2015 were included. 
Next to Dutch Fishery information, Germany, UK, France, Sweden and Denmark have contributed fishery data. In this update the fishery intensity and catch in the area is investigated for the timeframe 2015 till 2019.
In addition the bottom impact of the seines is evaluated using the BENTHIS method


The following information is required;
Effort (in days at sea) and catch (in KG and EURO) and catch (in KG) for the top 10 species at for the seine fleets in ICES areas adjacent to the Dutch and German parts of the Doggerbank, in the Dutch and German parts of the Doggerbank and in the Dutch and German management zones of the Doggerbank. 
The swept area of the seine fleets at a 1' by 1' resolution grid.
   
By running two scripts the required information can be extracted from (cleaned) tacsat and eflalo datasets. The following data files (provided) are required:

Datafiles;
1.	DB_withoutUK.RData		– contains shapes the German and Dutch Doggerbank areas.
2.	grd.RData 		        - contains 1' x 1' grid of relevant ICES rectangles
3.	DB_withoutUK_ices.RData		- contains the ICES rectangles adjacent to the study area
4.	DB_mngtareas_withoutUK.RData	- contains shapes of the German and Dutch management zones of the Doggerbank


Script ‘1.1 DataExtractionFlyshoot_DB.R’
Selects, from clean eflalo and tacsat datasets, seine fishery conducted in the Doggebank region for the years 2013 till 2019. 


Script ‘1.2 CalculationsFlyshoot_DB.R.RData’. 
Calculates the effort in days at sea, catch (in KG and EURO) and catch (in KG) for the top 10 species for the German and Dutch Doggerbank areas (in management zones as well as total areas) 
and the swept area for a 1' x 1' grid of 7 ICES rectangles adjacent to the study area.

! you should make sure to adapt the method to define the activity per ping to the best available method for your data

PLEASE RETURN THE FILES IN THE shareOutput FOLDER or via email to katell.hamon@wur.nl & sander.glorius@wur.nl

 



