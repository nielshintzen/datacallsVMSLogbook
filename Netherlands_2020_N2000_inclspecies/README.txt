Data request all fishery in Dutch Natura 2000 areas

Goal; determine who, how much the Dutch potential Natura 2000 areas are fished. 
In the North Sea agreement, the Natura 2000 parts are closed to several types of fisheries. This report will be used for discussion in the Scheveningen group 

Client; Dutch Ministry of Economic Affairs, (J.B.F. Vonk,  <j.b.f.vonk@minlnv.nl>).

Next to Dutch Fishery information, Germany, UK, France, Sweden and Denmark have contributed fishery data. In this study the fishery intensity and catch in the area is investigated for the timeframe 2014 till 2019.

The following information is required;
Effort (in days at sea) and catch (total in KG and EURO) in the Dutch Natura 2000 areas. 
In this call we also ask for the catch of the main species in KG in order to understand which fisheries/species are caught in the areas.

By running two scripts the required information can be extracted from (cleaned) tacsat and eflalo datasets. The following data files (provided) are required:

Datafiles;
1.	shapes.RData			– contains shapes the Dutch Natura 2000 areas.

Script ‘1.1 getData_N2000.R’
Selects, from clean eflalo and tacsat datasets, all types of fishery conducted in the area around the Natura 2000 areas from 2014-2019


Script ‘1.2 Calculations_N2000.R.RData’. 
Calculates the effort in days at sea, catch (in KG (total and top species) and EURO (total)) (in management zones) 

! you should make sure to adapt the method to define the activity per ping to the best available method for your data

PLEASE RETURN THE FILES IN THE shareOutput FOLDER or via email to katell.hamon@wur.nl & bea.deetman@wur.nl

 



