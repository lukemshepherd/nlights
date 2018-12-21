# nlights
R function that inter-calibrates nightlight images

See HTML for full explaination.

Nightlight images can be downloaded from: https://ngdc.noaa.gov/eog/dmsp/downloadV4composites.html

This is a slow function- give it time, it will work

Don't change the file name of the images (SEE MANUAL)

You will need these libraries:
 - `raster`
 - `rgdal`
 - `tidyverse`
 - `viridis`
 - `rgeos`
 - `tmap`
 - `sf`
 
## Function Variables

`target`     File path to the shp file. This is the area that you want to calibrate.

`Sicily`     File path to a shp file of Sicily. The default (Elvidge 2014)calibration method uses Sicily

`Wu`       File path to Wu coefficients CSV  This will set (Wu 2013) as the inter-calibration method

`years`     Vector, sets which years will be used for the plots

`map`       Logical, TRUE/NULL, generates maps for the target area

`PRIO`      File path to a PRIO grid shp file, generates maps for the target area with an overlayed PRIO grid

`tsol`      Logical, TRUE/NULL, generates a total sum of lights graph

`pixel`     Logical, TRUE/NULL, generates a pixel calibration graph- showing what part of the dynamic range has been altered in calibration


## Quick Start:

Calibration with (Elvidge 2014): `nlights(target, Sicily)`

Calibration with (Wu 2013) with maps and tsol: `nlights(target, Wu, map=TRUE , tsol=TRUE)`

