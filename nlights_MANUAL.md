---
title: "nlights"
author: 'Luke Shepherd'
numbersections: yes
output:
  html_document:
    df_print: paged
---


# Why this function exists

There is no onboard calibration of the optical sensor for the DMSP OLS satellite and only records the camera gain as a relative decibel value.

Furthermore, the dynamic range of the images are very low (0-63) this means areas of high brightness become very overexposed and blown out- the whole city area will register 63 however that only shows that the luminance is either equal to or greater than 63 with the actual urban distribution of luminance lost.

Additionally there is an issue with values on the lower end of the dynamic range. Firstly noise, as these images are taken at night the camera again is set very high to maximise the amount areas recorded – however this results in a high level of random noise in the image. This can interfere with calculations that look at the total amount of light emitted by an area. Furthermore the cameras are susceptible to pixel blooming [@mukherjeeAlgorithmIntercalibrationTimeSeries2017]. This is where the pixel surrounding very high intensity areas can artificially be activated further reducing the accuracy of the image.

Moreover, each generation of the satellite has a different optical sensor which has been calibrated differently- which will also degrade differently to previous generations. Meaning you cannot make a direct comparison between generations.

During its life cycle the sensor degrades as it is repeatedly exposed to ionising solar radiation and charged particles- exacerbated by the satellites orbital path running through the Van Allen belts (which contain a large amount of charged particles). This means you can not compare the same satellite year to year.

In order to make inferences from time series nightlight data the images have to be inter-calibrated to normalise the sensor degradation and the differences between satellite generations.

This function serves to collate and simplify workflows for inter-calibration as well as expanding inter-calibration automation into R- as existing scripts are only available for ArcGIS and in Python.

# Inter-calibration

Most inter-calibration methods identify areas that show very little change of nightlight activity over time (Pseudo invariant features, PIF) and use that area for the calibration.

There are multiple methods of inter-calibration, however the original and most widely used is @elvidgeNationalTrendsSatelliteObserved2014a. This uses Sicily as its PIF with the F121999 satellite year as the assumed stable year. A polynomial regression is then performed against each other year to generate coefficients which are used to calibrate that year.

$$y=C0 + C_1X +C_2X^2$$


Alternative methods include @wuIntercalibrationDMSPOLSNighttime2013 uses  power mode and a composite of Okinawa, Porto Rico and Mauritius for the PIFs 

$$DN_c +1 = a (DN_m + m +1)^b$$

*$$DN_c$$ = DN value after calibration*
*$$DN_m$$ = DN value before calibration* 
*a and b are the model coefficients* 


# Dependencies

## Libraries

- `raster`
- `rgdal`
- `tidyverse`
- `viridis`
- `rgeos`
- `tmap`
- `sf`

## Nightlight images

Nightlight images can be downloaded from

    https://ngdc.noaa.gov/eog/dmsp/downloadV4composites.html

Use the *'Average Visible, Stable Lights, & Cloud Free Coverages'*

## File path

You need to have a directory *Nightlights* with un-zipped files.

Your file tree should look like this:

    working directory
    |
    |-Nightlight
    |   |- F121999.v4
    |   |   |---F121999.v4b_web.avg_vis.tfw
    |   |   |---F121999.v4b_web.avg_vis.tif.gz
    |   |   |---F121999.v4b_web.cf_cvg.tfw
    |   |   |---F121999.v4b_web.cf_cvg.tif.gz
    |   |   |---F121999.v4b_web.stable_lights.avg_vis.tfw
    |   |   |---F121999.v4b_web.stable_lights.avg_vis.tif
    |   |   |---F121999.v4b_web.stable_lights.avg_vis.tif.gz
    |   |   |---README_V4.txt

\

 You will need F121999.v4b_web.stable_lights.avg_vis.tif downloaded and have it in the Nightlight directory. This is the image that is used for the Elvigdge calibration.

You will need the leave the file names unchanged the exact names are used further down the code for subsetting and renaming.

## Shape Files

Sicily .shp file- Sicily is used as the calibration site for Elvigdge

The shp file can be called with an absolute file path- does not need to be in a specific directory

The file can be downloaded from:


    http://www.marineregions.org/gazetteer.php?p=details&id=3383

## Wu Calibration limits

This calibration is based on Wu's coefficients from his 2013 paper. They are not generated organically. This means that you can only calibrate satellites/years that he has published data for.

The CSV can be downloaded from: 

    www.gitlab.com/NikosAlexandris/i.nightlights.intercalibration/raw/master/archived/wu_2013.csv

This is the list of satellite/year:

|Satellite | Year|
|---|---|
|F10 |1992|
|F10 |1993|
|F10 |1994|
|---|---|
|F12 |1994|
|F12 |1995|
|F12 |1996|
|F12 |1997|
|F12 |1998|
|F12 |1999|
|---|---|
|F14 |1997|
|F14 |1998|
|F14 |1999|
|F14 |2000|
|F14 |2001|
|F14 |2002|
|F14 |2003|
|---|---|
|F15 |2000|
|F15 |2001|
|F15 |2002|
|F15 |2003|
|F15 |2004|
|F15 |2005|
|F15 |2006|
|F15 |2007|
|---|---|
|F16 |2004|
|F16 |2005|
|F16 |2006|
|F16 |2007|
|F16 |2008|
|F16 |2009|
|---|---|
|F18 |2010|


# nlights
@CorrectWaySpecifiy2015 was drawn upon for the conditional code chunks and @jacquesNighttimeLightsCalibration2018 provided the loading code and the framework for the  nested for loops.

## Function Variables

`target`    

File path to the shp file. This is the area that you want to calibrate.

`Sicily`     

File path to a shp file of Sicily. The default @elvidgeNationalTrendsSatelliteObserved2014a calibration method uses Sicily

`Wu`       

File path to Wu coefficients CSV  This will set @wuIntercalibrationDMSPOLSNighttime2013 as the inter-calibration method

`years`     

Vector, sets which years will be used for the plots

`map`       

Logical, TRUE/NULL, generates maps for the target area

`PRIO`      

File path to a PRIO grid shp file, generates maps for the target area with an overlayed PRIO grid

`tsol`      

Logical, TRUE/NULL, generates a total sum of lights graph

`pixel`     

Logical, TRUE/NULL, generates a pixel calibration graph- showing what part of the dynamic range has been altered in calibration




```{r eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
nlights(
  target = 'sen_admbnda_adm0_1m_gov_ocha_04082017/sen_admbnda_adm0_1m_gov_ocha_04082017.shp',
  sicily = 'Sicily/Sicily.shp',
  map = T,
  PRIO='priogrid_cellshp/priogrid_cell.shp',
  pixel = T,
  tsol = T,
  Year = 2000:2005
  )
```


## Load files

This loads the nightlight files from the directory *Nightlight*  by creating a list of filenames that have been unzipped (and therefore have .tif extension). They are then put into a raster stack and had there coordinates normalised.

```{r eval=FALSE}
# Load files----
# This will load ALL .tiff files in Nightlight
filenames <-
  list.files(
    "Nightlight",
    pattern = ".tif$",
    recursive = TRUE,
    full.names = TRUE
  )

#Load the data you want into a Raster stack
nlight <- stack(filenames)
print(filenames)

# Convert them to the same coord system
crs(nlight)
```

## Target Shapefile

This loads the *target* shapefile. This is the area of the world that you are interested and want to calibrate. To reduced computational load the function doesn't calibrate the full global image. Instead, it crops the target area using the shapefile and then calibrates only that section.

Also in this step DN values > 62 and < 3 are removed @elvidgeNationalTrendsSatelliteObserved2014a. Then DMSP OLS images only record data a light value of 0-63. This small dynamic range means that areas of high light intensity only recorded as 63 even if

Values < 3 are removed to counter random camera noise and the pixel blooming effect [@mukherjeeAlgorithmIntercalibrationTimeSeries2017].


```{r eval=FALSE}
   # Target Shapefile----
    target <- readOGR(target)

    print("converting target coordinates")
    target <- spTransform(target, crs(nlight))

    print("croping the global file to the extent of the target area")
    nlight.target <- crop(nlight, extent(target))

    # this only cuts to the bounding box
    target.mask <- mask(nlight.target, target)

    # This makes all vaules ouside of shp NA
    nlight.target[is.na(target.mask)] <- NA

    # saving an original, uncalibrated target object for future calibration
    nlight.target_org <- nlight.target

    print("Removing over saturated DN/ noise")
    nlight.target[nlight.target > 62] <- NA
    nlight.target[nlight.target < 3] <- NA

```

## Calibration


### Wu Calibration @wuIntercalibrationDMSPOLSNighttime2013
This is the optional calibration method- if a file path is assigned to `Wu` then this code will execute. As this calibration is limited to satellite/years that are in the CSV

The nested loops check every satellite image against the ones in the CSV; if present, it is then calibrated and renamed. There is also a calibration counter (an object with an integer value that increases by one after each calibration) which ends the loop once it reaches the number of satellite images loaded.

It then removes all of the uncalibrated images from the raster stack and prints a warning message for each one removed; reduces any 'impossible' DN values (one's greater than 63) back within the range of the satellite and pushes the calibrated images into the global environment.


```{r eval=FALSE}
 if (!is.null(Wu)) {
      # Wu Calibration----

      print("Wu Calibration")

      wu.csv <- read_csv(Wu)

      #merges year/ satellite
      wu.csv$Satellite <- paste(wu.csv$Satellite, wu.csv$Year)

      #removes year column
      wu.csv <- wu.csv %>%
        select(-c(Year))

      # removes the space between FxxYEAR
      wu.csv$Satellite <-
        gsub(pattern = " ",
             replacement = "",
             wu.csv$Satellite)

      # makes a duplicate for formatting
      nlight.target_cal <- nlight.target

      # makes a calibration counter
      cal_count <- 0

      #Every satellite layer is checked against the names of the CSV.
      #If they match; they are calibrated and renamed- if not; skipped.
      #Once all of the layers have been calibrated- the loop is ended.

      for (i in 1:nlayers(nlight.target)) {
        print(paste("Layer", i))

        #nested loop for pairwise comparison
        for (j in 1:nrow(wu.csv)) {
          print(j)

          if (grepl(wu.csv$Satellite[j], names(nlight.target[[i]]))) {
            # prevents negative values- Wu2013
            nlight.target_cal[[i]] <- nlight.target_cal[[i]] + 1

            # calibration from coefficients- Wu2013
            nlight.target_cal[[i]] <-
              wu.csv$a[j] * ((nlight.target[[i]] + 1) ^ wu.csv$b[j])

            # renames calibrated satellite
            names(nlight.target_cal[[i]]) <- wu.csv$Satellite[j]

            print(paste(
              "Sucessfull Calibration of",
              names(nlight.target_cal[[i]])
            ))
            cal_count <- (cal_count + 1)

          }

          # checks to see if the are still layers to call
          else if (cal_count != nlayers(nlight.target)) {
            print("still looking...")
          }

          #stops the loop once all of the layers have been calibrated
          else if (cal_count == nlayers(nlight.target)) {
            break
          }

        }

      }

      #sets copies for easy formatting
      nlight.target_cal.inter <- nlight.target_cal

      uncal.layers <- (NULL)

      #Renames uncalibrated layers
      for (i in 1:nlayers(nlight.target_cal)) {
        # identifies layers that haven't had there name changed - therefor uncalled
        if (grepl('avg_vis', names(nlight.target_cal[[i]]))) {
          uncal.layers <- rbind(uncal.layers, c(i, row.names = NULL))

          #prints calibration warnings
          print("!!!!!!!!!!!!!!!!!!!!!!!!!")

          print(paste('UNCALIBRATED', names(nlight.target_cal[[i]])))

          print("Not in Wu(2013) CSV")

          print("!!!!!!!!!!!!!!!!!!!!!!!!!")


        } else{
          next
        }

      }


      nlight.target_cal <- dropLayer(nlight.target_cal, uncal.layers)

      nlight.target <- dropLayer(nlight.target, uncal.layers)

      print(names(nlight.target_cal))

      # removes impossible over 63 values
      nlight.target_cal[nlight.target_cal > 63] <- 63

      #rewrite  the onto the original name
      #pushes it to global environment
      nlight.target.Wu_cal <<- nlight.target_cal
```


### Elvidge Calibration @elvidgeNationalTrendsSatelliteObserved2014a
This is the default calibration. Unlike `Wu` the coefficients are generated by running the calibration method, therefore not limited to any particular years.

The code loads the Sicily shp file for analysis; runs regression for values from that area which generate the coefficients needed for calibration. An Impossible values are then reduced and the finished objected is pushes to the global environment. 


```{r eval=FALSE}
 }else{
      #Elv calibration----
      print('ELV calibaration')

      Sicily <- readOGR(sicily)
      #ELV Sicily

      print("calibration shapefile loaded")

      Sicily <- spTransform(Sicily, crs(nlight))
      nlight.Sicily <- crop(nlight, extent(Sicily))

      nlight.Sicily <- mask(nlight.Sicily, Sicily)
      #remeber mask(raster_data, shp.file)
      # this only cuts to the bounding box

      nlight.Sicily[is.na(nlight.Sicily)] <- NA

      # Removes over saturated DN/ noise
      nlight.Sicily[nlight.Sicily > 62] <- NA
      nlight.Sicily[nlight.Sicily < 3] <- NA


      # ELV regression calibration (data)
      print("running regression...")

      #F121999 is separated from the raster stack and individually defined-
      #this means that the data can be imputed in any order
      #F12199 selected out of raster stack by file name
      cal_year_1999 <- grep("F121999", names(nlight.Sicily))

      # F12199 defined independently
      nlight.Sicily[[cal_year_1999]]

      # Creates empty data.frame for loop outputs
      # defined outside the loop
      Elv_coef = NULL

      for (i in 1:nlayers(nlight.Sicily)) {
        d <- data.frame('x' = getValues(nlight.Sicily[[cal_year_1999]]),
                        'y' = getValues(nlight.Sicily[[i]]))
        #remove NA vaules to allow for regrestion analisis
        d <- na.omit(d)
        model <- lm(y ~ x + I(x ^ 2), data = d)

        # intercept: C0
        c0 <- (model)$coefficients[1]

        # x coefficent: C1
        c1 <- (model)$coefficients[2]

        #I(x^2) coefficent: C2
        c2 <- (model)$coefficients[3]

        #FxxYEAR takes the first 7 character of the file name to identify satellite and year
        year <- (nlight.Sicily[[i]]) %>%
          names() %>%
          substr(1, 7)

        #data.frame of coefficients for each year
        Elv_coef <-
          rbind(Elv_coef, data.frame(year, c0, c1, c2, row.names = NULL))
      }

      # Calibration of nlights (ELV)

      #makes a duplicate raster stack for ease of formatting
      #goes outside the loop so it is not wiped on each repeat
      nlight.target_cal <- nlight.target

      for (i in 1:nlayers(nlight.target)) {
        # applies Elvidge calibration coefficients for each layer of the raster stack
        # order is alright bc Elv_coef is generated from nlight order
        nlight.target_cal[[i]] <-
          Elv_coef$c0[i] + (Elv_coef$c1[i] * nlight.target[[i]]) + (Elv_coef$c2[i] *
                                                                      (nlight.target[[i]] ^ 2))

        # renames each layer with shortened satellite name (FxxYEAR)
        names(nlight.target_cal[[i]]) <- Elv_coef$year[i]
        print(names(nlight.target_cal[[i]]))

      }

      print('impossible values reduced')
      nlight.target_cal[nlight.target_cal > 62] <- 63


      #pushes to global environment
      nlight.target.ELV_cal <<- nlight.target_cal

    }
```

## Years
If you only want to generate plots for certain years, you can manually set which years are used for creating plots (if no are set- all years will be used).

This subsets the calibrated years to only the ones that have been called.

```{r  eval=FALSE}
    # Years----
    #Limits the maps the the years chosen only returns year that 
    # have been chosen and have been calibrated by the model-
    #this particularly applies to the limitations of the Wu CSV
    print("years start")

     if (!is.null(years)) {

       years <- as.list(years)


      if (!is.null(Wu)) {
        for (i in 1:length(years.list)) {
          if (years.list[i] %in% years) {

            years.list.inter <- rbind(years.list.inter,
                                      c(substr(names(nlight.target_cal[[i]]), 4, 7), row.names = NULL))
          }else{
            next
          }

        }
          years.list <- years.list.inter

      } else{
        # matches the layer number of the nlight output to the input years
        for (i in 1:nlayers(nlight.target_cal)) {
          if (substr(names(nlight.target_cal[[i]]), 4, 7) %in% years) {
            years.list.inter <- as.list(c(years.list.inter, (substr(
              names(nlight.target_cal[[i]]), 4, 7
            ))))
          }
        }

      years.list <- years.list.inter

      }

      #subset nlight.target_cal/ nlight.target to years

      nonyear.layers <- (NULL)

      for (i in 1:nlayers(nlight.target_cal)) {
        if (!(substr(names(nlight.target_cal[[i]]), 4, 7) %in% years.list)) {
          nonyear.layers <- rbind(nonyear.layers, c(i, row.names = NULL))
        }
      }

      nlight.target_cal <- dropLayer(nlight.target_cal,  nonyear.layers)
      nlight.target <- dropLayer(nlight.target,  nonyear.layers)


    }
```



## Ploting

### Map
This creates a map for each year of the target area using the calibrated satellite images.
Each map plot is named with its year as its name and saved into the plotting environment `nlight.output.plots`.


```{r eval=FALSE}
# Map----
    if (!is.null(map)) {
      print("plotting map...")
      print(years.list)

      #makes a map for each year
      for (j in 1:length(years.list)) {
        print(years.list[[j]])

        assign(
          paste("map.", years.list[[j]],sep=""),
          envir = nlight.output.plots,
          tm_shape(nlight.target_cal[[j]]) + tm_raster(palette = "viridis", n = 20) +
            tm_shape(target) + tm_borders() +
            tm_layout(legend.outside = TRUE)
        )
      }
    }
```



### PRIO
This creates the same maps the `map` argument- except these will have a PRIO grid overlaid. As the PRIO shp file is so large the function first checks if it is present in the environment- if not it is loaded, and then pushed to the global environment; meaning it should only have to be loaded once per session.
Again like the maps, they are saved to the `nlight.output.plots` environment.


```{r eval=FALSE}
    # PRIO----
    if (!is.null(PRIO)) {
      if (!exists('PRIO.data')) {
        # using read_sf because readOGR takes FOREVER
        print("loading PRIO...This takes a LONG time be patient")
        PRIO.data <- read_sf(PRIO)
        # converting to spacial object
        print("converting PRIO to spacial object...continue being patient")
        PRIO.data <- as_Spatial(PRIO.data)
        #crops the PRIO grid
        print("cropping PRIO...don't worry, almost done!")

        #PRIO.target<- crop(PRIO, extent(target))
        PRIO.target <- gIntersection(PRIO.data, target, byid = TRUE)

        PRIO.data <<- PRIO.data

      } else if (exists('PRIO.data')) {
        print("PRIO data already loaded, cropping PRIO...be patient")

        #PRIO.target<- crop(PRIO, extent(target))
        PRIO.target <- gIntersection(PRIO.data, target, byid = TRUE)
      }

      print("plotting PRIO maps...")
      #makes a map for each year
      for (j in 1:length(years.list)) {
        assign(
          paste("PRIO.map.", years.list[j],sep=""),
          envir = nlight.output.plots,
          tm_shape(nlight.target_cal[[j]]) + tm_raster(palette = "viridis", n = 20) +
            tm_shape(target) + tm_borders() +
            tm_layout(legend.outside = TRUE) +
            tm_shape(PRIO.target) + tm_borders()
        )
      }
    }
```


### Total Sum of Lights

This generates TSOL plot for both the calibrated and uncalibrated values of the target area and each of the satellite generations.


```{r eval=FALSE}
# TSOL----
    if (!is.null(tsol)) {
      print("plotting TSOL...")

      #sum of all DN values in the target area
      cal_target.TSOL <- cellStats(nlight.target_cal, sum)
      uncal_target.TSOL <- cellStats(nlight.target, sum)

      print("tsol vaules comp")

      #data.frame created for ggplot y=tsol x= year(character subset of file names)
      tsol <-
        data.frame(
          'sat.year' = names(nlight.target_cal),
          'TSOL_cal' = cal_target.TSOL,
          'TSOL_uncal' = uncal_target.TSOL,
          'sat' = substr(names(nlight.target_cal), 1, 3),
          row.names = NULL
        )

      tsol <- na.omit(tsol)

      #plot is structured so each geom_point is an independent area and dataframe

      tsol.plot <-
        ggplot(data = tsol, aes(
          x = tsol$sat.year,
          group = 1
        )) +
        scale_color_brewer(type = "seq", palette = 'Dark2') +
        geom_point(aes(
          y = TSOL_cal,
          size = 1,
          color = tsol$sat
        )) +
        geom_line(
          aes(y = TSOL_cal, colour="Calibrated"),
          color = 'blue',
          size = 1,
          alpha = 0.6
        ) +
        geom_point(aes(
          y = TSOL_uncal,
          size = 1,
          color = tsol$sat
        )) +
        geom_line(
          aes(y = TSOL_uncal,colour="uncalibrated"),
          color = 'red',
          size = 1,
          alpha = 0.6
        ) +
        theme(axis.text.x = element_text(angle = 45)) +
        labs(
          x = "Satellite/Year",
          y = "Total Sum of Lights (DN vaules)",
          color = "Satellite") +
        guides(size = FALSE)


      nlight.output.plots$tsol.plot <- tsol.plot
    }


```



### Pixel
This generates a comparison of the calibrated and uncalibrated distribution of the pixels in the target area- showing how the pixel values have been altered through calibration.


```{r eval=FALSE}
    #Pixel----
    if (!is.null(pixel)) {
      # Pixel change during calibration
      print("plotting pixels")

      #makes a pixel map for each year
      for (j in 1:length(years.list)) {
        print(years.list[[j]])
        d <-
          data.frame(
            'uncal' = getValues(nlight.target_org[[j]]),
            'cal' = getValues(nlight.target_cal[[j]])
          )

        assign(
          paste("pixel.", years.list[j],sep=""),
          envir = nlight.output.plots,
          ggplot(d, aes(
            x = d$uncal, y = d$cal
          )) +
            geom_bin2d() +
            theme_bw() +
            coord_fixed(ratio = 1) +
            geom_abline(color = "red") +
            scale_fill_viridis(discrete = FALSE) +
            xlab(
              nlight.target[[j]] %>%
                names() %>%
                substr(1, 7) %>%
                paste("Un-calibrated")
            ) +
            ylab(
              nlight.target_cal[[j]] %>%
                names() %>%
                substr(1, 7) %>%
                paste("Calibrated")
            )
        )
      }

    }

```


### Testing

The internal extraction of the coefficients for Elvidge was checked against most up-to-date coefficients generated by Elvidge (available through chris.elvidge@noaa.gov).

Furthermore the TSOL and pixel plots demonstrate that calibration has occurred; altering both the total cumulative DN for the target area and the distribution of DN values on the pixel heat map.


# Limitations
There are two notable omitions in this package that are common in the literature.

## @zhangRobustMethodGenerate2016
This method uses ridgeline regression and is one of the more powerful and globally applicable inter-calibration methods [@pandeyComparativeEvaluationRelative2017].

## SNDI

The sum of normalised differences is commonly used to check the effectiveness of calibration [@elvidgeNationalTrendsSatelliteObserved2014a; @mukherjeeAlgorithmIntercalibrationTimeSeries2017; @pandeyComparativeEvaluationRelative2017; @wuIntercalibrationDMSPOLSNighttime2013; @zhangRobustMethodGenerate2016] However such a test cannot be performed within the function- instead TSOL values would have to be manually generated from the calibrated nightlight images and then perform the calculation.


$$SNDI=\sum NDI_t$$

$$NDI_t=\frac{\lfloor TSOL_{1t} - TSOL_{2t} \rfloor}{TSOL_{1t} + TSOL_{2t}}$$

*t= moment in time*

*TSOL = Total sum of lights*

# Refences 
