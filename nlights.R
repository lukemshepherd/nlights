nlights <-
  function(target,
           sicily = NULL,
           Wu = NULL,
           years = NULL,
           map = NULL,
           PRIO = NULL,
           tsol = NULL,
           pixel = NULL) {
    #loads libraries
    library(raster)
    library(rgdal)
    library(tidyverse)
    library(viridis)
    library(rgeos)
    library(tmap)
    library(sf)
    
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
    
    
    # Target Shapefile----
    target <- readOGR(target)
    
    print("converting target coordinates")
    target <- spTransform(target, crs(nlight))
    
    print("croping the global file to the extent of the target area")
    nlight.target <- crop(nlight, extent(target))
    
    print("mask")
    # this only cuts to the bounding box
    target.mask <- mask(nlight.target, target)
    
    print("Na")
    # This makes all vaules ouside of shp NA
    nlight.target[is.na(target.mask)] <- NA
    
    print("rename")
    # saving an original, uncalibrated target object for future calibration
    nlight.target_org <- nlight.target
    
    print("Removing over saturated DN/ noise")
    nlight.target[nlight.target > 62] <- NA
    nlight.target[nlight.target < 3] <- NA
    
  
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
      
      #Every satellite layer is checked against the names of the CSV. If they match; they are calibrated and renamed- if not; skipped. Once all of the layers have been calibrated- the loop is ended.
      
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
    
    #Year list(setup)----
    print('ploting enviromnet created')
    nlight.output.plots <<- new.env()
    
    
    print('years list')
  
    years.list <- as.list(substr(names(nlight.target_cal), 4, 7))
    
    
    # creates an empty object
    years.list.inter <- (NULL)

    
    
    # Years----
    #limits the maps the the years chosen only returns year that have been chosen and have been calibrated by the model-
    #this particularly applies to the limitations of the Wu CSV
    print("years start")
    
     if (!is.null(years)) {
      
       years <- as.list(years)
       
       
      if (!is.null(Wu)) {
        for (i in 1:length(years.list)) {
          if (years.list[i] %in% years) {
            
            years.list.inter <- rbind(years.list.inter, c(substr(names(nlight.target_cal[[i]]), 4, 7), row.names = NULL))
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
    

    
    
    # Map----
    if (!is.null(map)) {
      print("plotting map...")
      
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
          aes(y = TSOL_cal, color="Calibrated"),
          size = 1,
          alpha = 0.6
        ) +
        geom_point(aes(
          y = TSOL_uncal,
          size = 1,
          color = tsol$sat
        )) +
        geom_line(
          aes(y = TSOL_uncal,color="Uncalibrated"),
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
    
    print('nlights finished')
  }
