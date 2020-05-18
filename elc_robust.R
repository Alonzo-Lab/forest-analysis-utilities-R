#Preamble ----
# Author:       Mike Alonzo
#
# Date:        5/17/20
# Origin Date:  5/17/20
# Purpose:      At an arbitrary number of points build, implement, and test empirical line
#               corrections on any number of images with any number of bands.
# R version:    
# Input file:   shapefile of points; image layers
# Output files: ELC-corrected image and fit statistics based on ELC sample points and linear fit
#                Optionally, one can just output NDVI from here too.
# Notes:        
# Useful documentation: https://www.rdocumentation.org/packages/raster/versions/3.0-12/topics/extract
#If loading shapefile with readOGR (https://www.neonscience.org/dc-shapefile-attributes-r)

#
#############################################################################################

#activate necessary packages
library(raster)
library(sf) # shapefile packaage that should come with rgdal
library(rgdal)
library(snow) #for parallel processing though this may be outdated
#try this package that does not require snow (https://github.com/isciences/exactextractr)
library(ggplot2)
library(stringr)

#Specify user inputs ----
  #Filenames must have full path unless you're in the correct working directory
  #name of shapefile containing polygons to use to extract pixel values
  shapefile_filename = 'F:/ENVS_RS Dropbox/Data/DC/skysat/scenes/elc_pts.shp'

  #base image for ELC
  im_filename_base = 'F:/ENVS_RS Dropbox/Data/DC/skysat/scenes/20190401/toar_mosaic_20190401_geo.tif'
  #filename for image to be corrected
  im_filename_raw <- 'F:/ENVS_RS Dropbox/Data/DC/skysat/scenes/20190525/20190525_185020_toar_mos_geo.tif'
  #where to output raster (saving with "elcR" to note that ELC was done in R, not ENVI)
  output_filename <- paste(str_sub(im_filename_raw,1,-5),"_elcR.tif", sep="")
  #specify output filename for NDVI image
  output_filename_ndvi <-  paste(str_sub(im_filename_raw,1,-5),"_elcR_ndvi.tif", sep="")

#Load data ----

  # Load shapefile
  #not clear whether using st_read from sf or readOGR is better.
  elc_pts <- readOGR(shapefile_filename)
  #display key information like how many features and crs
  elc_pts
  #just show the attribute column names
  names(elc_pts@data)

  
  # Load images
  im_base <- stack(im_filename_base)
  num_layers <- nlayers(im_base)
  im_raw <- stack(im_filename_raw)
  
  #plot raster stack with elc points on top
  # Create an RGB image from the raster stack
  plotRGB(im_raw, r = 4, g = 3, b = 2, stretch="lin")
  plot(elc_pts,lwd=6, main="ELC sample points", add=TRUE)


#Extraction of pixel values using points ----

  #extract point values from the base image at each band
  values_df_base <- extract(im_base, elc_pts, small=TRUE, 
                           df=TRUE,nl=num_layers)
  
  #extract point values from the raw image at each band
  values_df_raw <- extract(im_raw, elc_pts, small=TRUE, 
                            df=TRUE,nl=num_layers)
  
  #plot for visualization purposes
  #basic plotting (without images, just using data extracted in arcmap)
  # dev.new()
  # Basic scatter plot
  df = cbind(values_df_base, values_df_raw)
  ggplot(subset(df, toar_mosaic_20190401_geo.4 < 1000), aes(x=toar_mosaic_20190401_geo.4, 
    y=X20190701_183041_toar_mos_geo.4)) + geom_point() +
    geom_abline() + geom_smooth(method=lm)
  
  #build linear model for correction based on ELC sample points
  #empty data frame to store corrected points
  len <- length(elc_pts) #how many elc points were imported?
  elc_corr <- data.frame(b1=rep_len(NA,len),
                   b2=rep_len(NA,len), 
                   b3=rep_len(NA,len), 
                   b4=rep_len(NA,len))
  
  #new, corrected raster
  im_raw_elc <- im_raw
  
  #empty df to collect fit statistics
  elc_fit_stats <- data.frame(r2=double(), rmse=double(), m=double(),
                              b=double())
  
  #loop through the 4 bands, develop correction at each and apply at each to im_raw
  for (band in 1:num_layers) {
    #add one to band because of Id column in the data frames
    elc_model <- lm(values_df_base[,band+1] ~ values_df_raw[,band+1])
    
    #just collecting the predicted values at the points for examination and fit stats
    pred <- predict(elc_model, new_data=values_df_raw[,band+1])
    idx_good_vals <- as.numeric(names(pred)) #use names attribute of pred as index
    elc_corr[idx_good_vals,band] <- pred
    
    #apply the linear models band by band to the images
    #(this method allows the raster structure to stay intact, unlike using "predict")
    im_raw_elc[[band]] <- im_raw[[band]]*elc_model$coefficients[2] + elc_model$coefficients[1]
    
    #store fit statistics
    elc_model_summary <- summary(elc_model)
    r2 <- elc_model_summary$adj.r.squared
    rmse <- sqrt(mean(elc_model$residuals^2)) 
    elc_fit_stats <- rbind(elc_fit_stats, data.frame(r2, rmse, elc_model$coefficients[[2]], 
                                                     elc_model$coefficients[[1]]))
    
  }
  
  #examine result of correction on stack
  plotRGB(im_raw_elc, r = 4, g = 3, b = 2, stretch="lin")
  
  #examine result (at sample points)
  df = cbind(values_df_base, elc_corr)
  ggplot(subset(df, toar_mosaic_20190401_geo.4 < 100), aes(x=toar_mosaic_20190401_geo.4, 
    y=b4)) + geom_point() +
    geom_abline() + geom_smooth(method=lm)
  
  #look at the fit stats
  elc_fit_stats

  #write output raster
  writeRaster(im_raw_elc, output_filename,
              format="GTiff", bylayer=FALSE, suffix='numbers',NAflag = 0, overwrite =TRUE)
  
  #if wanted, just create and output NDVI here too
  ndvi <- (im_raw_elc[[4]]-im_raw_elc[[3]])/(im_raw_elc[[4]]+im_raw_elc[[3]])
  
  #plot to make sure everything looks ok
  brks <- seq(0,1,by=0.01)
  plot(ndvi, breaks=brks, lab.breaks=brks,zlim=c(0,1))
  
  #write output NDVI raster
  writeRaster(ndvi, output_filename_ndvi,
              format="GTiff", bylayer=FALSE, suffix='numbers',NAflag = 0, overwrite =TRUE)
  
  