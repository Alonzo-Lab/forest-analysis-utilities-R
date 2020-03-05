#Preamble ----
# Author:       Mike Alonzo
#
# Date:        3/4/2020
# Origin Date:  3/4/2020
# Purpose:      Use polygons to extract pixels from an arbitrary number of image layers of arbitrary resolutions. 
#               Make into a data frame or other useful object.
# R version:    3.6.2
# Input file:   shapefile of polygons; image layers
# Output files: TBD
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

#Specify user inputs ----
  #Filenames must have full path unless you're in the correct working directory
  #name of shapefile containing polygons to use to extract pixel values
  shapefile_filename = 'F:/ENVS_RS Dropbox/Projects/DC_pheno/shapefiles/dc_pheno_crowns.shp'
  #set the fieldnames for your polygon unique ID and the class field (e.g., species, LCZ type)
  #case sensitive. Match must be exact.
  uid_fieldname = "UID"
  class_fieldname = "SpCode"
  #image with any number of bands whose pixels will be extracted
  im_filename = 'F:/ENVS_RS Dropbox/Data/DC/skysat/scenes/20190401/toar_mosaic_20190401_geo.tif'
  #im_filename <- 'F:/ENVS_RS Dropbox/Data/DC/lidar/nDSM_20180405/nDSM.tif'
  #where to output csv
  output_filename = 'C:/Users/alonzo/Documents'
  #parallel processing (do not use for now)
  #num_nodes =15
  
  #select records by attribute if desired
  #polys <- polys[polys$SpCode=="QUPA",] #comma means that we're retaining all columns

#Load data ----

  # Load shapefile
  #not clear whether using st_read from sf or readOGR is better.
  polys <- readOGR(shapefile_filename)
  #display key information like how many features and crs
  polys
  #just show the attribute column names
  names(polys@data)
  
  #plot polygon subset assuming readOGR pathway
  # plot(polys,
  #      lwd=6,
  #      main="Polygon Subset")
  
  # Load images (ultimately should be a loop, right?)
  im <- stack(im_filename)
  num_layers <- nlayers(im)
  
  #plot raster stack
  # Create an RGB image from the raster stack
  #plotRGB(im, r = 4, g = 3, b = 2, stretch="lin")


#Extraction of pixel values using polygons ----

  # S4 method for Raster,SpatialPolygons
  #small=TRUE allows for extraction of pixels that are larger than the polygon
  #df=TRUE outputs as data frame
  #normalizeWeights=FALSE and weights=TRUE returns a column saying what percent of that pixel is in the poly
  #nl = number of layers in stack from which you want to extract
 #beginCluster(num_nodes) do not use for now
  values_df_all <- extract(im, polys, small=TRUE, 
                           df=TRUE, weights=TRUE, normalizeWeights=FALSE,
                           nl=num_layers)

  #create dummy df that has the extract IDs (1:number of polygons) aligned with
  #the polygon unique IDs
  #first subset the polys shapefile by column, only finding the UID and class name cols
  uid_and_class_data <- polys[,(names(polys)==uid_fieldname | names(polys)==class_fieldname)]
  #this should have one column that is the polygon UID, and one that is class code
  poly_uid_df <- data.frame(uid_and_class_data@data)
  #add the 1:n column to the the linking ID
  num_rows <- data.frame("ID"=1:nrow(poly_uid_df))
  poly_uid_df <- cbind(num_rows, poly_uid_df)

  #this outer join gets the species and crown UID information attached
  #in a one-to-many fashion to the pixel level results of values_df_all
  all_crowns_df<-merge(x=values_df_all,y=poly_uid_df,by="ID",all=TRUE)

#write output to some format ----
  write.csv(all_crowns_df, file=output_filename)


#other stuff that might be useful ----

# max for each polygon
crown_max_vals <- unlist(lapply(v, function(x) if (!is.null(x)) max(x, na.rm=TRUE) else NA ))


