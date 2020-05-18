###################################################################################
#Purpose: Mosaic any number of images (by geography) using gdal functionality
#
#Created: MGA 5/15/2020
#Updated: MGA 5/15/2020
#
#Inputs: A folder with bunch of images. Need to check the file list that results to make
#sure you're not grabbing other stuff.
#Output: A mosaic in some user-specified image format
#
#Notes:
###################################################################################

library(gdalUtils)
library(raster)
library(rgdal)

# ENTER FOLDER information here for the the images to be mosaicked
image_dir <- "F:/dropbox/Data/DC/skysat/scenes/python_api_download/20190701_183041/analytic/toar/"
setwd(image_dir)
#name your output mosaic
output_filename <- "test_mosaic.tif"
#Find tifs in folder (this code avoids things like ".tif.ovr")
file <- list.files(pattern = '\\.tif$', full.names = TRUE)

# We'll pre-check to make sure there is a valid GDAL install
# and that raster and rgdal are also installed.
# Note this isn't strictly neccessary, as executing the function will
# force a search for a valid GDAL install.
outdir <- image_dir
gdal_setInstallation()
valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
if(require(raster) && require(rgdal) && valid_install)
{
  #mosaic all filenames together. Important that separate=FALSE to avoid making a
  #band for each input image resulting in ginormous and non-functional files.
  mosaic_rasters(gdalfile=file,dst_dataset=file.path(outdir,output_filename),
                 separate=FALSE,of="GTiff",verbose=TRUE)
  #show crs info, etc. for the output file
  gdalinfo(output_filename)
}