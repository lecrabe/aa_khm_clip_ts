####################################################################################################
####################################################################################################
## Configure the AA scripts
## Contact remi.dannunzio@fao.org
## 2019/02/07
####################################################################################################
####################################################################################################

the_map    <- ""
sae_dir    <- "~/aa_khm_clip_ts/"
setwd(sae_dir)
sae_dir <- paste0(getwd(),"/")
point_file <- list.files(sae_dir,glob2rx("pts_*.csv"))

#### Name of the directory where your Landsat data is
lsat_dir <- paste0(sae_dir,"landsat/")

#### Name of the directory where your Sentinel data is
stnl_dir <- paste0(sae_dir,"sentinel/")

#### Name of the directory where your Rapideye data is
plnt_dir <- paste0(sae_dir,"planet/")

#### Name of the directory where your data will be stored in output
dest_dir <- paste0(sae_dir,"clip_time_series/")

#### NAME MUST IN FORMAT paste0(lsat_basename,"YYYY_bbx.tif")
lsat_basename <- "median_hul_clip_lsat_"
stnl_basename <- "median_hul_clip_s2_"

## The export image will be in a 1 (height) x 6 (width) grid box
dim_v_grid <- 1
dim_h_grid <- 6

## setup year start and end for landsat 
yr_str_lsat <- 2014
yr_end_lsat <- 2015

## setup year start and end for sentinel
yr_str_stnl <- 2016
yr_end_stnl <- 2017

## setup the visualisation parameters for the interpretation box size. in meters
interpretation_box_size <- 30

## setup the visualisation parameters for the level of zoom. in meters
outside_box_size        <- 1500

## position in landsat archive name of the "bounding box". Example: "median_hul_clip_lsat_1995_" == 27
bb_pos_lsat <- nchar(lsat_basename)+6

## position in sentinel archive name of the "bounding box". Example: "median_hul_clip_s2_1995_" == 25
bb_pos_stnl <- nchar(stnl_basename)+6
