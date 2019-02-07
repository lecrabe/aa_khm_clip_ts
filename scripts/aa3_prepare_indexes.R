####################################################################################################
####################################################################################################
## Clip time series to desired boxes: Landsat time series, Sentinel, RapidEye, Spot, NDVI+NDWI trend 
## Contact remi.dannunzio@fao.org
## 2017/09/11 
####################################################################################################
####################################################################################################

# Options -----------------------------------------------------------------


options(stringsAsFactors=FALSE)

library(Hmisc)
library(sp)
library(rgdal)
library(raster)
library(plyr)
library(foreign)
library(dplyr)
library(rgeos)

##########################################################################################################################################################################
################# Directory and FILE : ADAPT TO YOUR CONFIGURATION
##########################################################################################################################################################################

############################################################

############################################################
#################### SET PARAMETERS


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


## position in landsat archive name of the "bounding box". Example: "median_hul_clip_lsat_1995_" == 27
bb_pos_lsat <- nchar(lsat_basename)+6

## position in sentinel archive name of the "bounding box". Example: "median_hul_clip_s2_1995_" == 25
bb_pos_stnl <- nchar(stnl_basename)+6

## Read the datafile and setup the correct names for the variables
pts <- read.csv(paste0(sae_dir,point_file))  #####  CHANGE TO MY VALUE HERE

head(pts)
names(pts)

## setup the correct names for the variables
map_code <- "map_class"
point_id <- "id"
xcoord   <- "XCoordinate"
ycoord   <- "YCoordinate"









##########################################################################################################################################################################
################## SCRIPT AUTOMATICALLY RUNS FROM HERE
##########################################################################################################################################################################
dir.create(dest_dir,showWarnings = F)
#proj_utm <- proj4string(raster(paste0(rpdy_dir,list.files(rpdy_dir,pattern=glob2rx("*.tif"))[1])))
dev.off()

################# Create spatial point file 
pt_df_geo <- SpatialPointsDataFrame(
  coords = pts[,c(xcoord,ycoord)],
  data   = data.frame(pts[,c(point_id,map_code)]),
  proj4string=CRS("+init=epsg:4326")
)

################# Create spatial point file in UTM
proj_utm <- proj4string(raster(paste0(plnt_dir,list.files(plnt_dir,pattern=glob2rx("*.tif"))[1])))

pt_df_utm <- spTransform(pt_df_geo,proj_utm)

################ Create the index of the Landsat tiles
list_lsat <- list.files(lsat_dir,pattern=paste0(yr_str_lsat))
lp <- list()

for(file in list_lsat){
  raster <- raster(paste(lsat_dir,file,sep=""))
  
  e<-extent(raster)
  
  poly <- Polygons(list(Polygon(cbind(
    c(e@xmin,e@xmin,e@xmax,e@xmax,e@xmin),
    c(e@ymin,e@ymax,e@ymax,e@ymin,e@ymin))
  )),file)
  lp <- append(lp,list(poly))
}

## Transform the list into a SPDF PRIMER ERROR
lsat_idx <-SpatialPolygonsDataFrame(
  SpatialPolygons(lp,1:length(lp)), 
  data.frame(list_lsat), 
  match.ID = F
)

head(lsat_idx)
names(lsat_idx@data) <- "bb"
lsat_idx@data$bb <- substr(lsat_idx@data$bb,bb_pos_lsat,(nchar(lsat_idx@data$bb)-4))
head(lsat_idx@data)
#plot(lsat_idx)

################ Create the index of the Sentinel tiles
list_s2 <- list.files(stnl_dir,pattern=paste0("s2_"))
lp <- list()

for(file in list_s2){
  raster <- raster(paste(stnl_dir,file,sep=""))
  
  e<-extent(raster)
  
  poly <- Polygons(list(Polygon(cbind(
    c(e@xmin,e@xmin,e@xmax,e@xmax,e@xmin),
    c(e@ymin,e@ymax,e@ymax,e@ymin,e@ymin))
  )),file)
  lp <- append(lp,list(poly))
}

## Transform the list into a SPDF
stnl_idx <-SpatialPolygonsDataFrame(
  SpatialPolygons(lp,1:length(lp)), 
  data.frame(list_s2), 
  match.ID = F
)

names(stnl_idx@data) <- "bb"
stnl_idx@data$bb <- substr(stnl_idx@data$bb,bb_pos_stnl,(nchar(stnl_idx@data$bb)-4))
head(stnl_idx@data)
#plot(stnl_idx,add=T)

################# Project both into Lat-Lon EPSG:4326
proj4string(pt_df_geo) <- CRS("+init=epsg:4326")
proj4string(lsat_idx)  <- CRS("+init=epsg:4326")
proj4string(stnl_idx)  <- CRS("+init=epsg:4326")



################ Create the index of the Planet tiles
list_plnt <- list.files(plnt_dir,pattern=".tif")
lp<-list()
file <- list_plnt[1]

tmpdir <- paste0(sae_dir,"tmp/")
dir.create(tmpdir,showWarnings = F)

system(sprintf("rm %s",
               paste0(plnt_dir,"index_planet.shp")))

for(file in list_plnt){

system(sprintf("gdalwarp -tr 100 100 -co COMPRESS=LZW %s %s",
               paste0(plnt_dir,file),
               paste0(tmpdir,"tmp.tif")
               ))

system(sprintf("gdal_calc.py -A %s --co=\"COMPRESS=LZW\" --outfile=%s --overwrite --calc=\"%s\"",
               paste0(tmpdir,"tmp.tif"),
               paste0(tmpdir,"mask.tif"),
               "A>0"
))

system(sprintf("gdal_polygonize.py  -f \"ESRI Shapefile\" %s %s",
               paste0(tmpdir,"mask.tif"),
               paste0(tmpdir,"mask.shp")
))

dbf <- read.dbf(paste0(tmpdir,"mask.dbf"))
dbf$bb <- file
write.dbf(dbf,paste0(tmpdir,"mask.dbf"))

system(sprintf("ogr2ogr -append -update -where \"DN=1\" %s %s",
               paste0(plnt_dir,"index_planet.shp"),
               paste0(tmpdir,"mask.shp")
))

system(sprintf("rm %s",
               paste0(tmpdir,"/*.*")))

}

plnt_idx <- readOGR(paste0(plnt_dir,"index_planet.shp"))
#plot(plnt_idx)

# for(file in list_plnt){
#   raster <- raster(paste(plnt_dir,file,sep=""))
# 
#   e <- extent(raster)
#   
#   poly <- Polygons(list(Polygon(cbind(
#     c(e@xmin,e@xmin,e@xmax,e@xmax,e@xmin),
#     c(e@ymin,e@ymax,e@ymax,e@ymin,e@ymin))
#   )),file)
#   lp <- append(lp,list(poly))
# }
# 
# ## Transform the list into a SPDF
# plnt_idx <-SpatialPolygonsDataFrame(
#   SpatialPolygons(lp,1:length(lp)), 
#   data.frame(list_plnt), 
#   match.ID = F
# )
# names(plnt_idx@data) <- "bb"

proj4string(plnt_idx) <- proj_utm

################# Intersect points with index of imagery and append ID's of imagery to data.frame
pts_lsat <- over(pt_df_geo,lsat_idx)
pts_stnl <- over(pt_df_geo,stnl_idx)
pts_plnt <- over(pt_df_utm,plnt_idx)

pts<-cbind(pts,pts_lsat$bb)
pts<-cbind(pts,pts_stnl$bb)
pts<-cbind(pts,pts_plnt$bb)

################# Create the outside boundaries box (1km // twice 500m from center of box)
lp<-list()
ysize <- outside_box_size/111321/2

## Loop through all points
for(i in 1:nrow(pts)){
  ymin <- pts[i,ycoord]-ysize
  ymax <- pts[i,ycoord]+ysize
  xmin <- pts[i,xcoord]-ysize*cos(pts[1,ycoord]*pi/180)
  xmax <- pts[i,xcoord]+ysize*cos(pts[1,ycoord]*pi/180)
  
  p  <- Polygon(cbind(c(xmin,xmin,xmax,xmax,xmin),c(ymin,ymax,ymax,ymin,ymin)))
  ps <- Polygons(list(p), pts[i,point_id])
  lp <- append(lp,list(ps))
}

## Transform the list into a SPDF
outbox<-SpatialPolygonsDataFrame(
  SpatialPolygons(lp,1:nrow(pts)), 
  pts[,c(map_code,point_id,xcoord,ycoord)], 
  match.ID = F
)

proj4string(outbox) <- CRS("+init=epsg:4326")

## Project into UTM
outbox_utm <- spTransform(outbox,proj_utm)

################# Create the 0.5 ha box (70/2 = 35m shift from center)
lp<-list()
ysize <- interpretation_box_size/111321/2

## Loop through all points
for(i in 1:nrow(pts)){
  ymin <- pts[i,ycoord]-ysize
  ymax <- pts[i,ycoord]+ysize
  xmin <- pts[i,xcoord]-ysize*cos(pts[1,ycoord]*pi/180)
  xmax <- pts[i,xcoord]+ysize*cos(pts[1,ycoord]*pi/180)
  
  p  <- Polygon(cbind(c(xmin,xmin,xmax,xmax,xmin),c(ymin,ymax,ymax,ymin,ymin)))
  ps <- Polygons(list(p), pts[i,1])
  lp <- append(lp,list(ps))
}

## Transform the list into a SPDF
inbox<-SpatialPolygonsDataFrame(
  SpatialPolygons(lp,1:nrow(pts)), 
  pts[,c(map_code,point_id,xcoord,ycoord)], 
  match.ID = F
)

proj4string(inbox) <- CRS("+init=epsg:4326")
proj4string(inbox) <- proj4string(outbox) <- CRS("+init=epsg:4326")

## Project into UTM
inbox_utm <- spTransform(inbox,proj_utm)



head(pts[!is.na(pts$`pts_lsat$bb`) & !is.na(pts$`pts_stnl$bb`) & !is.na(pts$`pts_plnt$bb`),])
