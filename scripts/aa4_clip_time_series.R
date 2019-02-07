####################################################################################################
####################################################################################################
## Clip time series to desired boxes: Landsat time series, Sentinel, RapidEye, Spot, NDVI+NDWI trend 
## Contact remi.dannunzio@fao.org
## 2017/09/11 
####################################################################################################
####################################################################################################


##########################################################################################################################################################################
################# Directory and FILE : ADAPT TO YOUR CONFIGURATION
##########################################################################################################################################################################

############################################################

############################################################
#################### SET PARAMETERS

## Setup the number of snippets to generate
how_many <- 10

################ Create the list of ID's to process
list_ids <- pts[!is.na(pts$`pts_lsat$bb`) 
                & !is.na(pts$`pts_stnl$bb`) 
                #& !is.na(pts$`pts_plnt$bb`)
                ,
                point_id]

# ID to process 
listdone <- list() 
listdone <- read.table(text=list.files(dest_dir),as.is=T,fill=T,sep="_")[,2]
listdone <- gsub(".png","",listdone)
listodo  <- list_ids[!(list_ids %in% listdone)]

## Read the datafile and setup the correct names for the variables
head(pts[!is.na(pts$`pts_lsat$bb`) & !is.na(pts$`pts_stnl$bb`) & !is.na(pts$`pts_plnt$bb`)
         ,])

#####################################################################################
#####################################################################################
#####################################################################################

# Loop through all IDs ----------------------------------------------------

to_go <- min(how_many,length(listodo))

for(the_id in listodo[1:to_go]){
  
  print(paste0(to_go," remain to do"))
  to_go <- to_go-1
  ####################################################################
  ################# Open the image output file
  
  ## Check which point is being processed
  (the_pt <- pts[pts[,point_id]==the_id,])
  
  out_name <- paste(dest_dir,"pt_",the_id,"_class",the_pt$map_class,".png",sep="")
  png(file=  out_name,
      width= 400*dim_h_grid,
      height=400*dim_v_grid)
  
  ####################################################################
  ##### Delimitations of the plot in geographic coordinates
  one_poly <- outbox[outbox@data[,point_id]==the_id,]
  in_poly  <-   inbox[inbox@data[,point_id]==the_id,]
  
  margins <- extent(
    one_poly@bbox["x","min"]-1/111321,
    one_poly@bbox["x","max"]+1/111321,
    one_poly@bbox["y","min"]-1/111321,
    one_poly@bbox["y","max"]+1/111321)
  
  ####################################################################
  ##### Delimitations of the plot in UTM coordinates
  one_poly_utm <- outbox_utm[outbox_utm@data[,point_id]==the_id,]
  in_poly_utm  <-   inbox_utm[inbox_utm@data[,point_id]==the_id,]
  
  margins_utm <- extent(
    one_poly_utm@bbox["x","min"]-100,
    one_poly_utm@bbox["x","max"]+100,
    one_poly_utm@bbox["y","min"]-100,
    one_poly_utm@bbox["y","max"]+100)
  
  ###################################################################
  ################# Find the corresponding indexes
  tryCatch({lsat_bbox <- the_pt[,"pts_lsat$bb"]},
           error=function(e){print(paste0("no Landsat image available for the point"))})
  
  tryCatch({stnl_bbox <- the_pt[,"pts_stnl$bb"]},
           error=function(e){print(paste0("no Sentinel image available for the point"))})
  
  tryCatch({plnt_bbox <- the_pt[,"pts_plnt$bb"]},
           error=function(e){print(paste0("no Planet image available for the point"))})
  
  ################# Set the layout
  #dev.off()
  ## The export image will be in a 4 (height) x 5 (width) grid box
  par(mfrow = c(dim_v_grid,dim_h_grid))
  par(mar=c(0,0,0,0))
  
  ndvi_trend <- data.frame(matrix(nrow=0,ncol=2))
  names(ndvi_trend) <- c("year","mean")
  
  ndwi_trend <- data.frame(matrix(nrow=0,ncol=2))
  names(ndwi_trend) <- c("year","mean")
  
  i <- 1
  ## year <- "2013"
  ####################################################################
  ################# Clip the landsat time series
  for(year in c(yr_str_lsat:yr_end_lsat)){
    print(year)
    
    plot(margins,axes=F,xlab="",ylab="")
    tryCatch({
      lsat <- brick(paste(lsat_dir,lsat_basename,year,"_",lsat_bbox,".tif",sep=""))
      lsat_clip<-crop(lsat,one_poly)
      
      swir <- raster(lsat_clip,4)
      nir  <- raster(lsat_clip,3)
      red  <- raster(lsat_clip,2)
      green<- raster(lsat_clip,1)
      ndvi <- (nir-red)/(nir+red)
      ndwi <- (nir-swir)/(nir+swir)
      
      ndvi_trend[i,]$year <- year 
      ndvi_trend[i,]$mean <- cellStats(crop(ndvi,in_poly),stat='mean')
      
      ndwi_trend[i,]$year <- year 
      ndwi_trend[i,]$mean <- cellStats(crop(ndwi,in_poly),stat='mean')
      
      i <- i + 1
      
      #Plot natural colours composite (NIR-RED-GREEN == 4-3-2 in L7 nomenclature)
      stack <- stack(nir,red,green)
      plotRGB(stack,stretch="hist",add=T)
      #plot(ndvi,add=T)
    },error=function(e){print(paste0("no image available in ",year," for ",lsat_bbox))})
    
    lines(in_poly,col="yellow",lwd=2)
    rect(
      xleft =   margins@xmin, 
      ybottom = margins@ymax - outside_box_size/10/111320, 
      xright =  margins@xmin + outside_box_size/1.9/111320, 
      ytop =    margins@ymax, 
      col = "white", 
      border = NA)
    
    title(main=paste("Landsat ",year,sep=""),font.main=2,cex.main=2,line=-3,adj=0.05)
    
  }
  
  ####################################################################
  ################# Clip the sentinel tile 
  for(year in c(yr_str_stnl:yr_end_stnl)){
    plot(margins,axes=F,xlab="",ylab="")
    print(year)
    the_pt
    tryCatch({
      stnl <- brick(paste(stnl_dir,stnl_basename,year,"_",stnl_bbox,".tif",sep=""))
      stnl_clip<-crop(stnl,one_poly)
      
      blu <- raster(stnl_clip,1)
      grn <- raster(stnl_clip,2)
      red <- raster(stnl_clip,3)
      nir <- raster(stnl_clip,4)
      
      ndvi <- (nir-red)/(nir+red)
      #ndwi <- (nir-swir)/(nir+swir)
      
      ndvi_trend[i,]$year <- year 
      ndvi_trend[i,]$mean <- cellStats(crop(ndvi,in_poly),stat='mean')
      
      #ndwi_trend[i,]$year <- year 
      #ndwi_trend[i,]$mean <- cellStats(crop(ndwi,in_poly),stat='mean')
      i <- i + 1
      
      stackNat <- stack(red,grn,blu)
      #stackVeg <- stack(nir,ndvi,grn)
      stackNIR <- stack(nir,red,grn)
      
      plotRGB(stackNIR,stretch="hist",add=T)
      
      
    },error=function(e){print(paste0("no image available in ",year," for sentinel"))})
    lines(in_poly,col="yellow",lwd=2)
    
    rect(
      xleft =   margins@xmin, 
      ybottom = margins@ymax-100/111320, 
      xright =  margins@xmin+500/111320, 
      ytop =    margins@ymax, 
      col = "white", 
      border = NA)
    
    title(main=paste0("Sentinel ",year),font.main=2,cex.main=2,line=-3,adj=0.05)
  }
  
  ####################################################################
  ################# Clip the PLANET tile 
  
  for(year in c(2018)){
    plot(margins_utm,axes=F,xlab="",ylab="")
    print(year)
    the_pt
    tryCatch({
      plnt <- brick(paste(plnt_dir,plnt_bbox,sep=""))
      plnt_clip<-crop(plnt,one_poly_utm)
      
      blu <- raster(plnt_clip,1)
      grn <- raster(plnt_clip,2)
      red <- raster(plnt_clip,3)
      nir <- raster(plnt_clip,4)
      
      ndvi <- (nir-red)/(nir+red)
      
      ndvi_trend[i,]$year <- year 
      ndvi_trend[i,]$mean <- cellStats(crop(ndvi,in_poly_utm),stat='mean')
      i <- i + 1
      
      #stackNat <- stack(red,grn,blu)
      #stackVeg <- stack(nir,ndvi,grn)
      stackNIR <- stack(nir,red,grn)
      
      plotRGB(stackNIR,stretch="hist",add=T)
      
      
    },error=function(e){cat("Configuration impossible \n")})
    lines(in_poly_utm,col="red",lwd=2)
    #plot(in_poly,add=T,col="red")
    
    title(main=paste0("planet_",year),font.main=200)
  }
  
  ####################################################################
  ################# NDVI graph
  par(mar=c(2,2,2,2))
  tryCatch({
    plot(ndvi_trend,
         # yaxt='n',
         # xaxt='n',
         xlab="year",
         ylab="",
         ylim=c(0,1)
    )
    lines(ndvi_trend, pch=16,col="blue")
    
    title(main="Annual mean ndvi",font.main=2,cex.main=2)
  },error=function(e){print(paste0("problem with NDVI"))})
  
  ####################################################################
  ################# NDWI graph
  # par(mar=c(2,2,2,2))
  # tryCatch({
  #   plot(ndwi_trend,
  #        # yaxt='n',
  #        # xaxt='n',
  #        xlab="year",
  #        ylab="",
  #        ylim=c(0,1)
  #   )
  #   lines(ndwi_trend, pch=16,col="blue")
  #   
  #   title(main="Annual mean ndwi",font.main=2,cex.main=2)
  # },error=function(e){print(paste0("problem with NDwI"))})
  ####################################################################
  ### Close the image file
  dev.off()
  
  
  ####################################################################
  ### End the points loop
}

the_pt
