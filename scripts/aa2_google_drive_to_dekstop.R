####################################################################################
####### Object: Google Drive to Local Drive    
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2019/02/07                                   
###################################################################################

####### TIME SERIES DATA ARE GENERATED IN GOOGLE EARTH ENGINE
##      https://code.earthengine.google.com/b758a7ac4629ea95591078800419f33b


###################################################################################
#### Parameters
###################################################################################

#### Root directory

####################################################################################################################
####### LOAD AUTHORIZATION KEY FOR "DRIVE" AND DOWNLOAD RESULTS
####################################################################################################################

#### Select a basename for the archives to transfer
base <- 'median_hul_clip'
setwd(sae_dir)

#### Initialize the DRIVE function, change the authorization key
system(sprintf("echo %s | drive init",
               auth_key))

#### Read list of files in GEDrive that contain base
system(sprintf("drive list -matches %s > %s",
               paste0(base),
               "list_down.txt"))

data_input <- basename(as.character(unlist(read.table("list_down.txt"))))
         
#### download
for(data in data_input){
  system(sprintf("drive pull %s",
                 data))
}

#### Create two destination folders
dir.create(paste0(sae_dir,"landsat/"),recursive = T)
dir.create(paste0(sae_dir,"sentinel/"),recursive = T)

#### Make a subset for LANDSAT and one for SENTINEL
lsat <- data_input[grep(paste0("lsat"),data_input)]
stnl <- data_input[grep(paste0("s2"),data_input)]

lapply(lsat,function(x){file.rename(x,paste0(sae_dir,"landsat/",x))})
lapply(stnl,function(x){file.rename(x,paste0(sae_dir,"sentinel/",x))})
