
packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

## Packages for everything
packages(Hmisc)

## Packages for geospatial data handling
packages(raster)
packages(rgeos)
packages(rgdal)
#packages(spatialEco)
packages(rasterVis)


## Packages for Shiny 
packages(shiny)
packages(shinydashboard)
packages(shinyFiles)
packages(snow)
packages(htmltools)
packages(devtools)

## Packages for data table handling
packages(xtable)
packages(DT)
packages(dismo)
packages(stringr)
packages(plyr)
packages(dplyr)

## Packages for graphics and interactive maps
packages(ggplot2)
packages(leaflet)
packages(RColorBrewer)

packages(gfcanalysis)

## Packages for statistical tests
packages(agricolae)
packages(outliers)



