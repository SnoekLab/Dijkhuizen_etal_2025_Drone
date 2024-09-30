###### This script shows how to extract the plot data (rgb images, height etc) obtained by drones 
###### from the GeoTiff files.
###### This script also processes the serriola data which is not used in the paper

### packages needed
library(shapefiles)
library(ggplot2)
library(openxlsx)
library(rgdal)
library(imager)
library(spdep)
library(raster)
library(dplyr)
library("viridis")   
library(grid)
library(gridExtra)
library(cowplot)
# library(rotate)


# First, we load in an object with metada on the plots. Most importantly this frame contains the plot identifier,
# (starts with "IL" followed by a number), the species growing on the plot (Serriola or Sativa), the type of
# lettuce if the species is Sativa, the accession (LK followed by number) the replicate number (two 
# replicates per accession), and the geographic location of each plot. The latter variable is indicated by
# specifying the latitude and longitude of the four corners of the plot in decimal degrees. corner1.X represents
# the longitude of one of the corners of the plot and corner1.Y the latitude of the same corner.
load(file="obj_metawco.out")
metawco

# If you wish to obtain the coordinates from the metawco frame, use as.character() because otherwise the 
# coordinates will be rounded. 

### To get the geotiff files we will work with, we need to connect the internal coordinate system of the raw 
### data to the geographic coordinate system. This process is called georeferencing. To do the georeferencing
### we used software called QGIS. This is therefore not shown in this script. For georeferencing we need to 
### use several points with known coordinates. We used the below points.

# most left IL677                 470  "6.06971790441074" "51.3779043047911"  w LK182
# lowest IL095                    147  "6.06982203611198" "51.3778110304933"    LK295
# row 1 start (in middle) IL796   767  "6.07090395127319" "51.3783150456096"  w LK077
# end row 9 IL001                  66  "6.07251288710945" "51.3787006580359"    LK201
# top IL755                       726  "6.07267242570111" "51.3789000869318"    LK119
# most right IL403                228  "6.07274829488094" "51.3787970568035"  w LK003

# The code below will plot the corners one of the plots used for georeferencing in decimal degrees. Notice
# how they are not perpendicular to the latitude and longitude axes.
tmp <- metawco[metawco$Plot.ID %in% c("IL677","IL095","IL796","IL001","IL755","IL403"),c("Plot.ID","Accession","corner1.X","corner1.Y","corner2.X","corner2.Y","corner3.X","corner3.Y","corner4.X","corner4.Y")]
pnt.no <- 3
plot(as.numeric(tmp[pnt.no,c(3,5,7,9)]),as.numeric(tmp[pnt.no,c(3,5,7,9)+1]),main=tmp[pnt.no,1])
cbind(tmp[,1],as.character(tmp[,2]),as.character(tmp[,3]))


# Below we load in the GEOTiff files we obtained after georeferencing. Obviously you would need to change the
# file location to wherever its stored in your computer. We start with the files from the 11th of june 2021.

## This file contains the rgb values.
use.file.rgb <- "C:/Users/Gebruiker/Documents/LettuceKnow/Rproject/Tiff_files_11/246f746c-f8e2-4bcf-beb9-6051bc535812_RGB_modified_ref_points.tif"

## File contains Digital Surface Model (DSM) of the plots ,i.e. a 3D computer representation from which the
# height of the plants can be reconstructed.
use.file.dsm <- "C:/Users/Gebruiker/Documents/LettuceKnow/Rproject/Tiff_files_11/246f746c-f8e2-4bcf-beb9-6051bc535812_DSM_modified.tif"

## This file contains the multi-spectral data. There are five channels corresponding to different wave-lenghts,
# MPS1 ~ 475 nm blue, MSP2 ~ 560 nm green, MSP3 ~ 668 nm red, MSP4 ~ 718 nm red-edge, MSP5 ~ 842 nm Near-Infra Red
# (NIR)
use.file.msp <- "C:/Users/Gebruiker/Documents/LettuceKnow/Rproject/Tiff_files_11/cebe7014-1426-4a5a-a78f-14d4559875c4_MSP_modified.tif"

### Get some basics info on structure and dimensionality of the files.
GDALinfo(use.file.rgb)
GDALinfo(use.file.dsm)
GDALinfo(use.file.msp)

# The below file will contain the coordinates of the points used for georeferencing.

dus <- readOGR("C:/Users/Gebruiker/Documents/LettuceKnow/Rproject/Tiff_files_11/11_GCP.shp")
apply(dus@coords,2,as.character)

## Third coordinate is the height.
# coords.x1          coords.x2          coords.x3         
# [1,] "6.06981245542049" "51.3777942095375" "71.9974507630497"
# [2,] "6.07041035445823" "51.3779929301431" "72.077231107901" 
# [3,] "6.07160717601852" "51.3784831876998" "71.9366088757917"
# [4,] "6.07278800383705" "51.3787820033747" "72.0030368749678"
# [5,] "6.07268669946156" "51.3789305266429" "71.8082819345102"
# [6,] "6.07206601074224" "51.3787241469641" "71.8440759299949"
# [7,] "6.0705171445343"  "51.3781558752918" "71.9124734007448"
# [8,] "6.0696944997994"  "51.3779411174735" "71.8211082840964"

plot(dus@coords[,1:2])

### Raster images 

# Using the stack function we create an S4 object with the pixels of the images denoted by rows and columns.
# The resulting "stack" consists of the layers corresponding to the red, green and blue values. These range
# between 0 and 255.
rastim.rgb <-  raster::stack(use.file.rgb)
names(rastim.rgb) <- c("red","green","blue")
raster::plotRGB(rastim.rgb)

# Raster objects can be indexed in a familiar way. rastim.rgb$red[1:10,1:10] will give the red intensity
# values for the 100 pixels that constitute the top-left part of the image. Similarly, rastim.rgb[1:100] will
# give the red green and blue values of the first 100 pixels going from left to right, top to bottom. 
# As is obvious from the above image, many pixels are not on the plot and thus white. These all have NA values.

# Create a stack of the DSM data with one layer (corresponding to height). 
rastim.dsm <-  raster::stack(use.file.dsm)
breaks = seq(71.5, 73, by = 0.1)
names(rastim.dsm) <-c("height")
plot(rastim.dsm,col=c(rainbow(36)),breaks=breaks)

# Finally, create stack of the MSP data with five layers for each wavelength.
rastim.msp <-  raster::stack(use.file.msp)
names(rastim.msp) <-c("blue","green","red","red_edge","NIR")
plot(rastim.msp,col=magma(256))

### Next we want to extract the plots from the stacks we created. We want to subset the plots based on their
# species ID and replicate number. In total there are 788 plots
dim(metawco) # 788
set1 <- which(metawco$Species == "sativa" & metawco$rep == 1) ; length(set1)
set2 <- which(metawco$Species == "serriola" & metawco$rep == 1) ; length(set2)
set3 <- which(metawco$Species == "sativa" & metawco$rep == 2) ; length(set3)
set4 <- which(metawco$Species == "serriola" & metawco$rep == 2) ; length(set4)

# Now we create a loop where for each plot in one of the sets define above we extract the plot data from the
# stacks we made earlier. we will store the result in the all.pl dataframe.
all.pl <- NULL

# For each plot in the set we:
for ( i in 1:length(set4)){
  # Extract relevant variables, most importantly the coordinates of the corners of the plot.
  use.num <- set4[i]
  use.plotid <- metawco$Plot.ID[use.num]
  use.lk <- metawco$Accession[use.num]
  cornerx <- c(metawco$corner1.X[use.num],metawco$corner2.X[use.num],metawco$corner3.X[use.num],metawco$corner4.X[use.num])
  cornery <- c(metawco$corner1.Y[use.num],metawco$corner2.Y[use.num],metawco$corner3.Y[use.num],metawco$corner4.Y[use.num])  
  use.coord <- as.matrix(cbind(cornerx,cornery))
  
  # We will use the coordinates of the corners of the plot to define a polygon between these points. These
  # will be rectangles. Once we have a object of class SpatialPolygons, we can extract all the data within
  # the vertices of the rectangle from our stacks. 
  
  pol1 <- Polygon(use.coord)
  pol2 <- Polygons(list(pol1),ID="A")
  b2 <- SpatialPolygons(list(pol2))
  
  # With the extract function we can extract all the cell numbers of the stack that are located inside of the
  # polygon. We now extract them from the rgb stack. sp.val is now a list containing a matrix with these 
  # cell numbers and the corresponding red green and blue values.
  sp.val <- extract(rastim.rgb,b2,along=F,cellnumbers=T)
  
  # The function xyFromCell gets for each of these cell numbers the geographic coordinates of the center.
  lonlat <- xyFromCell(rastim.rgb,sp.val[[1]][,1])
  
  # Define matrix with cell numbers and red green blue values
  coldat <- sp.val[[1]]
  colnames(coldat) <- c("cellnum","red","green","blue")
  
  # Paste to all coordinates, cell number, red green blue values en metadata together.
  to.pl <- data.frame(lonlat,coldat,use.plotid,use.lk)
  
  ## extract height data from the DSM stack. We use the geographic coordinates we determined earlier by 
  # means of the polygon. We than add the height data to the to.pl dataframe.
  esth <- NULL
  esth  <- extract(rastim.dsm,to.pl[,c("x","y")],method=c("simple","bilinear")[1])
  colnames(esth) <- "estheight"
  to.pl <- data.frame(to.pl,esth)
  
  ## We similarly extract MSP data and add it to to.pl
  msp <- NULL
  msp  <- extract(rastim.msp,to.pl[,c("x","y")],method=c("simple","bilinear")[1])
  colnames(msp) <- c("blue", "green","red","red_edge","NIR")
  to.pl <- data.frame(to.pl,msp)
  
  ## combine
  all.pl <- rbind(all.pl,to.pl)
}

all.pl[1:5,]
# save(all.pl,file = "obj_set_1.out")
# save(all.pl,file = "obj_set_2.out")
# save(all.pl,file = "obj_set_3.out")
# save(all.pl,file = "obj_set_4.out")

# Let's plot the rgb, height and multispectral data for an example plot.

example_plot <- all.pl[all.pl$use.plotid=="IL269",]

# Using the rgb function we can obtain we can obtain a hexadecimal representation of the red green and blue
# intensities in each cell. Each hex code corresponds to a color which we can feed to ggplot for plotting.
use.rgb <- rgb(example_plot$red/255,example_plot$green/255,example_plot$blue/255)

rgb_plot <- ggplot(example_plot,aes(x,y))+
  geom_tile(fill=use.rgb)+
  ggtitle(paste(unique(example_plot$use.lk),"RGB image"))+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")

rgb_plot

# We can similarly plot the height data.

height_plot <- ggplot(example_plot,aes(x,y))+
    geom_tile(aes(fill=(estheight)))+
    scale_fill_gradientn(colors = magma(256),name = "Height")+
    ggtitle(paste(unique(example_plot$use.lk),"DSM data")) +
    theme_void()+
    theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25)) +
    xlab("Longitude")+
    ylab("Latitude")

height_plot

# And, finally, let's try to plot the multi-spectral data. We can use the rgb()  function again,
# but the picture reconstructed from the multi-spectral data will not quite be the same as from the rgb
# camera. Don't stress, that's normal. The best way to plot rgb picture is to normalize each wavelength 
# with its max value.

# use.rgb <- rgb(to.pl$msp3,to.pl$msp2,to.pl$msp1,maxColorValue = 0.16)
use.rgb <- rgb(example_plot$red.1/max(example_plot$red.1),example_plot$green.1/max(example_plot$green.1),example_plot$blue.1/max(example_plot$blue.1))

rgb_MS <- ggplot(example_plot,aes(x,y))+
  geom_tile(fill=use.rgb)+
  ggtitle(paste(unique(example_plot$use.lk),"RGB MS")) + 
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25)) +
  xlab("Longitude")+
  ylab("Latitude")

rgb_MS

# Of course, we can also plot the other two wavelengths, although we'll need to use an artifical color scale.

red_edge_plot <- ggplot(example_plot,aes(x,y))+
  geom_tile(aes(fill=(red_edge)))+
  scale_fill_gradientn(colors = magma(256),name = "Red-edge")+
  ggtitle(paste(unique(example_plot$use.lk), "Red-edge")) +
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25)) +
  xlab("Longitude")+
  ylab("Latitude")

red_edge_plot

NIR_plot <- ggplot(example_plot,aes(x,y))+
  geom_tile(aes(fill=(NIR)))+
  scale_fill_gradientn(colors = magma(256),name = "NIR")+
  ggtitle(paste(unique(example_plot$use.lk), "NIR")) +
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25)) +
  xlab("Longitude")+
  ylab("Latitude")

NIR_plot

### Next we will repeat this procedure from the data from the 25th of june 2021. We have additional types of data
### at our disposal, as you'll see below.


## We start by defining the directory for the RGB values (Blue: 475 nm; Green: 560 nm; Red: 668 nm).
use.file.rgb <- "C:/Users/Gebruiker/Documents/LettuceKnow/Rproject/Tiff_files_25/8257ada7-0290-4537-bd88-dbc846d1dfad_RGB_modified_ref_points.tif"

## DSM map with height data
use.file.dsm <- "C:/Users/Gebruiker/Documents/LettuceKnow/Rproject/Tiff_files_25/8257ada7-0290-4537-bd88-dbc846d1dfad_DSM_modified.tif"

## MSP (Blue: 475 nm; Green: 560 nm; Red: 668 nm; Red-edge 717 nm; Near-IR: 842 nm)
use.file.msp <- "C:/Users/Gebruiker/Documents/LettuceKnow/Rproject/Tiff_files_25/f718ec74-1061-4678-a6d7-8d035a1ed6d2_MSP_modified.tif"

## cired (Red Edge Chlorophyll Index). This is an estimator of chlorophyll content in plants and is calculated
## as a ratio of the reflectance of NIR over the reflectance of red edge wavelengths - 1. The red-edge band 
## is sensitive to variations in chlorophyll content, and the chlorophyll red-edge index is approximately linearly
## related to chlorophyll content. 
use.file.cired <- "C:/Users/Gebruiker/Documents/LettuceKnow/Rproject/Tiff_files_25/f718ec74-1061-4678-a6d7-8d035a1ed6d2_cired_modified.tif"

## NDRE (Normalized Difference Red Edge). Another measure of chlorophyll content. Can be calculated as
## NDRE = (NIR – red-edge)/(NIR + red-edge). NDRE is particularly useful for mid to late season plants, unlike 
## measures like NDVI because red-edge better penetrates the canopy. NDVI will lose sensitivity after a 
## certain measure of leaf cover and chlorophyll content. 
use.file.ndre <- "C:/Users/Gebruiker/Documents/LettuceKnow/Rproject/Tiff_files_25/f718ec74-1061-4678-a6d7-8d035a1ed6d2_ndre_modified.tif"

## NDVI (Normalized Difference Vegetation Index). Can be calculated as (NIR- Red)/(NIR + Red). Ranges between
## -1 and 1. As visible light is used for photosynthesis and absorbed by plants, while NIR is not, high values
## for this index indicate more vegetation. 
use.file.ndvi <- "C:/Users/Gebruiker/Documents/LettuceKnow/Rproject/Tiff_files_25/f718ec74-1061-4678-a6d7-8d035a1ed6d2_ndvi_modified.tif"

## WDVI (Weighted Difference Vegetation Index). This index can be calculated as NIR - a * RED. a is a parameter
## that corrects for the reflectance of the soil, in which we are not interested, and can be calculated as
## red soil / NIR soil. WDVI is considered a good estimator of the Leaf Area Index (LAI).
use.file.wdvi <- "C:/Users/Gebruiker/Documents/LettuceKnow/Rproject/Tiff_files_25/f718ec74-1061-4678-a6d7-8d035a1ed6d2_wdvi_modified.tif"

## shaped data ref points

dus@data
dus <- readOGR("C:/Users/Gebruiker/Documents/LettuceKnow/Rproject/Tiff_files_25/20210625 Bejo Maasbree GCPs.shp")
apply(dus@coords,2,as.character)
# "38" "39" "20" "36" "25" "28" "34" "35"
# coords.x1          coords.x2          coords.x3         
# [1,] "6.06981692612535" "51.3777990253452" "72.0668868577734"
# [2,] "6.07043489632286" "51.378058492278"  "71.9258247331411"
# [3,] "6.0709501425765"  "51.3781769463004" "72.1160700429857"
# [4,] "6.07281837879934" "51.3787940320537" "71.8681356587142"
# [5,] "6.07270104102227" "51.3789320285525" "71.8193361895785"
# [6,] "6.07212914979723" "51.3787095215645" "71.8582537552968"
# [7,] "6.07153301810372" "51.3785490733449" "71.8651860548809"
# [8,] "6.06968136085033" "51.3779384443302" "71.8050446991503"

plot(dus@coords[,1:2])
text(dus@coords[,1:2],labels= 1:8)

### Information on GeoTiff files.
GDALinfo(use.file.rgb)
GDALinfo(use.file.dsm)
GDALinfo(use.file.msp)
GDALinfo(use.file.cired)
GDALinfo(use.file.ndre)
GDALinfo(use.file.ndvi)
GDALinfo(use.file.wdvi)

### Create stacks and plot each of the files.
rastim.rgb <-  raster::stack(use.file.rgb)
names(rastim.rgb) <- c("red", "green", "blue")
plotRGB(rastim.rgb)
rastim.dsm <-  raster::stack(use.file.dsm)
names(rastim.dsm) <- "height"
plot(rastim.dsm,col=c(grey(1:36/36),rainbow(36)))
rastim.msp <-  raster::stack(use.file.msp)
names(rastim.msp) <-c("blue","green","red","red_edge","NIR")
plot(rastim.msp,col=magma(256))
rastim.cired <-  raster::stack(use.file.cired)
names(rastim.cired) <- near_edge_chlorophyll
plot(rastim.cired,col=magma(256))
rastim.ndre <-  raster::stack(use.file.ndre)
names(rastim.ndre) <- "normalized_difference_red_edge"
plot(rastim.ndre,col=magma(256))
rastim.ndvi <-  raster::stack(use.file.ndvi)
names(rastim.ndvi) <- "normalized_difference_vegetation_index"
plot(rastim.ndvi,col=magma(256))
rastim.wdvi <-  raster::stack(use.file.wdvi)
names(rastim.wdvi) <- "weighted_difference_vegetation_index"
plot(rastim.wdvi,col=magma(256))


### Loop again, one loop per set of plots.
set1 <- which(metawco$Species == "sativa" & metawco$rep == 1) ; length(set1)
set2 <- which(metawco$Species == "serriola" & metawco$rep == 1) ; length(set2)
set3 <- which(metawco$Species == "sativa" & metawco$rep == 2) ; length(set3)
set4 <- which(metawco$Species == "serriola" & metawco$rep == 2) ; length(set4)

all.pl <- NULL

for(i in 1:length(set4)){ ## 
  use.num <- set4[i]
  use.plotid <- metawco$Plot.ID[use.num]
  use.lk <- metawco$Accession[use.num]
  cornerx <- c(metawco$corner1.X[use.num],metawco$corner2.X[use.num],metawco$corner3.X[use.num],metawco$corner4.X[use.num])
  cornery <- c(metawco$corner1.Y[use.num],metawco$corner2.Y[use.num],metawco$corner3.Y[use.num],metawco$corner4.Y[use.num])  
  use.coord <- as.matrix(cbind(cornerx,cornery))
  pol1 <- Polygon(use.coord)
  pol2 <- Polygons(list(pol1),ID="A")
  b2 <- SpatialPolygons(list(pol2))
  
  sp.val <- extract(rastim.rgb,b2,along=F,cellnumbers=T)
  lonlat <- xyFromCell(rastim.rgb,sp.val[[1]][,1])
  coldat <- sp.val[[1]]
  colnames(coldat) <- c("cellnum","red","green","blue")
  to.pl <- data.frame(lonlat,coldat,use.plotid,use.lk)
  
  ## extract data from DSM / height
  esth <- NULL
  esth  <- extract(rastim.dsm,to.pl[,c("x","y")],method=c("simple","bilinear")[1])
  colnames(esth) <- "estheight"
  to.pl <- data.frame(to.pl,esth)

  ## extract data from MSP
  msp <- NULL
  msp  <- extract(rastim.msp,to.pl[,c("x","y")],method=c("simple","bilinear")[1])
    colnames(msp) <- c("blue", "green","red","red_edge","NIR")
  to.pl <- data.frame(to.pl,msp)


  ## cired
  cired <- NULL
  cired  <- extract(rastim.cired,to.pl[,c("x","y")],method=c("simple","bilinear")[1])
  colnames(cired) <- "cired"
  to.pl <- data.frame(to.pl,cired)
  
  ## ndre
  ndre <- NULL
  ndre  <- extract(rastim.ndre,to.pl[,c("x","y")],method=c("simple","bilinear")[1])
  colnames(ndre) <- "ndre"
  to.pl <- data.frame(to.pl,ndre)
  
  ## ndvi
  ndvi <- NULL
  ndvi  <- extract(rastim.ndvi,to.pl[,c("x","y")],method=c("simple","bilinear")[1])
  colnames(ndvi) <- "ndvi"
  to.pl <- data.frame(to.pl,ndvi)
  
  ## wdvi
  wdvi <- NULL
  wdvi  <- extract(rastim.wdvi,to.pl[,c("x","y")],method=c("simple","bilinear")[1])
  colnames(wdvi) <- "wdvi"
  to.pl <- data.frame(to.pl,wdvi)
  
  ## combine
  all.pl <- rbind(all.pl,to.pl)
  
}

all.pl[1:5,]
# save(all.pl,file="obj_all.pl_sat_rep1_2506_rgb_dsm_msp_red_nd.out")
# save(all.pl,file="obj_all.pl_ser_rep1_2506_rgb_dsm_msp_red_nd.out")
# save(all.pl,file="obj_all.pl_sat_rep2_2506_rgb_dsm_msp_red_nd.out")
# save(all.pl,file="obj_all.pl_ser_rep2_2506_rgb_dsm_msp_red_nd.out")

# Let's plot variables for an example plot again.

example_plot <- all.pl[all.pl$use.plotid=="IL269",]

# Using the rgb function we can obtain we can obtain a hexadecimal representation of the red green and blue
# intensities in each cell. Each hex code corresponds to a color which we can feed to ggplot for plotting.
use.rgb <- rgb(example_plot$red/255,example_plot$green/255,example_plot$blue/255)

rgb_plot <- ggplot(example_plot,aes(x,y))+
  geom_tile(fill=use.rgb)+
  ggtitle(paste(unique(example_plot$use.lk),"RGB image"))+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")

rgb_plot

# We can similarly plot the height data.

height_plot <- ggplot(example_plot,aes(x,y))+
  geom_tile(aes(fill=(estheight)))+
  scale_fill_gradientn(colors = magma(256),name = "Height")+
  ggtitle(paste(unique(example_plot$use.lk),"DSM data")) +
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25)) +
  xlab("Longitude")+
  ylab("Latitude")

height_plot

# And let's try to plot the multi-spectral data. We can use the rgb()  function again,
# but the picture reconstructed from the multi-spectral data will not quite be the same as from the rgb
# camera. Don't stress, that's the usual way. The best way to plot rgb picture is to normalize each wavelength 
# with its max value.

# use.rgb <- rgb(to.pl$msp3,to.pl$msp2,to.pl$msp1,maxColorValue = 0.16)
use.rgb <- rgb(example_plot$red.1/max(example_plot$red.1),example_plot$green.1/max(example_plot$green.1),example_plot$red.1/max(example_plot$red.1))

rgb_MS <- ggplot(example_plot,aes(x,y))+
  geom_tile(fill=use.rgb)+
  ggtitle(paste(unique(example_plot$use.lk),"RGB MS")) + 
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25)) +
  xlab("Longitude")+
  ylab("Latitude")

rgb_MS

# Of course, we can also plot the other two wavelengths, although we'll need to use an artifical color scale.

red_edge_plot <- ggplot(example_plot,aes(x,y))+
  geom_tile(aes(fill=(red_edge)))+
  scale_fill_gradientn(colors = magma(256),name = "Red-edge")+
  ggtitle(paste(unique(example_plot$use.lk), "Red-edge")) +
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25)) +
  xlab("Longitude")+
  ylab("Latitude")

red_edge_plot

NIR_plot <- ggplot(example_plot,aes(x,y))+
  geom_tile(aes(fill=(NIR)))+
  scale_fill_gradientn(colors = magma(256),name = "NIR")+
  ggtitle(paste(unique(example_plot$use.lk), "NIR")) +
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25)) +
  xlab("Longitude")+
  ylab("Latitude")

NIR_plot

# As an aside (not essential to the preprocessing), we can use kmeans to cluster the data and try to distinguish
# the plants from the soil.

# We can check the correlation of all variables, and plot these as a heatmap.

use.cor <- cor(to.pl[,c("red","green","blue","estheight","red.1","green.1","blue.1","red_edge","cired","cired","ndre","ndvi","wdvi")])
use.cor
heatmap(use.cor,scale="none")

# setting seed makes it reproducible.
set.seed(1337)
to.pl <- all.pl

# We choose a subset of variables that are not strongly correlated, and mean center and scale the data. 
use.data <- to.pl[,c("green","blue","estheight","green.1","NIR","cired","ndvi")] ; colm <- apply(to.pl[,c("green","blue","estheight","green.1","NIR","cired","ndvi")],2,mean)
fkm <- scale(use.data)

# Now we  perform k-means, and add the cluster assignment of each pixel to the dataframe.
kcl <- kmeans(fkm,3)
to.pl2 <- data.frame(to.pl,kcl$cluster)

# Now let's plot the cluster assignments of an example plot. 
example_plot <- ggplot(to.pl2[to.pl2$use.plotid == "IL433",],aes(x,y))+
              geom_tile(aes(fill=as.factor(kcl.cluster)))

# Likely cluster 1 is plants, 2 is shade and 3 is empty soil. 
example_plot

# Now let's plot rgb values to see if interpretation was correct.

IL433_km <- to.pl2[to.pl2$use.plotid == "IL433",]

use.rgb <- rgb(IL433_km$red/255,IL433_km$green/255,IL433_km$blue/255)

example_rgb <- ggplot(IL433_km,aes(x,y))+
  geom_tile(fill=use.rgb) +
  ggtitle(paste("RGB","IL433"))+
  facet_wrap(.~kcl.cluster)+
  theme_minimal()
example_rgb

# Finally, let's see how the different variables are distributed within each cluster.

values <- stack(to.pl2[,c("red","green","blue","estheight","red.1","green.1","blue.1","red_edge","NIR","cired","ndre","ndvi","wdvi")])
Kcluster <- rep(as.factor(to.pl2$kcl.cluster),13)
to.pl3 <- data.frame(Kcluster,values)

var_per_cluster <- ggplot(to.pl3,aes(Kcluster,values))+
            geom_boxplot(aes(fill=Kcluster))+
            facet_wrap(.~ind,scale="free")

var_per_cluster


