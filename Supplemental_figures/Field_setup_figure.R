# This script creates an overview of the field_layout

library(ggplot2)
library(openxlsx)
library(viridis)
library(cowplot)
library(dplyr)
library(data.table)


## data 
load("/R_objects_plots/obj_drone_ima.rep1_sat_1106_rgb_dsm_msp.R4.3.2.out")
LK_lines <- read.xlsx(xlsxFile =  "D:/Drone-paper/Supplemental_data.xlsx", "LK_lines") #We need this to get the morphology types.

all.pl <- all.pl %>%
  left_join(LK_lines %>% select(LKID, Morphology.type),
            by = c("use.lk" = "LKID"))

# the all.pl object contains every pixel of every plot.
# This function extracts the corners, these we use to draw the plot.

get_corners <- function(plot){
  #Left top
  corner1.X <- min(plot$x)
  corner1.Y <- min(plot[plot$x == corner1.X,]$y)
  #right top
  corner2.Y <- max(plot$y)
  corner2.X <- max(plot[plot$y == corner2.Y,]$x)
  #right bottom.
  corner3.X <- max(plot$x)
  corner3.Y <- max(plot[plot$x == corner3.X,]$y)
  #left bottom.
  corner4.Y <- min(plot$y)
  corner4.X <- min(plot[plot$y == corner4.Y,]$x)
  
  
  Plot.ID <- plot$use.plotid[1]
  Accession <- plot$use.lk[1]
  Type <- plot$Morphology.type[1]
  new.row<- data.frame(Plot.ID, Accession, Type, corner1.X, corner1.Y, corner2.X, corner2.Y, corner3.X, corner3.Y, corner4.X, corner4.Y)
  message("Processed plot: ", plot$use.plotid[1])  # show progress
  return(new.row)
}


plot.list <- split(all.pl, all.pl$use.plotid)
corner.list <- lapply(plot.list, get_corners)
ok <- rbindlist(corner.list)

xmean <- apply(ok[,c("corner1.X","corner2.X","corner3.X","corner4.X")],1,mean)
ymean <- apply(ok[,c("corner1.Y","corner2.Y","corner3.Y","corner4.Y")],1,mean)

Xcorners <- as.numeric(c(ok$corner1.X,ok$corner2.X,ok$corner3.X,ok$corner4.X))
Ycorners <- as.numeric(c(ok$corner1.Y,ok$corner2.Y,ok$corner3.Y,ok$corner4.Y))

Plot.ID <- as.factor(rep(ok$Plot.ID,4))
LK.ID <- rep(ok$Accession,4)
#Species <- rep(ok$Species,4)
Crop.type <- rep(ok$Type,4)

ok2 <- data.frame(Plot.ID,LK.ID, Crop.type, Xcorners,Ycorners)

## plot

this <- ggplot(ok2)+
  geom_polygon(aes(x=Xcorners,y=Ycorners, fill = Crop.type),group=Plot.ID) + 
  geom_text(aes(rep(xmean,4),rep(ymean,4),label=LK.ID),size=2) +
  scale_fill_manual(values = turbo(9)[2:8])+
  xlab("X coordinates") + ylab("Y coordinates") +
  theme_minimal()+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
this

pdf(file="Plot_layout_field_trial_sativa.pdf",height = 10,width = 16)
print(this)
dev.off()


############################# END ########################################################################
