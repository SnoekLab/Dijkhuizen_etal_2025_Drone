
### Author: Basten L. Snoek -- 2024-10-02 -- l.b.snoek@uu.nl

## script part of the publication:
# ------------------------------------------------------------------------------
# From   aerial  drone to QTL: Leveraging next-generation phenotyping to reveal 
# the genetics of color and height in field-grown Lactuca sativa
#
# Rens Dijkhuizen*, Bram van Eijnatten*, Sarah Mehrem, Esther van den Bergh,
# Jelmer van Lieshout, Kiki Spaninks, Steven Kaandorp, Remko Offringa, 
# Marcel Proveniers, Guido van den Ackerveken, Basten L. Snoek 
#
# ------------------------------------------------------------------------------
#

### This script works with the following files (R data objects)
obj_drone_ima.rep1_sat_1106_rgb_dsm_msp.R4.3.2.out
obj_drone_ima.rep1_sat_2506_rgb_dsm_msp_red_nd.R4.3.2.out
obj_drone_ima.rep2_sat_1106_rgb_dsm_msp.R4.3.2.out
obj_drone_ima.rep2_sat_2506_rgb_dsm_msp_red_nd.R4.3.2.out

## library to make a plot ##
library(ggplot2)

### Example to load the data and make a plot ...................................

load("R_objects_plots/obj_drone_ima.rep1_sat_1106_rgb_dsm_msp.R4.3.2.out") # load data

# show data
head(all.pl)

# mean green per plot (note this still includes the soil pixels)
meangreen.per.lk <- aggregate(all.pl$green,list(all.pl$use.lk),mean,na.rm=T)
hist(meangreen.per.lk$x,breaks = 100)

# select one genotype and make a plot
use.title <- "LK125"
sub.pl <- data.frame(all.pl[all.pl$use.lk == use.title,1:6],day="day1")
use.rgb <- rgb(red=sub.pl$red,green = sub.pl$green,blue = sub.pl$blue,maxColorValue = 255)
my.plot <- ggplot(sub.pl)+
  geom_raster(aes(x,y),fill=use.rgb)+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  xlab(use.title) + ylab("") +
  theme(plot.title = element_text(size  = 28),
        plot.title.position = "plot",
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = 24)
  )

my.plot


######################## END ##################################################