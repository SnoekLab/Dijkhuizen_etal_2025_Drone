#####################################################################
## Script to show the effectiveness of selectionn on EVI.
############################################################################
library(ggplot2)
library(cowplot)

load(file="R_objects_plots/obj_drone_ima.rep1_sat_1106_rgb_dsm_msp.R4.3.2.out") # day1
all.pl1 <- all.pl
load(file="R_objects_plots/obj_drone_ima.rep1_sat_2506_rgb_dsm_msp_red_nd.R4.3.2.out") #day2
all.pl2 <- all.pl

# Both days have different thresholds
evi.selc.day1 <- 2.5*((all.pl1$msp5-all.pl1$msp3)/(all.pl1$msp5+6*all.pl1$msp3+7.5*all.pl1$msp1+1))>0.25
evi.selc.day2 <- 2.5*((all.pl2$msp5-all.pl2$msp3)/(all.pl2$msp5+6*all.pl2$msp3+7.5*all.pl2$msp1+1))>0.4

# Here we create all plots. At the end of the script we join them all together.
# LK 187 is your typical green cutting lettuce
mm <- all.pl1[all.pl1$use.lk == "LK187" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_187 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK187")

mm <- all.pl2[all.pl2$use.lk == "LK187" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot2_187 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())

mmselc <- all.pl1[evi.selc.day1&all.pl1$use.lk == "LK187",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
selc_187 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())

mmselc <- all.pl2[evi.selc.day2&all.pl2$use.lk == "LK187",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
selc2_187 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())

# LK 198 is oilseed
mm <- all.pl1[all.pl1$use.lk == "LK198" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_198 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK198")

mm <- all.pl2[all.pl2$use.lk == "LK198" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot2_198 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())

mmselc <- all.pl1[evi.selc.day1&all.pl1$use.lk == "LK198",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
selc_198 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())

mmselc <- all.pl2[evi.selc.day2&all.pl2$use.lk == "LK198",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
selc2_198 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())

#LK150 is cos
mm <- all.pl1[all.pl1$use.lk == "LK150" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_150 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK150: RGB day 78")

mm <- all.pl2[all.pl2$use.lk == "LK150" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot2_150 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK150: RGB day 93")

mmselc <- all.pl1[evi.selc.day1&all.pl1$use.lk == "LK150",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
selc_150 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK150: selection day 78")

mmselc <- all.pl2[evi.selc.day2&all.pl2$use.lk == "LK150",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
selc2_150 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK150: selection day 93")

# LK 166
mm <- all.pl1[all.pl1$use.lk == "LK166" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_166 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK166")

mm <- all.pl2[all.pl2$use.lk == "LK166" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot2_166 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())

mmselc <- all.pl1[evi.selc.day1&all.pl1$use.lk == "LK166",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
selc_166 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())

mmselc <- all.pl2[evi.selc.day2&all.pl2$use.lk == "LK166",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
selc2_166 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())

#LK 019
mm <- all.pl1[all.pl1$use.lk == "LK019" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_019 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK019")

mm <- all.pl2[all.pl2$use.lk == "LK019" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot2_019 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())

mmselc <- all.pl1[evi.selc.day1&all.pl1$use.lk == "LK019",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
selc_019 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())

mmselc <- all.pl2[evi.selc.day2&all.pl2$use.lk == "LK019",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
selc2_019 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())


RGB1 <- plot_grid(plot_150, plot_166, plot_198, plot_187, plot_019, nrow = 5)
RGB2 <- plot_grid(plot2_150, plot2_166, plot2_198, plot2_187, plot2_019, nrow = 5)
EVI1 <- plot_grid(selc_150, selc_166, selc_198, selc_187, selc_019, nrow = 5)
EVI2 <- plot_grid(selc2_150, selc2_166, selc2_198, selc2_187, selc2_019, nrow = 5)
all <- plot_grid(RGB1, EVI1, EVI2, RGB2, nrow = 1)
all

png(filename = "EVI_selection.png",width = 1500,height = 1250)
all
dev.off()
