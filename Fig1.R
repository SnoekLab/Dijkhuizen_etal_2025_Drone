## Script to make figure 1. Examples of all lettuce morphology types.
# This script requires the dataframes containing the image data.
# These are the same files as used in "Extr_phe_per_plot_annotated.R".
library(ggplot2)
library(cowplot)

# Load in the raw image data of day 1 or 2. Here we only look at rep1.
# In the paper we used day 2 (2506).
#load(file="Robjects_allpl/obj_drone_ima.rep1_sat_1106_rgb_dsm_msp.R4.3.2.out")
load(file="Robjects_allpl/obj_all.pl_sat_rep1_2506_rgb_dsm_msp_red_nd.out")

#First we define a function to plot a accession by their LKID
plot.plot <- function(use.lk =  "LK147",use.scale = 0.25,use.title = "my title here"){
  cos <- all.pl[all.pl$use.lk == use.lk ,]
  total.x.dist <- max(cos$x) - min(cos$x)
  total.y.dist <- max(cos$y) - min(cos$y)
  z.fac <- use.scale #Factor to determine how much you want to zoom in on the plot
  sub.pl <- cos[cos$x > (min(cos$x)+z.fac*total.x.dist) & cos$x < (max(cos$x)-z.fac*total.x.dist) &
                  cos$y > (min(cos$y)+z.fac*total.y.dist) & cos$y < (max(cos$y)-z.fac*total.y.dist),]
  sub.pl <- data.frame(sub.pl,use.title)
  use.rgb <- rgb(red=sub.pl$red,green = sub.pl$green,blue = sub.pl$blue,maxColorValue = 255)
  my.plot <- ggplot(sub.pl)+
    geom_tile(aes(x,y),fill=use.rgb)+
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    facet_grid(.~use.title)+
    xlab(use.title) + ylab("") +# ggtitle(use.title) +
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

  return(my.plot)
}

# We handpicked these examples of the different morphology groups.
## Butterhead
butterhead.fig <- plot.plot(use.lk = "LK061",use.title = "Butterhead")
butterhead.fig
## Cos
cos.fig <- plot.plot(use.lk = "LK147",use.title = "Cos")
cos.fig
## Crisp LK067
crisp.fig <- plot.plot(use.lk = "LK067",use.title = "Crisp")
crisp.fig
## cutting LK183
cutting.fig <- plot.plot(use.lk = "LK183",use.title = "Cutting")
cutting.fig
## Latin LK107
latin.fig <- plot.plot(use.lk = "LK107",use.title = "Latin")
latin.fig
## Oilseed LK198
oilseed.fig <- plot.plot(use.lk = "LK198",use.title = "Oilseed")
oilseed.fig
## Stalk LK194
stalk.fig <- plot.plot(use.lk = "LK194",use.title = "Stalk")
stalk.fig


plot_grid(butterhead.fig,
          cos.fig,
          crisp.fig,
          cutting.fig,
          latin.fig,
          oilseed.fig,
          stalk.fig,
          ncol=7)
