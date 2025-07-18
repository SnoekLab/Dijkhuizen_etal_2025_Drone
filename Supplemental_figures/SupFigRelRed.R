#####################################################################
## Script to make supplemental figure, showing relative red
############################################################################
library(ggplot2)
library(cowplot)
library(readxl)
library(dplyr)
library(tidyr)

## Add a  histogram
phenotypes <- read_excel("D:/Drone-paper/Supplemental_data.xlsx", sheet = "Phenotypes")
phenotypes <- phenotypes[phenotypes$TraitID == "relred.mean.1106",]

# Pivot to long format
phenotypes_long <- phenotypes %>%
  pivot_longer(
    cols = matches("^[LI]K\\d+"),
    names_to = "Individual",
    values_to = "Value"
  )

# Subset individuals to highlight
highlight_inds <- c("LK134", "LK121", "LK093", "LK169", "LK036", "LK165", "LK183", "LK166")
highlight_points <- phenotypes_long[phenotypes_long$Individual %in% highlight_inds, ]

# Plot histogram, colored by highlight group
histo <- ggplot(phenotypes_long, aes(x = Value)) +
  geom_histogram(bins = 30, color = "black", fill = "steelblue", position = "identity") +
  theme_minimal() +
  xlab("Relative red") +
  ylab("Count") +
  ggtitle(" ") +
  theme(legend.title = element_blank(), legend.position = "top")+
  geom_point(data = highlight_points, 
             aes(x = Value, y = 0),  # y = 0 places it at the bottom
             color = "red", size = 3, inherit.aes = FALSE) +
  geom_text(data = highlight_points, 
            aes(x = Value, y = 0, label = Individual), 
            vjust = -1, color = "red", inherit.aes = FALSE)

histo

### Now let's create the plots
load(file="R_objects_plots/obj_drone_ima.rep1_sat_1106_rgb_dsm_msp.R4.3.2.out")

mm <- all.pl[all.pl$use.lk == "LK134" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_134 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =18, hjust = 0.4),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK134")

mm <- all.pl[all.pl$use.lk == "LK121" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_121 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =18, hjust = 0.4),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK121")

mm <- all.pl[all.pl$use.lk == "LK093" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_093 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =18, hjust = 0.4),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK093")

mm <- all.pl[all.pl$use.lk == "LK169" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_169 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =18, hjust = 0.4),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK169")

mm <- all.pl[all.pl$use.lk == "LK036" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_036 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =18, hjust = 0.4),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK036")

mm <- all.pl[all.pl$use.lk == "LK165" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_165 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =18, hjust = 0.4),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK165")

mm <- all.pl[all.pl$use.lk == "LK183" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_183 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =18, hjust = 0.4),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK183")

mm <- all.pl[all.pl$use.lk == "LK166" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_166 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =18, hjust = 0.4),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK166")

plots <- plot_grid(plot_134, plot_121, plot_093, plot_169, plot_036, plot_165, plot_183, plot_166, nrow = 2)

total.plot <- plot_grid(histo, plots, rel_heights = c(0.66,1), ncol = 1, labels = c("A", "B"))
total.plot

png("Supfig3.png", width = 2400, height = 2400, res = 300)
print(total.plot)
dev.off()
