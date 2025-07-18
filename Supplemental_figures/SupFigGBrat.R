#####################################################################
## Script to make supplemental figure, showing green blue ratio
############################################################################
library(ggplot2)
library(cowplot)
library(readxl)
library(dplyr)
library(tidyr)

## Add a  histogram
phenotypes <- read_excel("D:/Drone-paper/Supplemental_data.xlsx", sheet = "Phenotypes")
phenotypes <- phenotypes[phenotypes$TraitID == "gbrat.mean.1106",]

# Purple accessions to highlight
highlight_ids <- c("LK164", "LK183", "LK178", "LK187", "LK111", "LK166", "LK092")

# Pivot to long format
phenotypes_long <- phenotypes %>%
  pivot_longer(
    cols = matches("^[LI]K\\d+"),
    names_to = "Individual",
    values_to = "Value"
  ) %>%
  mutate(highlight_group = ifelse(Individual %in% highlight_ids, "Negative green/blue ratio", "Positive green/blue ratio"))

# Subset individuals to highlight
highlight_inds <- c("LK075", "LK165", "LK175", "LK144")
highlight_points <- phenotypes_long[phenotypes_long$Individual %in% highlight_inds, ]

# Plot histogram, colored by highlight group
antho.histo <- ggplot(phenotypes_long, aes(x = Value, fill = highlight_group)) +
  geom_histogram(binwidth = 0.1, color = "black", position = "identity") +
  scale_fill_manual(values = c("Negative green/blue ratio" = "purple", "Positive green/blue ratio" = "green")) +
  theme_minimal() +
  xlab("Green Blue ratio") +
  ylab("Count") +
  ggtitle(" ") +
  theme(legend.title = element_blank(), legend.position = "top")+
  geom_point(data = highlight_points, 
             aes(x = Value, y = 0),  # y = 0 places it at the bottom
             color = "red", size = 3, inherit.aes = FALSE) +
  geom_text(data = highlight_points, 
            aes(x = Value, y = 0, label = Individual), 
            vjust = -1, color = "red", inherit.aes = FALSE)

antho.histo

### Now let's create the plots
load(file="R_objects_plots/obj_drone_ima.rep1_sat_1106_rgb_dsm_msp.R4.3.2.out")

# Purple varieties
mm <- all.pl[all.pl$use.lk == "LK164" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_164 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =18, hjust = 0.4),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK164")

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

mm <- all.pl[all.pl$use.lk == "LK178" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_178 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =18, hjust = 0.4),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK178")

mm <- all.pl[all.pl$use.lk == "LK187" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_187 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =18, hjust = 0.4),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK187")

mm <- all.pl[all.pl$use.lk == "LK111" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_111 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =18, hjust = 0.4),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK111")

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

mm <- all.pl[all.pl$use.lk == "LK092" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_092 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =18, hjust = 0.4),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK092")

purple <- plot_grid(plot_164, plot_183, plot_178, plot_187, plot_111, plot_166, plot_092, nrow = 2)

### Green ###
mm <- all.pl[all.pl$use.lk == "LK075" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_075 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =18, hjust = 0.4),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK075")

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

mm <- all.pl[all.pl$use.lk == "LK175" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_175 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =18, hjust = 0.4),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK175")

mm <- all.pl[all.pl$use.lk == "LK144" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_144 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =18, hjust = 0.4),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab(element_blank())+
  ylab(element_blank())+
  ggtitle("LK144")

green <- plot_grid(plot_075, plot_165, plot_175, plot_144, nrow = 1)

total.plot <- plot_grid(antho.histo, purple, green, ncol = 1, labels = c("A", "B", "C"), rel_heights = c(1,1.5,1))
total.plot

ggsave("supfig2.pdf", plot = total.plot, width = 200, height = 230, units = "mm") ## GGsave gives a weird visual artefact

#Alternate way to save figure
png("supfig2.png", width = 2400, height = 2900, res = 300)
print(total.plot)
dev.off()
