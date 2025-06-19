## Script to make figure 4. Example on height, green blue ratio and rededge.
library(openxlsx)
library(cowplot)
library(ggplot2)
library(viridis)

# Load in the data
height.frame <- read.xlsx(xlsxFile =  "D:/Drone-paper/Supplemental_data.xlsx", sheet =  "Height.mean.day2")
gbrat.frame <- read.xlsx(xlsxFile =  "D:/Drone-paper/Supplemental_data.xlsx", sheet = "gbrat.mean.day2")
rededge.frame <- read.xlsx(xlsxFile =  "D:/Drone-paper/Supplemental_data.xlsx", sheet = "rededge.mean.day2")
#Locations of prominent peaks, annotated with colored rectangles:
rectangles <- data.frame(read.xlsx("D:/Drone-paper/Supplemental_data.xlsx", sheet = "peaks.fig4"))


 # make vector with trait names
Trait <- c(rep("Height",nrow(height.frame)),rep("G/B ratio",nrow(gbrat.frame)),rep("Red Edge",nrow(rededge.frame)))

# bind all together in one data frame
all.frames <- rbind(height.frame,gbrat.frame,rededge.frame)
all.frames$pval <- -log10(all.frames$pval)

## make pseudo points to get the right facet panle size
pseudo.points <- data.frame(cbind(rep(1:9,2),rep(2,18),c(rep(0,9),214.8,217.1,257.8,377.4,339.6,193.1,195.5,309.6,203.9)))
colnames(pseudo.points) <- c("chr","pval","pos")

## make the figure
to.pl <- data.frame(Trait,all.frames)

fig4 <- ggplot(to.pl[to.pl$pval>2,],aes(pos,pval))+
  geom_point(data = pseudo.points,aes(pos,pval),col="grey70")+
  geom_point(aes(col=pval>7), size = 0.75)+
  geom_rect(data = rectangles[rectangles$type == "confirmed",],
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), inherit.aes = FALSE,
            fill = alpha("grey",0), color = "magenta2", size = 0.3)+
  geom_rect(data = rectangles[rectangles$type == "new",],
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), inherit.aes = FALSE,
            fill = alpha("grey",0), color = "cyan2", size = 0.3)+
  geom_hline(yintercept = 7,col="red",linewidth=0.1)+
  geom_text(data = rectangles, aes(x = label_x, y= label_y, label = Name), size = 2)+
  facet_grid(Trait~chr,space = "free_x",scale = "free_x")+
  scale_x_continuous(breaks = c(50,100,150,200,250,300,350),expand = c(0,0))+
  scale_color_manual(values = c("grey70","black"))+
  xlab("Position (Mbp)") + ylab(bquote(-log[10] (p)))+
  theme_cowplot()+
  theme(panel.background = element_rect(linewidth = 0.2,color="black"),
        axis.line = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        axis.text.x = element_text(angle = 90,size=8,hjust = 1,vjust = 0.5),
        axis.text.y = element_text(lineheight =7, size = 7),
        axis.title.y = element_text(size = 9),
        panel.spacing.x = unit(1,"mm"),
        panel.spacing.y = unit(1,"mm"),
        strip.text.y = element_text(face="bold", size=8),
        panel.grid.major = element_line(color = "grey2",linewidth =  0.1,linetype = 3),
        legend.position = "none")

fig4

ggsave("Script_per_figure/Figures/figure4a.png", width = 15, height = 7.5, units = "cm" )




#################################### END ################################################################