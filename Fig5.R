## Script to make figure 5. The large aggregate of manhattan plots
library(ggplot2)
library(openxlsx)
library(cowplot)
library(viridis)

plot.frame <- read.xlsx(xlsxFile =  "D:/Drone-paper/Supplemental_data.xlsx", sheet =  "pvalues")
plot.frame <- plot.frame[!is.na(plot.frame$mean_clustering),]
head(plot.frame)
#Locations of prominent peaks, annotated with rectangles:
rectangles <- data.frame(read.xlsx("D:/Drone-paper/Supplemental_data.xlsx", sheet = "peaks.fig5"))

### INCLUDE DAY and diff/dira as column 
obs <- rep(NA,nrow(plot.frame))
obs[grepl("1106",plot.frame$Phenotype)] <- "Day78"
obs[grepl("2506",plot.frame$Phenotype)] <- "Day93"
obs[grepl("dira",plot.frame$Phenotype)] <- "Ratio Day93/Day78"
obs[grepl("diff",plot.frame$Phenotype)] <- "Diff"
plot.frame$obs <- obs
unique(plot.frame$obs)

## order the cluster so those with more similar patterns are grouped together
## not needed for mean, seems ordered good enough.

## add names to clusters 
clust.name <- plot.frame$mean_clustering
clust.name[clust.name == "A"] <- "Cluster A: Color change"
clust.name[clust.name == "B"] <- "Cluster B: Color day 93"
clust.name[clust.name == "C"] <- "Cluster C: VI"
clust.name[clust.name == "D"] <- "Cluster D: Green"
clust.name[clust.name == "E"] <- "Cluster E: Red/Green"
clust.name[clust.name == "F"] <- "Cluster F: Relative blue"
clust.name[clust.name == "G"] <- "Cluster G: VI change"
clust.name[clust.name == "H"] <- "Cluster H: Height"
clust.name[clust.name == "I"] <- "Cluster I: Relative red"
clust.name[clust.name == "J"] <- "Cluster J: MSP change"
plot.frame$mean_clustering <- clust.name


## make pseudo points to get the right facet panel size
pseudo.points <- data.frame(cbind(rep(1:9,2),rep(7,18),c(rep(0,9),214.8,217.1,257.8,377.4,339.6,193.1,195.5,309.6,203.9)))
colnames(pseudo.points) <- c("Chromosome","Pval","Position")

fig5 <- ggplot()+
  geom_point(data = pseudo.points,aes(Position,Pval),col=NA)+
  geom_point(data = plot.frame,aes(Position,Pval,col=obs),alpha=0.4,size=2,shape=17)+
  geom_rect(data = rectangles[rectangles$type == "confirmed",],
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), inherit.aes = FALSE,
            fill = alpha("grey",0), color = "magenta2", size = 0.5)+
  geom_rect(data = rectangles[rectangles$type == "new",],
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), inherit.aes = FALSE,
            fill = alpha("grey",0), color = "cyan2", size = 0.5)+
  geom_text(data = rectangles, aes(x = label_x, y= label_y, label = label), size = 2)+
  facet_grid(mean_clustering~Chromosome,space = "free_x",scale = "free_x",labeller = label_wrap_gen(width =13,multi_line = TRUE))+
  scale_color_manual(values = c("blue3","red3","grey30"))+
  scale_x_continuous(breaks = c(50,100,150,200,250,300,350),expand = c(0,0))+
  guides(color=guide_legend(title="",override.aes = list(alpha = 1,size=3),direction = "horizontal"))+
  xlab("Position (Mbp)") + 
  ylab(bquote(-log[10] (p)))+
  theme_cowplot()+
  theme(panel.background = element_rect(linewidth = 0.2,color="black"),
        axis.line = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        axis.text.x = element_text(angle = 90,size=7,hjust = 1,vjust = 0.5),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_blank(),
        panel.spacing.x = unit(1,"mm"),
        panel.spacing.y = unit(1,"mm"),
        strip.text.y = element_text(face="bold", size=9,angle = 0),
        panel.grid.major = element_line(color = "grey2",linewidth =  0.1,linetype = 3),
        legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.background = element_rect(linewidth = 0.2,fill = "grey90",color = "black"),
        legend.text=element_text(size=7))
  
fig5

rectangles.hist <-rectangles[!is.na(rectangles$label),]
rectangles.hist$ymin <- rep(0 , dim(rectangles.hist)[1])
rectangles.hist$ymax <- c(6,8, 13, 36, 36, 13, 16, 20, 46, 13, 32, 13, 11, 38)
### add histogram
fig5.2 <- ggplot()+
  geom_point(data = pseudo.points,aes(Position,Pval),col=NA)+
  geom_histogram(data = plot.frame,aes(Position,fill=obs),binwidth = 10)+
  geom_rect(data = rectangles.hist[rectangles.hist$type == "confirmed",],
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), inherit.aes = FALSE,
            fill = alpha("grey",0), color = "magenta2", size = 0.25)+
  geom_rect(data = rectangles.hist[rectangles.hist$type == "new",],
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), inherit.aes = FALSE,
            fill = alpha("grey",0), color = "cyan2", size = 0.25)+
  facet_grid(.~Chromosome,space = "free_x",scale = "free_x",labeller = label_wrap_gen(width =13,multi_line = TRUE))+
  scale_fill_manual(values = c("blue3","red3","grey30"))+
  scale_x_continuous(breaks = c(50,100,150,200,250,300,350),expand = c(0,0))+
  guides(color=guide_legend(title="",override.aes = list(alpha = 1,size=3),direction = "horizontal"))+
  xlab("Position (Mbp)") + ylab("QTL Count")+
  theme_cowplot()+
  theme(panel.background = element_rect(linewidth = 0.2,color="black"),
        axis.line = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        axis.text.x = element_text(angle = 90,size=7,hjust = 1,vjust = 0.5),
        axis.text.y = element_text(size = 7),
        panel.spacing.x = unit(1,"mm"),
        panel.spacing.y = unit(1,"mm"),
        strip.text = element_blank(),
        strip.background = element_blank(),
        
        panel.grid.major = element_line(color = "grey2",linewidth =  0.1,linetype = 3),
        legend.position = "none",
        legend.justification = c(0,1),
        legend.background = element_rect(linewidth = 0.2,fill = "grey90",color = "black"),
        legend.text=element_text(size=7))

fig5.2

plot_grid(fig5,fig5.2,ncol = 1,rel_heights = c(10,2),align = "v",axis = "tblr")

ggsave("Script_per_figure/Figures/figure5.png", width = 15, height = 16, units = "cm" )

######################### END ######################################################
