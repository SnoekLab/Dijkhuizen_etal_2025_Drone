library(ggplot2)
library(openxlsx)
library(cowplot)

plot.frame <- read.xlsx(xlsxFile =  "D:/Drone-paper/Necessary_data.xlsx", "height.traits")
load(file="Robjects_allpl/obj_all.pl_sat_rep1_2506_rgb_dsm_msp_red_nd.out") # The images

plot.frame <- read.xlsx(xlsxFile =  "./DroneData2023/Necessary_data.xlsx", "height.traits")
load(file="./DroneData2023/obj_all.pl_sat_rep1_2506_rgb_dsm_msp_red_nd.out")



## make pseudo points to get the right facet panel size
pseudo.points <- data.frame(cbind(rep(1:9,2),rep(7,18),c(rep(0,9),214.8,217.1,257.8,377.4,339.6,193.1,195.5,309.6,203.9)))
colnames(pseudo.points) <- c("Chromosome","Pval","Position")

plot.frame$alpha <- NA
plot.frame$size <- NA
plot.frame$alpha[plot.frame$types == "descriptives"] <- .9
plot.frame$alpha[plot.frame$types != "descriptives"] <- 1
plot.frame$size[plot.frame$types == "descriptives"] <- 1.5
plot.frame$size[plot.frame$types == "descriptives"] <- 2.5




fig7c <- ggplot()+
  geom_point(data = pseudo.points,aes(Position,Pval),col=NA)+
  geom_point(data = plot.frame, aes(Position, Pval, col = stat, alpha = alpha, size = size), shape = 17)+
  geom_point(data = plot.frame[plot.frame$types != "descriptives",], aes(Position, Pval), size = 3, shape = 17)+ #Manually add the mean traits again so they're on top
  scale_color_manual(values = c("cyan3","black","red4","#7570B3","magenta","#66A61E","#E6AB02"))+
  scale_alpha_identity() + scale_size_identity()+
  scale_x_continuous(breaks = c(50,100,150,200,250,300,350),expand = c(0,0))+
  facet_grid(cols = vars(Chromosome), space = "free_x",scale = "free_x") +
  guides(color=guide_legend(title="",override.aes = list(alpha = 1,size=2),direction = "horizontal"),
         alpha = guide_none(),
         size = guide_none())+
  xlab("Position (Mbp)") + ylab(bquote(-log[10] (p)))+
  theme_cowplot()+
  theme(panel.background = element_rect(linewidth = 0.2,color="black"),
        axis.line = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        axis.text.x = element_text(angle = 90,size=7,hjust = 1,vjust = 0.5),
        axis.text.y = element_text(size = 7),
        panel.spacing.x = unit(1,"mm"),
        panel.spacing.y = unit(1,"mm"),
        strip.text.y = element_text(face="bold", size=9,angle = 0),
        panel.grid.major = element_line(color = "grey2",linewidth =  0.1,linetype = 3),
        legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.background = element_rect(linewidth = 0.2,fill = "grey90",color = "black"),
        legend.text=element_text(size=8),
        legend.key.size = unit(0.2, "cm"))

fig7c
ggsave("Script_per_figure/Figures/figure7.png", width = 15, height = 5, units = "cm" )

fig7a <- all.pl[all.pl$use.lk == "LK153" ,]
use.rgb <- rgb(red=fig7a$red,green = fig7a$green,blue = fig7a$blue,maxColorValue = 255)
fig7a.plot <- ggplot(fig7a)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =12, hjust = 0.1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  ggtitle(" ")
fig7a.plot

fig7b <- all.pl[all.pl$use.lk == "LK153" ,]
fig7b$estheight <- fig7b$estheight - min(fig7b$estheight) # estimation
fig7b.plot <- ggplot(fig7b)+
  geom_tile(aes(x,y, fill = estheight))+
  scale_fill_viridis_c()+
  theme_void()+
  theme(plot.title = element_text(size  =12, hjust = 0.1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  ggtitle("LK153")
fig7b.plot

#Alternate figure 7b

fig7b <- all.pl[all.pl$use.lk == "LK153" ,]
use.rgb <- rgb(red=fig7b$red,green = fig7b$green,blue = fig7b$blue,maxColorValue = 255)
fig7b.plot <- ggplot(fig7b)+
  geom_tile(aes(x,y, fill = estheight))+
  scale_fill_viridis_c()+
  theme_void()+
  theme(plot.title = element_text(size  =12, hjust = 0.1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK153")
fig7b.plot

### Another alternative plot 7b
fig7b.plot <- ggplot() + geom_blank() + theme_bw() + # Create a blank plot to draw on.
  theme(panel.border = element_blank())
fig7b.plot
fig7b.plot <- ggdraw(fig7b.plot) + draw_image("boltinglettuce.png")

################################
fig7AB <- plot_grid(fig7a.plot, fig7b.plot, ncol =2, labels = c("A", "B"))
fig7ABC <- plot_grid(fig7AB, fig7c, nrow = 2, rel_heights = c(1.5,2), labels = c("", "C"))
fig7ABC

ggsave("Script_per_figure/Figures/figure7ABC.png", width = 15, height = 10, units = "cm" )
png("Script_per_figure/Figures/figure7abcpng.png", width = 860, height = 540)
fig7ABC
dev.off()
