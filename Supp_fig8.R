library(ggplot2)
library(openxlsx)
library(cowplot)
library(viridis)
library(stringr)

plot.frame <- read.xlsx(xlsxFile =  "D:/Drone-paper/Necessary_data.xlsx", "pvalues")
plot.frame <- subset(plot.frame, select = -c(mean_clustering))
plot.frame <- na.omit(plot.frame)
head(plot.frame)
#Locations of prominent peaks:
rectangles <- data.frame(read.xlsx("D:/Drone-paper/Necessary_data.xlsx", sheet = "peaks.fig6"))
rectangles

### INCLUDE DAY and diff/dira as column 
obs <- rep(NA,nrow(plot.frame))
obs[grepl("1106",plot.frame$Phenotype)] <- "Day1"
obs[grepl("2506",plot.frame$Phenotype)] <- "Day2"
obs[grepl("dira",plot.frame$Phenotype)] <- "Ratio Day1/Day2"
obs[grepl("diff",plot.frame$Phenotype)] <- "Diff"
plot.frame$obs <- obs
unique(plot.frame$obs)

## make pseudo points to get the right facet panel size
pseudo.points <- data.frame(cbind(rep(1:9,2),rep(7,18),c(rep(0,9),214.8,217.1,257.8,377.4,339.6,193.1,195.5,309.6,203.9)))
colnames(pseudo.points) <- c("Chromosome","Pval","Position")

## Add a column to annotate whether the trait in question is a mean trait
check.if.mean <- function(row){
  trait.name <- row$Phenotype
  type <- "all traits" # Set type to all unless the following if statements are met
  if (str_detect(trait.name, "mean")){
    if (!str_detect(trait.name, "trimmed")){
      type = "mean only"
    }
  }
  
  return(type)
}

types <- sapply(1:nrow(plot.frame), function(x) check.if.mean(row = plot.frame[x,]))
plot.frame <- cbind(plot.frame, types)
plot.frame <- plot.frame[order(types),]

fig6 <- ggplot()+
  geom_point(data = pseudo.points,aes(Position,Pval),col=NA)+
  geom_point(data = plot.frame,aes(Position,Pval,col=types),alpha=0.8,size=2,shape=17)+
  geom_rect(data = rectangles[rectangles$type == "confirmed",],
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), inherit.aes = FALSE,
            fill = alpha("grey",0), color = "magenta2", size = 0.5)+
  geom_rect(data = rectangles[rectangles$type == "new",],
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), inherit.aes = FALSE,
            fill = alpha("grey",0), color = "cyan2", size = 0.5)+
  geom_text(data = rectangles, aes(x = label_x, y= label_y, label = label), size = 2)+
  facet_grid(all_clustering~Chromosome,space = "free_x",scale = "free_x",labeller = label_wrap_gen(width =13,multi_line = TRUE))+
  scale_color_manual(values = c("gold2", "black"))+
  scale_x_continuous(breaks = c(50,100,150,200,250,300,350),expand = c(0,0))+
  guides(color=guide_legend(title="",override.aes = list(alpha = 1,size=3),direction = "horizontal"))+
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
        legend.text=element_text(size=7))

fig6
ggsave("Script_per_figure/Figures/figure6e.png", width = 15, height = 15, units = "cm" )
