# This plot explores the usefullness of the extended descriptives.
# This can also be done per cluster, but this is the brief version that ignores the clusters
library(ggplot2)
library(openxlsx)
library(stringr)



plot.frame <- read.xlsx(xlsxFile =  "D:/Drone-paper/Supplemental_data.xlsx", sheet =  "pvalues.low.thresh")
plot.frame <- plot.frame[plot.frame$Phenotype != "growth_width_diff",]
plot.frame <- plot.frame[plot.frame$Phenotype != "growth_width_dira",]
plot.frame$Position <- plot.frame$Position * 10

### Add a column to determine summary statistic
head(plot.frame)
check.type <- function(row){
  trait.name <- row$Phenotype
  type <- str_split_1(trait.name, "_")[2]
  if (str_detect(trait.name, "Q")){
    type = "quantile"
  }
  return(type)
}

type <- sapply(1:nrow(plot.frame), function(x) check.type(row = plot.frame[x,]))
plot.frame <- cbind(plot.frame, type)


pseudo.points <- data.frame(cbind(rep(1:9,2),rep(7,18),c(rep(0,9),214.8,217.1,257.8,377.4,339.6,193.1,195.5,309.6,203.9)))
colnames(pseudo.points) <- c("Chromosome","Pval","Position")
plot.frame$point_color <- ifelse(plot.frame$Pval < 7, "lightgrey", "black")

manhat_theme <- function(){
  theme(axis.title.x =element_text(size = 12),axis.title.y = element_text(size = 12),axis.text.x = element_text(size = 6, angle = 270), axis.text.y = element_text(size = 6),
        panel.spacing.x = unit(1, "lines"), legend.text = element_text(size = 12),legend.title = element_text(size = 10),strip.text.x = element_text(size = 10),
        panel.border = element_rect(fill = NA,color = "black"),strip.text.y = element_text(size = 10), legend.position = "none", panel.grid = element_line(color = "lightgray", linewidth = 0.1))
}

peak.plot <- ggplot(plot.frame,aes(x = Position,y = Pval))+
  geom_point(aes(color = point_color),size = 0.25, alpha = 0.4) + 
  scale_color_identity()+
  geom_point(data = pseudo.points,aes(Position,Pval),col=NA)+
  facet_grid(type~Chromosome,scales = "free",space = "free_x")+
  scale_x_continuous(expand = c(0.15,0.15),breaks = c(0,100,200,300, 400)) +
  scale_y_continuous(expand = c(0.1,0.1))+
  theme_minimal() +
  xlab("Position in Mb")+
  ylab("")+
  guides(col=guide_legend(title="Day"))+
  manhat_theme()+
  theme(panel.spacing = unit(2, "mm"), panel.spacing.x = unit(1, "mm"))

peak.plot

ggsave("Script_per_figure/Figures/SummaryStatsAlt.png", width = 15, height = 18, units = "cm")
