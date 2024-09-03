##############################################################################################################
## LBS version ###############################################################################################
setwd("C:/Users/RensD/OneDrive/Work/RA/Drone_paper/Bram")

library(openxlsx)
library(ggplot2)
library(ggnetwork)
library(viridis)
library(cowplot)

plot.frame <- read.xlsx(xlsxFile = "./DroneData2023/Necessary_data.xlsx",sheet =  "pvalues")
sat.full <- read.xlsx(xlsxFile = "./DroneData2023/Necessary_data.xlsx", sheet = "Phenotypes")

plot.frame <- read.xlsx(xlsxFile = "D:/Drone-paper/Necessary_data.xlsx",sheet =  "pvalues")
sat.full <- read.xlsx(xlsxFile = "D:/Drone-paper/Necessary_data.xlsx", sheet = "Phenotypes")

sat.full.colnames <- sat.full$TraitID
sat.full <- data.frame(t(sat.full[,-(1:4)]))
colnames(sat.full) <- sat.full.colnames

only.mean <- colnames(sat.full)[ grep("mean",colnames(sat.full))[ ! grep("mean",colnames(sat.full)) %in% 
                                                                    c(grep("diff",colnames(sat.full)),grep("trimmed",colnames(sat.full)))]]
##
sat.full <- sat.full[,only.mean]
## calculate correlation matrix
cor.sat <- cor(sat.full, use = "pairwise")
## clustering
set.seed(1)
km.sat.sqrd <- kmeans(cor.sat^2, centers = 10)

## make network + layout
cor.sat.08 <- cor.sat
cor.sat.08[lower.tri(cor.sat.08)] <-0
cor.sat.08[abs(cor.sat.08)<0.8] <- 0
n <- which(abs(cor.sat.08)>0.8,arr.ind = T)
ok <- ggnetwork(n,arrow.gap = 0)
ok <-data.frame(ok,Phenotype = rownames(cor.sat)[ok$vertex.names])
ok$Phenotype
### add additional data
kmeans_clust <- as.factor(km.sat.sqrd$cluster[ok$vertex.names])
clust.names <- c("A: Color change","B: Color day 93","C: Vegetation indices (VI)","D: Green","E: Red/green",
                 "F: Relative blue","G: VI change","H: Height","I: Relative red","J: MSP change")
kmeans_clust <- clust.names[kmeans_clust]

Atday <- rep(NA,nrow(ok))
Atday[grep("1106",ok$Phenotype)] <- "Day 78"
Atday[grep("2506",ok$Phenotype)] <- "Day 93"
Atday[grep("diff",ok$Phenotype)] <- "Diff"
Atday[grep("dira",ok$Phenotype)] <- "Change" # change ratio to change to make figure more intuitive

to.pl <- data.frame(ok,kmeans_clust,Atday)
to.pl[1:5,]

## plot
cor.net.08 <- ggplot(to.pl, aes(x = x, y = y, xend = xend, yend = yend,fill = kmeans_clust,shape=Atday)) +
  geom_edges(color = "grey35",curvature = 0.3, lwd = 0.25) +
  geom_point(size = 3)+
  scale_fill_manual(values = turbo(11)[2:11])+
  scale_shape_manual(values = c(21,22,23))+
  theme_blank()+
  guides(fill = guide_legend(override.aes = list(size = 4,shape = 21), title = "Kmeans cluster"))+
  theme(text = element_text(size = 9), legend.title = element_text(size=7))

cor.net.08

#### Create scatter plots with trend line, showing correlation #########################################################################################
cor.sat <- abs(cor.sat)
relblue.cor <- data.frame(as.numeric(cor.sat[row.names(cor.sat) == "relblue.mean.1106",]),
                          rownames(cor.sat))
colnames(relblue.cor) <- c("correlation", "trait")
relblue.cor <- relblue.cor[order(relblue.cor$correlation),]
relblue.cor$trait <- factor(relblue.cor$trait, levels = relblue.cor$trait)

ggplot(relblue.cor, aes(x = trait, y = correlation, label = trait))+
  geom_line(aes(group = 1))+
  geom_point() + 
  geom_hline(yintercept = 0.8,col="red",linewidth=0.1)+
  theme_light()+
  ylab("relblue day-78 correlation") +
  xlab("traits") +
  geom_text(data = relblue.cor[relblue.cor$correlation > 0.8,], 
            nudge_y = 0.02, check_overlap = TRUE)+
  theme(axis.text.x = element_blank())

### Create another  ### -------------------------------------------------------------------
height.frame <- sat.full[,colnames(sat.full) == "height.mean.1106" | colnames(sat.full) == "height.mean.2506"]
height.cor <- ggplot(height.frame, aes(x= height.mean.1106, y=height.mean.2506))+
  geom_smooth(method = "lm")+
  geom_point(size = 0.3) + 
  ylab("height day 93") +
  xlab("height day 78") + 
  theme_light()+
  geom_label(aes(x= 0.08, y = 0.55, label = "cor = 0.68"), size = 2.5)+
  theme(axis.text = element_text(size = 5),
        axis.title = element_text(size = 6))
height.cor


color.frame <- sat.full[,colnames(sat.full) == "gbrat.mean.1106" | colnames(sat.full) == "relgreen.mean.1106"]
col.cor <- ggplot(color.frame, aes(x= gbrat.mean.1106, y=relgreen.mean.1106))+
  geom_smooth(method = "lm")+
  geom_point(size = 0.3) + 
  ylab("relative green day 78") +
  xlab("green blue ratio day 93") + 
  theme_light()+
  geom_label(aes(x= -0.18, y = 0.5, label = "cor = 0.97"), size = 2.5)+
  theme(axis.text = element_text(size = 5),
        axis.title = element_text(size = 6))
col.cor

plotAB <- plot_grid(height.cor, col.cor, ncol = 1, nrow=2, labels = c("A", "B"))
fig3 <- plot_grid(plotAB, cor.net.08, ncol=2, nrow=1, rel_widths = c(1,2.5), labels = c(" ", "C"))
fig3
ggsave("Script_per_figure/Figures/figure3composite.png", width = 15, height = 10, units = "cm" )


######################## Or another version of figure 3 ##########################################3
colour_vector <- 1:10
names(colour_vector) <-  sort(unique(to.pl$kmeans_clust))
colour_vector[1:10] <- NA
# Colors from : RColorBrewer::brewer.pal(8,name = 'Dark2')
colour_vector[1:10] <- c("#1B9E77", "#D95F02", "#6A3D9A", "#66A61E","#E7298A", "#1F78B4", "#E6AB02", "#A6761D", "#D7301F", "#666666")
## plot

cor.net.08 <- ggplot(to.pl, aes(x = x, y = y, xend = xend, yend = yend,fill = kmeans_clust,shape=Atday)) +
  geom_edges(color = "grey35",curvature = 0.3, lwd = 0.25) +
  geom_point(size = 3)+
  scale_fill_manual(values = colour_vector)+
  scale_shape_manual(values = c(21,22,23))+
  theme_blank()+
  guides(fill = guide_legend(ncol = 2, byrow = TRUE, override.aes = list(size = 4,shape = 21)),
         shape = guide_legend(ncol = 3))+
  theme(text = element_text(size = 9), legend.title = element_text(size=),
        legend.position = "bottom"
        ,legend.direction = "vertical",legend.box = "vertical") +
  labs(fill = "Kmeans cluster", shape = "Day")

cor.net.08
ggsave("Script_per_figure/Figures/figure3_legendbottom.png", width = 8, height = 18, units = "cm" )

cor.net.08 <- ggplot(to.pl, aes(x = x, y = y, xend = xend, yend = yend,fill = kmeans_clust,shape=Atday)) +
  geom_edges(color = "grey35",curvature = 0.3, lwd = 0.25) +
  geom_point(size = 2)+
  scale_fill_manual(values = colour_vector)+
  scale_shape_manual(values = c(21,22,23))+
  theme_blank()+
  guides(fill = guide_legend(ncol = 2, byrow = TRUE, override.aes = list(size = 4,shape = 21)),
         shape = guide_legend(ncol = 3))+
  theme(text = element_text(size = 9), legend.title = element_text(size=),
        ,legend.direction = "vertical",legend.box = "vertical") +
  labs(fill = "Kmeans cluster", shape = "Day")

cor.net.08
ggsave("Script_per_figure/Figures/figure3_legendside.png", width = 14, height = 7, units = "cm" )
# empty_plot <- ggplot() + theme_void()
# fig3 <- plot_grid(cor.net.08, empty_plot, ncol=2,
#                   rel_widths = c(2.5,1), labels = c(" ", " "))
# fig3

