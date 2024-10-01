## Script to make figure 3. The clustering
library(openxlsx)
library(ggplot2)
library(ggnetwork)
library(viridis)
library(cowplot)

# Load in data
plot.frame <- read.xlsx(xlsxFile = "D:/Drone-paper/Necessary_data.xlsx",sheet =  "pvalues")
sat.full <- read.xlsx(xlsxFile = "D:/Drone-paper/Necessary_data.xlsx", sheet = "Phenotypes")

sat.full.colnames <- sat.full$TraitID
sat.full <- data.frame(t(sat.full[,-(1:4)]))
colnames(sat.full) <- sat.full.colnames

# We will only show the mean in fig3, no absolute difference and no trimmed means
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

# Figure 3 with legend on the side.
colour_vector <- 1:10
names(colour_vector) <-  sort(unique(to.pl$kmeans_clust))
colour_vector[1:10] <- NA
# Colors from : RColorBrewer::brewer.pal(8,name = 'Dark2')
colour_vector[1:10] <- c("#1B9E77", "#D95F02", "#6A3D9A", "#66A61E","#E7298A", "#1F78B4", "#E6AB02", "#A6761D", "#D7301F", "#666666")
## plot


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