## Script to create the supplementary clustering on all traits

library(openxlsx)
library(ggplot2)
library(ggnetwork)
library(viridis)
library(cowplot)
library(tidyr)
library(pals)

# Load in data
plot.frame <- read.xlsx(xlsxFile = "D:/Drone-paper/Supplemental_data.xlsx",sheet =  "pvalues")
sat.full <- read.xlsx(xlsxFile = "D:/Drone-paper/Supplemental_data.xlsx", sheet = "Phenotypes")

sat.full.colnames <- sat.full$TraitID
sat.full <- data.frame(t(sat.full[,-(1:4)]))
colnames(sat.full) <- sat.full.colnames

selection <- colnames(sat.full)[grep("diff|px|ndvi2|growth_width", colnames(sat.full), invert = TRUE)]

sat.full <- sat.full[,selection]
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
# Or alternatively just stick with A,B C
clust.names <- c("A","B","C","D","E","F","G","H","I","J")

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
  theme(text = element_text(size = 12), legend.title = element_text(size=),
        ,legend.direction = "vertical",legend.box = "vertical") +
  labs(fill = "Kmeans cluster", shape = "Day")

cor.net.08
#ggsave("Script_per_figure/Figures/supplemental9.png", width = 14, height = 7, units = "cm" )

# There are many traits in here. To see what is in each cluster a table is not practical.
# Instead these barplots are made.

### Plot distribution phenotypes over clusters
pheno <- sapply(colnames(cor.sat),function(x) strsplit(x,".",fixed = T)[[1]][1])
pheno <- unname(pheno)

km.pheno.frame <- data.frame(Phenotype = names(table(factor(pheno[km.sat.sqrd$cluster==1],levels = unique(pheno)))),A = as.vector(table(factor(pheno[km.sat.sqrd$cluster==1],levels = unique(pheno)))),B = as.vector(table(factor(pheno[km.sat.sqrd$cluster==2],levels = unique(pheno)))),
                             C = as.vector(table(factor(pheno[km.sat.sqrd$cluster==3],levels = unique(pheno)))),D = as.vector(table(factor(pheno[km.sat.sqrd$cluster==4],levels = unique(pheno)))),
                             E = as.vector(table(factor(pheno[km.sat.sqrd$cluster==5],levels = unique(pheno)))),F = as.vector(table(factor(pheno[km.sat.sqrd$cluster==6],levels = unique(pheno)))),
                             G = as.vector(table(factor(pheno[km.sat.sqrd$cluster==7],levels = unique(pheno)))),H = as.vector(table(factor(pheno[km.sat.sqrd$cluster==8],levels = unique(pheno)))),
                             I = as.vector(table(factor(pheno[km.sat.sqrd$cluster==9],levels = unique(pheno)))),J = as.vector(table(factor(pheno[km.sat.sqrd$cluster==10],levels = unique(pheno)))))

km.pheno.frame.long <- pivot_longer(km.pheno.frame,cols = -Phenotype,names_to = "Cluster",values_to = "Count")
km.pheno.frame.long$Cluster <- factor(km.pheno.frame.long$Cluster,levels = c("A","B","C",
                                                                             "D","E","F"
                                                                             ,"G","H","I",
                                                                             "J"))

statistic <-  sapply(colnames(cor.sat),function(x) strsplit(x,".",fixed = T)[[1]][2])
statistic[which(statistic == "diff")] <- "Not applicable"

km.stat.frame <- data.frame(Stat = names(table(factor(statistic[km.sat.sqrd$cluster==1],levels = unique(statistic)))),A = as.vector(table(factor(statistic[km.sat.sqrd$cluster==1],levels = unique(statistic)))),B = as.vector(table(factor(statistic[km.sat.sqrd$cluster==2],levels = unique(statistic)))),
                            C = as.vector(table(factor(statistic[km.sat.sqrd$cluster==3],levels = unique(statistic)))),D = as.vector(table(factor(statistic[km.sat.sqrd$cluster==4],levels = unique(statistic)))),
                            E = as.vector(table(factor(statistic[km.sat.sqrd$cluster==5],levels = unique(statistic)))),F = as.vector(table(factor(statistic[km.sat.sqrd$cluster==6],levels = unique(statistic)))),
                            G = as.vector(table(factor(statistic[km.sat.sqrd$cluster==7],levels = unique(statistic)))),H = as.vector(table(factor(statistic[km.sat.sqrd$cluster==8],levels = unique(statistic)))),
                            I = as.vector(table(factor(statistic[km.sat.sqrd$cluster==9],levels = unique(statistic)))),J = as.vector(table(factor(statistic[km.sat.sqrd$cluster==10],levels = unique(statistic)))))


km.stat.frame$Stat[km.stat.frame$Stat == "trimmed"] <- "trimmed_mean_40" #A bug replaces trimmed_mean_40 to trimmed. This fixes that
km.stat.frame.long <- pivot_longer(km.stat.frame,cols = -Stat,names_to = "Cluster",values_to = "Count")
km.stat.frame.long$Cluster <- factor(km.stat.frame.long$Cluster,levels = c("A","B","C",
                                                                           "D","E","F"
                                                                           ,"G","H","I",
                                                                           "J"))

reference <- c("blue", "relblue", "green", "relgreen", "red", "relred", "gbrat", "gborrat", "rbrat", "rbograt",
               "rgrat", "rgobrat", "coltot", "msp1", "msp2", "msp3", "msp4", "msp5",
               "sipi", "ARVI", "cired", "SR", "ndvi", "EVI", "ndre", "wdvi", "height", "growth_width")
custom_colors <- c("#99ccff", "#3366ff", "#99ffcc", "#00ff00", "#ff9999", "#cc0000", "#00ff99", "#00ffcc", "#990099", "#cc0099",
                   "#ffff00", "#ffffcc", "#f2f2f2", "#003399", "#009933", "#993300", "#cc9900", "#ff1a1a",
                   "#9494b8", "#47476b", "#df9f9f", "#4d1919", "#ffc6b3", "#ff531a", "#661a00", "#4d3900",
                   "#b3b3ff", "#00cc99")

sorted_frame <- km.pheno.frame.long[order(factor(km.pheno.frame.long$Phenotype, levels = reference)),] # Frame with phenotypes sorted on wavelength
sorted_frame <- sorted_frame[sorted_frame$Phenotype != "tot",] #Remove the tot frame because it is rather meaningless
transformed_frame <- sorted_frame

for (cluster in levels(sorted_frame$Cluster)){ # Transform absolute counts into percentages
  cluster_frame <- sorted_frame[sorted_frame$Cluster == cluster,]
  total <- sum(cluster_frame$Count)
  new_counts <- cluster_frame$Count / total * 100
  transformed_frame$Count[transformed_frame$Cluster == cluster] <- new_counts
}
names(transformed_frame)[names(transformed_frame) == "Count"] <- "Percentage"

bar.pheno.plt <- ggplot(transformed_frame,aes(x = Cluster, y = Percentage, fill = Phenotype))+
  geom_bar(stat = "identity")+
  theme_minimal()+
  scale_fill_manual(breaks = reference, values = custom_colors)+
  theme(text= element_text(size = 12),axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))+
  guides(fill = guide_legend(ncol = 2))

#png(filename = "Clustering/Definitive_clustering/clus_sat_full/bar_pheno_corsqrd_km_12_sat_full.png",width = 750,height = 750)
bar.pheno.plt
#dev.off()

# Bar plot for the statistical methods
# This plot uses alphabet from pals. Be sure to not have another library with a function called alphabet loaded.
transformed_frame <- km.stat.frame.long
for (cluster in levels(km.stat.frame.long$Cluster)){ # Transform absolute counts into percentages
  cluster_frame <- km.stat.frame.long[km.stat.frame.long$Cluster == cluster,]
  total <- sum(cluster_frame$Count)
  new_counts <- cluster_frame$Count / total * 100
  transformed_frame$Count[transformed_frame$Cluster == cluster] <- new_counts
}
names(transformed_frame)[names(transformed_frame) == "Count"] <- "Percentage"

bar.stat.plt <- ggplot(transformed_frame,aes(x = Cluster, y = Percentage, fill = Stat))+
  geom_bar(stat = "identity")+
  theme_minimal()+
  scale_fill_discrete(type = as.vector(alphabet(length(unique(statistic)))))+
  theme(text= element_text(size = 12),axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))
#png(filename = "Clustering/Definitive_clustering/clus_sat_full/bar_stat_corsqrd_km_12_sat_full.png",width = 750,height = 750)
bar.stat.plt
#dev.off()

top_row <- plot_grid(cor.net.08, labels = "A", ncol = 1)
bottom_row <- plot_grid(bar.pheno.plt, bar.stat.plt, labels = c("B", "C"))
figure_1 <- plot_grid(top_row, bottom_row, ncol = 1, rel_heights = c(1, 1))
png(filename = "Script_per_figure/Figures/supplemental9.png",width = 800,height = 950)
figure_1
dev.off()

ggsave("supplemental9.png", figure_1, width = 14, height = 16, units = "cm" )
