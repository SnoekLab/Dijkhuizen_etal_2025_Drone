#.libPaths("~/R/x86_64-pc-linux-gnu-library/3.6")

library(ggplot2)
library(viridis)
library(ggnetwork)
library(pals)
library(tidyr)

load("manhat.manhat.nolog.sat.out")

load("GWAS_objects/phe.sat.mean.1106.2506.diff.out")

for(i in grep("trimmed.mean.40",colnames(sat.full))){
  colnames(sat.full)[i] <- gsub("trimmed.mean.40", "trimmed_mean_40", colnames(sat.full)[i])
}

cor.sat <- cor(sat.full)

pheno <- sapply(colnames(cor.sat),function(x) strsplit(x,".",fixed = T)[[1]][1])
pheno <- unname(pheno)

statistic <-  sapply(colnames(cor.sat),function(x) strsplit(x,".",fixed = T)[[1]][2])
statistic[which(statistic == "diff")] <- "Not applicable"

km.sat.full.12.sqrd.cor <- kmeans(cor.sat^2, centers = 12)
#save(km.sat.full.12.sqrd.cor,file = "Clustering/Definitive_clustering/clus_sat_full/km.sat.full.12.sqrd.cor.out")
save(km.sat.full.12.sqrd.cor,file = "Clustering/Definitive_clustering/clus_sat_full/km.sat.full.12.sqrd.cor.out")
#km.sat.full.16.sqrd.cor <- kmeans(cor.sat, centers = 16)

### Correlation clustering

cor.sat.09 <- cor.sat
cor.sat.09[lower.tri(cor.sat.09)] <-0
cor.sat.09[abs(cor.sat.09)<0.9] <- 0
n <- which(abs(cor.sat.09)>0.9,arr.ind = T)
ok <- ggnetwork(n,arrow.gap = 0)
ok <-data.frame(ok,Phenotype = rownames(n))

cor.net.09 <- ggplot(ok, aes(x = x, y = y, xend = xend, yend = yend,col = sapply(colnames(cor.sat)[ok$vertex.names],function(x) strsplit(x, "[.]")[[1]][1]))) +
  geom_edges(color = "grey50",curvature = 0.3) +
  geom_point(size = 3)+
  scale_color_discrete(type = as.vector(polychrome(length(unique(pheno)))))+
  theme_blank()+
  guides(col=guide_legend(title="Phenotype"))+
  theme(text = element_text(size = 25))

png(filename = "Clustering/Definitive_clustering/clus_sat_full/cor_09_phe_sat.png",width = 1125,height = 800)
cor.net.09
dev.off()

cor.net.09 <- ggplot(ok, aes(x = x, y = y, xend = xend, yend = yend,col = sapply(colnames(cor.sat)[ok$vertex.names],function(x) strsplit(x, "[.]")[[1]][2]))) +
  geom_edges(color = "grey50",curvature = 0.3) +
  geom_point(size = 3)+
  scale_color_discrete(type = as.vector(polychrome(length(unique(statistic)))))+
  theme_blank()+
  guides(col=guide_legend(title="Statistic"))+
  theme(text = element_text(size = 25))

png(filename = "Clustering/Definitive_clustering/clus_sat_full/cor_09_stat_sat.png",width = 1125,height = 800)
cor.net.09
dev.off()

cor.net.09 <- ggplot(ok, aes(x = x, y = y, xend = xend, yend = yend,col = as.factor(km.sat.full.12.sqrd.cor$cluster[ok$vertex.names]))) +
  geom_edges(color = "grey50",curvature = 0.3) +
  geom_point(size = 3)+
  scale_color_discrete(type = as.vector(polychrome(length(unique(km.sat.full.12.sqrd.cor$cluster)))))+
  theme_blank()+
  guides(col=guide_legend(title="Kmeans cluster"))+
  theme(text = element_text(size = 25))

png(filename = "Clustering/Definitive_clustering/clus_sat_full/cor_09_km_12_corsqrd_sat.png",width = 1125,height = 800)
cor.net.09
dev.off()

# cor.net.09 <- ggplot(ok, aes(x = x, y = y, xend = xend, yend = yend,col = as.factor(km.sat.full.16.sqrd.cor$cluster[ok$vertex.names]))) +
#   geom_edges(color = "grey50",curvature = 0.3) +
#   geom_point(size = 3)+
#   scale_color_discrete(type = as.vector(alphabet(length(unique(statistic)))))+
#   theme_blank()+
#   guides(col=guide_legend(title="Kmeans cluster"))+
#   theme(text = element_text(size = 25))
# 
# png(filename = "Clustering/Definitive_clustering/clus_sat_full/cor_09_km_12_corsqrd_sat.png",width = 1125,height = 800)
# cor.net.09
# dev.off()

km.clus <- match(plot.frame$Phenotype,gsub(".","_",names(km.sat.full.12.sqrd.cor$cluster),fixed = T))
plot.frame <- data.frame(plot.frame,cluster = factor(as.vector(km.sat.full.12.sqrd.cor$cluster[km.clus])))

### Manhattan plot ordered by kmeans clusters

load("GWAS_objects/obj_bgi.sat.snps.out")

snp.info <- sat.snps[,1:3]

sat.snps <- sat.snps[,-c(1,2,3)]

sat.snps <- sat.snps[,c(1:81,83:124,126:130)]

threshold <- round(ncol(sat.snps)*0.05, digits=0)
selc2 <- apply(sat.snps==1,1,sum) >= threshold & apply(sat.snps==3,1,sum) >= threshold #1 is wild type allele, 3 is alternative allele
sat.snps <- sat.snps[selc2,]
snp.info <-snp.info[selc2,]

sat.snps[sat.snps == 9] <- 2 

facet_bounds <- data.frame(chr = c(1,2,3,4,5,6,7,8,9),
                           xmin = c(0,0,0,0,0,0,0,0,0),
                           xmax= c(max(snp.info$POS[snp.info$CHR==1]),max(snp.info$POS[snp.info$CHR==2]),max(snp.info$POS[snp.info$CHR==3]),
                                   max(snp.info$POS[snp.info$CHR==4]),max(snp.info$POS[snp.info$CHR==5]),max(snp.info$POS[snp.info$CHR==6]),
                                   max(snp.info$POS[snp.info$CHR==7]),max(snp.info$POS[snp.info$CHR==8]),max(snp.info$POS[snp.info$CHR==9]))/10^7)

facet_hist <- facet_bounds[rep(seq_len(nrow(facet_bounds)), 12), ]
facet_hist <- data.frame(facet_hist,cluster = factor(c(rep(1,9),rep(2,9),rep(3,9),rep(4,9),rep(5,9),rep(6,9),rep(7,9),rep(8,9),rep(9,9),rep(10,9),rep(11,9),rep(12,9))))

ff.hist <- with(facet_hist,data.frame(Position=c(xmin,xmax),Chromosome=c(chr,chr),cluster =c(cluster,cluster)))

levels(plot.frame$cluster) <- as.character(1:12)

peak.plot <- ggplot(plot.frame,aes(x = Position,y = as.factor(Phenotype),col = cluster))+
  geom_point(size = 1) + 
  facet_grid(cluster~Chromosome,scales = "free",space = "free",drop = F)+
  scale_y_discrete(breaks = NULL) +
  geom_point(data=ff.hist,y=NA)+
  scale_x_continuous(expand = c(0.05,0.05),breaks = c(0,10,20,30)) +
  scale_color_discrete(type = as.vector(polychrome(length(unique(km.sat.full.12.sqrd.cor$cluster))+1)[-2])) +
  theme_minimal() +
  xlab("Position in Mb/10")+
  ylab("")+
  guides(col=guide_legend(title="Cluster")) +
  theme(axis.title.x =element_text(size = 25),axis.title.y = element_text(size = 30),axis.text.x = element_text(size = 20),panel.spacing.x = unit(1, "lines"),
        legend.text = element_text(size = 25),legend.title = element_text(size = 25),strip.text.x = element_blank(),panel.border = element_rect(fill = NA,color = "black"))

png(filename = "Clustering/Definitive_clustering/clus_sat_full/manhat_manhat_sativa_clus_km.png",width = 1000,height = 1000)
peak.plot
dev.off()


peak.plot <- ggplot(plot.frame,aes(x = Position,fill = cluster))+
  geom_histogram(size = 1,bins = 20) + 
  facet_grid(cluster~Chromosome,scales = "free_x",space = "free")+
  geom_histogram(data=ff.hist,y=NA)+
  scale_x_continuous(expand = c(0.05,0.05),breaks = c(0,10,20,30)) +
  scale_fill_discrete(type = as.vector(polychrome(length(unique(km.sat.full.12.sqrd.cor$cluster))))) +
  theme_minimal() +
  xlab("Position in Mb/10")+
  guides(fill=guide_legend(title="Cluster")) +
  theme(axis.title.x =element_text(size = 25),axis.title.y = element_text(size = 30),axis.text.x = element_text(size = 20),panel.spacing.x = unit(1, "lines"),
        legend.text = element_text(size = 25),legend.title = element_text(size = 25),strip.text.x = element_text(size = 25),panel.border = element_rect(fill = NA,color = "black"),
        strip.text.y = element_blank())

png(filename = "Clustering/Definitive_clustering/clus_sat_full/manhat_manhat_sativa_clus_km_hist.png",width = 1000,height = 1000)
peak.plot
dev.off()

### Plot distribution phenotypes over clusters

km.pheno.frame <- data.frame(Phenotype = names(table(factor(pheno[km.sat.full.12.sqrd.cor$cluster==1],levels = unique(pheno)))),Cluster1 = as.vector(table(factor(pheno[km.sat.full.12.sqrd.cor$cluster==1],levels = unique(pheno)))),Cluster2 = as.vector(table(factor(pheno[km.sat.full.12.sqrd.cor$cluster==2],levels = unique(pheno)))),
                             Cluster3 = as.vector(table(factor(pheno[km.sat.full.12.sqrd.cor$cluster==3],levels = unique(pheno)))),Cluster4 = as.vector(table(factor(pheno[km.sat.full.12.sqrd.cor$cluster==4],levels = unique(pheno)))),
                             Cluster5 = as.vector(table(factor(pheno[km.sat.full.12.sqrd.cor$cluster==5],levels = unique(pheno)))),Cluster6 = as.vector(table(factor(pheno[km.sat.full.12.sqrd.cor$cluster==6],levels = unique(pheno)))),
                             Cluster7 = as.vector(table(factor(pheno[km.sat.full.12.sqrd.cor$cluster==7],levels = unique(pheno)))),Cluster8 = as.vector(table(factor(pheno[km.sat.full.12.sqrd.cor$cluster==8],levels = unique(pheno)))),
                             Cluster9 = as.vector(table(factor(pheno[km.sat.full.12.sqrd.cor$cluster==9],levels = unique(pheno)))),Cluster10 = as.vector(table(factor(pheno[km.sat.full.12.sqrd.cor$cluster==10],levels = unique(pheno)))),
                             Cluster11 = as.vector(table(factor(pheno[km.sat.full.12.sqrd.cor$cluster==11],levels = unique(pheno)))),Cluster12 = as.vector(table(factor(pheno[km.sat.full.12.sqrd.cor$cluster==12],levels = unique(pheno)))))

km.pheno.frame.long <- pivot_longer(km.pheno.frame,cols = starts_with("Cluster"),names_to = "Cluster",values_to = "Count")
km.pheno.frame.long$Cluster <- factor(km.pheno.frame.long$Cluster,levels = c("Cluster1","Cluster2","Cluster3",
                                                                             "Cluster4","Cluster5","Cluster6"
                                                                             ,"Cluster7","Cluster8","Cluster9",
                                                                             "Cluster10","Cluster11","Cluster12"))

bar.pheno.plt <- ggplot(km.pheno.frame.long,aes(x = Cluster, y = Count, fill = Phenotype))+
  geom_bar(stat = "identity")+
  theme_minimal()+
  scale_fill_discrete(type = as.vector(polychrome(length(unique(pheno)))))+
  theme(text= element_text(size = 25),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

png(filename = "Clustering/Definitive_clustering/clus_sat_full/bar_pheno_corsqrd_km_12_sat_full.png",width = 750,height = 750)
bar.pheno.plt
dev.off()










