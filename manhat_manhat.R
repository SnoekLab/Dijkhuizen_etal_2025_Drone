.libPaths("~/R/x86_64-pc-linux-gnu-library/3.6")

library(ggplot2)
library(stringr)
library(withr)
library(viridis)

load("GWAS_objects/obj_bgi.sat.snps.out")

snp.info <- sat.snps[,1:3]

sat.snps <- sat.snps[,-c(1,2,3)]

sat.snps <- sat.snps[,c(1:81,83:124,126:130)]

threshold <- round(ncol(sat.snps)*0.05, digits=0)
selc2 <- apply(sat.snps==1,1,sum) >= threshold & apply(sat.snps==3,1,sum) >= threshold #1 is wild type allele, 3 is alternative allele
sat.snps <- sat.snps[selc2,]
snp.info <-snp.info[selc2,]

sat.snps[sat.snps == 9] <- 2 

max.snps <- c()

pheno <- c()

pvalues <- c()

for(i in list.files("/hosts/linuxhome/mutant16/tmp/Bramve/GWAS_sat/frames",full.names = T)){
  load(i)
  phe <- str_extract(i,"(?<=/frames/)(.*)(?=[.])")
  
  pval <- -log10(plot_frame$pval)
  chr <- as.numeric(plot_frame$chr)
  pos <- as.numeric(plot_frame$pos)
  
  to.pl <- data.frame(chr,pos,pval)
  
  maxpmil <- as.matrix(aggregate(to.pl$pval,list(to.pl$chr,as.numeric(cut(to.pl$pos,400))),max))
  
  
  if(sum(maxpmil[,3]>7)>1){
    mm.snps <- maxpmil[maxpmil[,3]>7,]
    
    snp.nums <- NULL
    
    for (j in 1:nrow(mm.snps)){
      snp.cor <- which(snp.info[snp.info$POS%in%c(as.integer(as.character(plot_frame$pos[pval == mm.snps[j,3]]*10^7))),1]==mm.snps[j,1])[1]
      snp.nums[j] <- which(snp.info$CHR == as.character(mm.snps[j,1]) & snp.info$POS == snp.info[snp.info$POS%in%c(as.integer(as.character(plot_frame$pos[pval == mm.snps[j,3]]*10^7))),2][snp.cor])
    }
    
    max.snps <- c(max.snps,snp.nums)
    pheno <- c(pheno,rep(phe,sum(maxpmil[,3]>7)))
    pvalues <- c(pvalues,mm.snps[,3])
  }
  
  if(sum(maxpmil[,3]>7)==1){
    mm.snps <- maxpmil[maxpmil[,3]>7,]
    
    snp.nums <- NULL
    
    for (j in c(1)){
      snp.cor <- which(snp.info[snp.info$POS%in%c(as.integer(as.character(plot_frame$pos[pval == mm.snps[3]]*10^7))),1]==mm.snps[1])[1]
      snp.nums[j] <- which(snp.info$CHR == as.character(mm.snps[1]) & snp.info$POS == snp.info[snp.info$POS%in%c(as.integer(as.character(plot_frame$pos[pval == mm.snps[3]]*10^7))),2][snp.cor])
    }
    
    max.snps <- c(max.snps,snp.nums)
    pheno <- c(pheno,phe)
    pvalues <- c(pvalues,mm.snps[3])
  }
}

pos <- snp.info[max.snps,2]
chr <- snp.info[max.snps,1]

plot.frame <- data.frame(Phenotype = pheno,Chromosome = chr,Position = pos/10^7,Pval=pvalues)
save(plot.frame,file = "manhat.manhat.nolog.sat.out")
write.csv(plot.frame,file = "peaks_sativa_log.csv",append = F)

facet_bounds <- data.frame(chr = c(1,2,3,4,5,6,7,8,9),
                           xmin = c(0,0,0,0,0,0,0,0,0),
                           xmax= c(max(snp.info$POS[snp.info$CHR==1]),max(snp.info$POS[snp.info$CHR==2]),max(snp.info$POS[snp.info$CHR==3]),
                                   max(snp.info$POS[snp.info$CHR==4]),max(snp.info$POS[snp.info$CHR==5]),max(snp.info$POS[snp.info$CHR==6]),
                                   max(snp.info$POS[snp.info$CHR==7]),max(snp.info$POS[snp.info$CHR==8]),max(snp.info$POS[snp.info$CHR==9]))/10^7)

ff <- with(facet_bounds,data.frame(Position=c(xmin,xmax),Chromosome=c(chr,chr)))
ff <- cbind(ff,Phenotype = rep(plot.frame$Phenotype[1],18))

peak.plot <- ggplot(plot.frame,aes(x = Position,y = as.factor(Phenotype),col = as.factor(Chromosome)))+
  geom_point(size = 1) + 
  facet_grid(~Chromosome,scales = "free_x",space = "free_x")+
  geom_point(data=ff,y=NA)+
  scale_x_continuous(expand = c(0.05,0.05),breaks = c(0,10,20,30)) +
  scale_y_discrete(breaks = NULL) +
  scale_color_viridis(discrete = T,option = "H") +
  theme_minimal() +
  xlab("Position in Mb/10")+
  ylab("Phenotype")+
  guides(col=guide_legend(title="Chr")) +
  theme(axis.title.x =element_text(size = 25),axis.title.y = element_text(size = 30),axis.text.x = element_text(size = 20),panel.spacing.x = unit(1, "lines"),
        legend.text = element_text(size = 25),legend.title = element_text(size = 25),strip.text.x = element_blank(),panel.border = element_rect(fill = NA,color = "black"))

png(filename = "manhat_manhat_sativa_log.png",width = 1000,height = 1000)
peak.plot
dev.off()

sim.mat <- matrix(0,length(unique(plot.frame$Phenotype)),length(unique(plot.frame$Phenotype)))
for(i in 1:nrow(sim.mat)){
  for(j in 1:nrow(sim.mat)){
    phe.i <- plot.frame[plot.frame$Phenotype==unique(plot.frame$Phenotype)[i],]
    phe.j <- plot.frame[plot.frame$Phenotype==unique(plot.frame$Phenotype)[j],]
    for(k in 1:nrow(phe.i)){
      k.chr <- phe.i[k,2]
      j.chr <- phe.j[phe.j$Chromosome==k.chr,]
      if(any(abs(j.chr$Position-phe.i[k,3])<0.5)){
        sim.mat[i,j]<-sim.mat[i,j]+1
      }
    }
  }
}

dist.mat <- dist(sim.mat,method ="euclidean")

clus.phe <- hclust(dist.mat,method = "average")
save(clus.phe,file = "clus_peaks_sat_log.out")

clus.phe$order

plot.frame.clus <- plot.frame
plot.frame.clus$Phenotype <- factor(plot.frame.clus$Phenotype,levels = unique(plot.frame.clus$Phenotype)[clus.phe$order])

peak.plot <- ggplot(plot.frame.clus,aes(x = Position,y = Phenotype,col = as.factor(Chromosome)))+
  geom_point(size = 1) + 
  facet_grid(~Chromosome,scales = "free_x",space = "free_x")+
  geom_point(data=ff,y=NA)+
  scale_x_continuous(expand = c(0.05,0.05),breaks = c(0,10,20,30)) +
  #scale_y_discrete(breaks =levels(as.factor(plot.frame$Phenotype))[c(T, rep(F, 15))]) +
  scale_y_discrete(breaks = NULL) +
  scale_color_viridis(discrete = T,option = "H") +
  theme_minimal() +
  xlab("Position in Mb/10")+
  ylab("Phenotype")+
  guides(col=guide_legend(title="Chr")) +
  theme(axis.title.x =element_text(size = 25),axis.title.y = element_text(size = 30),axis.text.x = element_text(size = 20),panel.spacing.x = unit(1, "lines"),
        legend.text = element_text(size = 25),legend.title = element_text(size = 25),strip.text.x = element_blank(),panel.border = element_rect(fill = NA,color = "black"))

png(filename = "manhat_manhat_sativa_log_clus.png",width = 1000,height = 1000)
peak.plot
dev.off()

load("GWAS_objects/obj_bgi.ser.snps.out")

snp.info <- ser.snps[,1:3]

ser.snps <- ser.snps[,-c(1,2,3)]

threshold <- round(ncol(ser.snps)*0.05, digits=0)
selc2 <- apply(ser.snps==1,1,sum) >= threshold & apply(ser.snps==3,1,sum) >= threshold #1 is wild type allele, 3 is alternative allele
ser.snps <- ser.snps[selc2,]
snp.info <-snp.info[selc2,]

ser.snps[ser.snps == 9] <- 2 

max.snps <- c()

pheno <- c()

pvalues <- c()

for(i in list.files("/hosts/linuxhome/mutant3/tmp/Bramve/GWAS_ser_log/frames",full.names = T)){
  load(i)
  phe <- str_extract(i,"(?<=/frames/)(.*)(?=[.])")
  
  pval <- -log10(plot_frame$pval)
  chr <- as.numeric(plot_frame$chr)
  pos <- as.numeric(plot_frame$pos)
  
  to.pl <- data.frame(chr,pos,pval)
  
  maxpmil <- as.matrix(aggregate(to.pl$pval,list(to.pl$chr,as.numeric(cut(to.pl$pos,400))),max))
  
  
  if(sum(maxpmil[,3]>7)>1){
    mm.snps <- maxpmil[maxpmil[,3]>7,]
    
    snp.nums <- NULL
    
    for (j in 1:nrow(mm.snps)){
      snp.cor <- which(snp.info[snp.info$POS%in%c(as.integer(as.character(plot_frame$pos[pval == mm.snps[j,3]]*10^7))),1]==mm.snps[j,1])[1]
      snp.nums[j] <- which(snp.info$CHR == as.character(mm.snps[j,1]) & snp.info$POS == snp.info[snp.info$POS%in%c(as.integer(as.character(plot_frame$pos[pval == mm.snps[j,3]]*10^7))),2][snp.cor])
    }
    
    max.snps <- c(max.snps,snp.nums)
    pheno <- c(pheno,rep(phe,sum(maxpmil[,3]>7)))
    pvalues <- c(pvalues,mm.snps[,3])
  }
  
  if(sum(maxpmil[,3]>7)==1){
    mm.snps <- maxpmil[maxpmil[,3]>7,]
    
    snp.nums <- NULL
    
    for (j in c(1)){
      snp.cor <- which(snp.info[snp.info$POS%in%c(as.integer(as.character(plot_frame$pos[pval == mm.snps[3]]*10^7))),1]==mm.snps[1])[1]
      snp.nums[j] <- which(snp.info$CHR == as.character(mm.snps[1]) & snp.info$POS == snp.info[snp.info$POS%in%c(as.integer(as.character(plot_frame$pos[pval == mm.snps[3]]*10^7))),2][snp.cor])
    }
    
    max.snps <- c(max.snps,snp.nums)
    pheno <- c(pheno,phe)
    pvalues <- c(pvalues,mm.snps[3])
  }
}

max.snps
pos <- snp.info[max.snps,2]
chr <- snp.info[max.snps,1]

pheno

plot.frame <- data.frame(Phenotype = pheno,Chromosome = chr,Position = pos/10^7,Pval=pvalues)
write.csv(plot.frame,file = "peaks_serriola_log.csv",append = F)

facet_bounds <- data.frame(chr = c(1,2,3,4,5,6,7,8,9),
                           xmin = c(0,0,0,0,0,0,0,0,0),
                           xmax= c(max(snp.info$POS[snp.info$CHR==1]),max(snp.info$POS[snp.info$CHR==2]),max(snp.info$POS[snp.info$CHR==3]),
                                   max(snp.info$POS[snp.info$CHR==4]),max(snp.info$POS[snp.info$CHR==5]),max(snp.info$POS[snp.info$CHR==6]),
                                   max(snp.info$POS[snp.info$CHR==7]),max(snp.info$POS[snp.info$CHR==8]),max(snp.info$POS[snp.info$CHR==9]))/10^7)

ff <- with(facet_bounds,data.frame(Position=c(xmin,xmax),Chromosome=c(chr,chr)))
ff <- cbind(ff,Phenotype = rep(plot.frame$Phenotype[1],18))

peak.plot <- ggplot(plot.frame,aes(x = Position,y = as.factor(Phenotype),col = as.factor(Chromosome)))+
  geom_point(size = 1) + 
  facet_grid(~Chromosome,scales = "free_x",space = "free_x")+
  geom_point(data=ff,y=NA)+
  scale_x_continuous(expand = c(0.05,0.05),breaks = c(0,10,20,30)) +
  #scale_y_discrete(breaks =levels(as.factor(plot.frame$Phenotype))[c(T, rep(F, 15))]) +
  scale_y_discrete(breaks = NULL) +
  scale_color_viridis(discrete = T,option = "H") +
  theme_minimal() +
  xlab("Position in Mb/10")+
  ylab("Phenotype")+
  guides(col=guide_legend(title="Chr")) +
  theme(axis.title.x =element_text(size = 25),axis.title.y = element_text(size = 30),axis.text.x = element_text(size = 20),panel.spacing.x = unit(1, "lines"),
        legend.text = element_text(size = 25),legend.title = element_text(size = 25),strip.text.x = element_blank(),panel.border = element_rect(fill = NA,color = "black"))

png(filename = "manhat_manhat_serriola_log.png",width = 1000,height = 1000)
peak.plot
dev.off()

sim.mat <- matrix(0,length(unique(plot.frame$Phenotype)),length(unique(plot.frame$Phenotype)))
for(i in 1:nrow(sim.mat)){
  for(j in 1:nrow(sim.mat)){
    phe.i <- plot.frame[plot.frame$Phenotype==unique(plot.frame$Phenotype)[i],]
    phe.j <- plot.frame[plot.frame$Phenotype==unique(plot.frame$Phenotype)[j],]
    for(k in 1:nrow(phe.i)){
      k.chr <- phe.i[k,2]
      j.chr <- phe.j[phe.j$Chromosome==k.chr,]
      if(any(abs(j.chr$Position-phe.i[k,3])<0.5)){
        sim.mat[i,j]<-sim.mat[i,j]+1
      }
    }
  }
}

dist.mat <- dist(sim.mat,method ="euclidean")

clus.phe <- hclust(dist.mat,method = "average")
save(clus.phe,file = "clus_peaks_ser_log.out")

clus.phe$order

plot.frame.clus <- plot.frame
plot.frame.clus$Phenotype <- factor(plot.frame.clus$Phenotype,levels = unique(plot.frame.clus$Phenotype)[clus.phe$order])

peak.plot <- ggplot(plot.frame.clus,aes(x = Position,y = Phenotype,col = as.factor(Chromosome)))+
  geom_point(size = 1) + 
  facet_grid(~Chromosome,scales = "free_x",space = "free_x")+
  geom_point(data=ff,y=NA)+
  scale_x_continuous(expand = c(0.05,0.05),breaks = c(0,10,20,30)) +
  #scale_y_discrete(breaks =levels(as.factor(plot.frame$Phenotype))[c(T, rep(F, 15))]) +
  scale_y_discrete(breaks = NULL) +
  scale_color_viridis(discrete = T,option = "H") +
  theme_minimal() +
  xlab("Position in Mb/10")+
  ylab("Phenotype")+
  guides(col=guide_legend(title="Chr")) +
  theme(axis.title.x =element_text(size = 25),axis.title.y = element_text(size = 30),axis.text.x = element_text(size = 20),panel.spacing.x = unit(1, "lines"),
        legend.text = element_text(size = 25),legend.title = element_text(size = 25),strip.text.x = element_blank(),panel.border = element_rect(fill = NA,color = "black"))

png(filename = "manhat_manhat_serriola_log_clus.png",width = 1000,height = 1000)
peak.plot
dev.off()




