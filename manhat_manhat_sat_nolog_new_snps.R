.libPaths("~/R/x86_64-pc-linux-gnu-library/3.6")

library(ggplot2)
library(stringr)
library(withr)
library(viridis)

load("GWAS_objects/obj_all.ALTREF.out")
load("~/Documents/lettuce/GWAS_objects/sat.accessions.out")

sat.snps <- all.ALTREF

rm(all.ALTREF)

snp.info <- sat.snps[,1:10]

sat.snps <- sat.snps[,-c(1:10)]

sat.snps <- sat.snps[,colnames(sat.snps)%in%accessions]

sat.snps[sat.snps == 9] <- 1

max.snps <- c()

pheno <- c()

pvalues <- c()

for(i in list.files("/hosts/linuxhome/mutant14/tmp/Bramve/GWAS_sat/frames",full.names = T)){
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
      snp.nums[j] <- which(snp.info$CHR == as.character(mm.snps[j,1]) & snp.info$POS == snp.info[snp.info$POS%in%c(as.integer(as.character(plot_frame$pos[pval == mm.snps[j,3]]*10^7))),7][snp.cor])
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
      snp.nums[j] <- which(snp.info$CHR == as.character(mm.snps[1]) & snp.info$POS == snp.info[snp.info$POS%in%c(as.integer(as.character(plot_frame$pos[pval == mm.snps[3]]*10^7))),7][snp.cor])
    }
    
    max.snps <- c(max.snps,snp.nums)
    pheno <- c(pheno,phe)
    pvalues <- c(pvalues,mm.snps[3])
  }
}

pos <- snp.info[max.snps,7]
chr <- snp.info[max.snps,1]

plot.frame <- data.frame(Phenotype = pheno,Chromosome = chr,Position = pos/10^7,Pval=pvalues)

save(plot.frame,file = "Clustering/cluster_new_snps/manhat.manhat.nolog.sat.new.snps.out")
write.csv(plot.frame,file = "Clustering/cluster_new_snps/peaks_sativa_nolog_new_snps.csv")

