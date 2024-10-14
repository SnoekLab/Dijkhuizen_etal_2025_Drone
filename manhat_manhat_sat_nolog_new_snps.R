#This script reads in all individual results and combines them into one object

library(ggplot2)
library(stringr)
library(withr)
library(viridis)

load("GWAS_objects/obj_all.ALTREF.out")
#Instead of loading this file here we manually code what accession numbers we use.
#load("GWAS_objects/sat.accessions.out")
accessions <- c("LK001", "LK002", "LK003", "LK004", "LK005", "LK006", "LK007", "LK008",
                "LK009", "LK010", "LK011", "LK012", "LK013", "LK014", "LK015", "LK016",
                "LK017", "LK018", "LK019", "LK020", "LK021", "LK022", "LK023", "LK024",
                "LK025", "LK026", "LK027", "LK028", "LK029", "LK030", "LK031", "LK032",
                "LK033", "LK034", "LK035", "LK036", "LK037", "LK038", "LK039", "LK040",
                "LK041", "LK042", "LK043", "LK044", "LK045", "LK046", "LK047", "LK048",
                "LK049", "LK050", "LK051", "LK052", "LK053", "LK054", "LK055", "LK056",
                "LK057", "LK058", "LK059", "LK060", "LK061", "LK062", "LK063", "LK064",
                "LK065", "LK066", "LK067", "LK068", "LK069", "LK070", "LK071", "LK072",
                "LK073", "LK074", "LK075", "LK076", "LK077", "LK078", "LK079", "LK080",
                "LK081", "LK082", "LK083", "LK084", "LK086", "LK087", "LK088", "LK089",
                "LK091", "LK092", "LK093", "LK094", "LK095", "LK097", "LK098", "LK099",
                "LK100", "LK101", "LK102", "LK103", "LK104", "LK105", "LK106", "LK107",
                "LK108", "LK109", "LK110", "LK111", "LK112", "LK113", "LK114", "LK115",
                "LK116", "LK117", "LK118", "LK119", "LK120", "LK121", "LK122", "LK123",
                "LK124", "LK125", "LK126", "LK127", "LK128", "LK129", "LK130", "LK131",
                "LK132", "LK133", "LK134", "LK135", "LK136", "LK137", "LK138", "LK139",
                "LK140", "LK141", "LK142", "LK143", "LK144", "LK145", "LK146", "LK147",
                "LK148", "LK149", "LK150", "LK151", "LK152", "LK153", "LK154", "LK155",
                "LK156", "LK157", "LK158", "LK159", "LK160", "LK161", "LK162", "LK163",
                "LK164", "LK165", "LK166", "LK168", "LK169", "LK170", "LK171", "LK172",
                "LK173", "LK174", "LK175", "LK176", "LK177", "LK178", "LK179", "LK180",
                "LK181", "LK182", "LK183", "LK184", "LK185", "LK186", "LK187", "LK189",
                "LK191", "LK192", "LK193", "LK194", "LK195", "LK196", "LK197", "LK198",
                "LK199", "LK200")

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
#write.csv(plot.frame,file = "Clustering/cluster_new_snps/peaks_sativa_nolog_new_snps.csv")

