




load(file="obj_new_grouped_snps_chr1_2023.out")
new.snps <- new.snps[,-ncol(new.snps)]

new.snps[1:50,1:15]
dim(new.snps)

sum(new.snps$Count9==0) ## 122048

sum(new.snps$Count9>9) ## 2049134
sum(new.snps$Count9>9 & new.snps$Count9<189) ## 2049134

sum(new.snps$Count2>9) ## 1066758
sum(new.snps$Count0>9) ## 2261910


### Snps with at least 10 REF and 10 ALT and no more than 10 ABS <-------------------------------------------
sum(new.snps$Count0 > 9 & new.snps$Count2 >9 & new.snps$Count9 <11) # 323927

#### loop and collect 

load(file="obj_new_grouped_snps_chr1_2023.out")
if (ncol(new.snps)==208){new.snps <- new.snps[,-ncol(new.snps)]}
selc <- new.snps$Count0 > 9 & new.snps$Count2 >9 & new.snps$Count9 <11
CHR <- 1
chr1.snps <- data.frame(CHR,new.snps[selc,])
gc()

load(file="obj_new_grouped_snps_chr2_2023.out")
if (ncol(new.snps)==208){new.snps <- new.snps[,-ncol(new.snps)]}
selc <- new.snps$Count0 > 9 & new.snps$Count2 >9 & new.snps$Count9 <11
CHR <- 2
chr2.snps <- data.frame(CHR,new.snps[selc,])
gc()

load(file="obj_new_grouped_snps_chr3_2023.out")
if (ncol(new.snps)==208){new.snps <- new.snps[,-ncol(new.snps)]}
selc <- new.snps$Count0 > 9 & new.snps$Count2 >9 & new.snps$Count9 <11
CHR <- 3
chr3.snps <- data.frame(CHR,new.snps[selc,])
gc()

load(file="obj_new_grouped_snps_chr4_2023.out")
if (ncol(new.snps)==208){new.snps <- new.snps[,-ncol(new.snps)]}
selc <- new.snps$Count0 > 9 & new.snps$Count2 >9 & new.snps$Count9 <11
CHR <- 4
chr4.snps <- data.frame(CHR,new.snps[selc,])
gc()

load(file="obj_new_grouped_snps_chr5_2023.out")
if (ncol(new.snps)==208){new.snps <- new.snps[,-ncol(new.snps)]}
selc <- new.snps$Count0 > 9 & new.snps$Count2 >9 & new.snps$Count9 <11
CHR <- 5
chr5.snps <- data.frame(CHR,new.snps[selc,])
gc()

load(file="obj_new_grouped_snps_chr6_2023.out")
if (ncol(new.snps)==208){new.snps <- new.snps[,-ncol(new.snps)]}
selc <- new.snps$Count0 > 9 & new.snps$Count2 >9 & new.snps$Count9 <11
CHR <- 6
chr6.snps <- data.frame(CHR,new.snps[selc,])
gc()

load(file="obj_new_grouped_snps_chr7_2023.out")
if (ncol(new.snps)==208){new.snps <- new.snps[,-ncol(new.snps)]}
selc <- new.snps$Count0 > 9 & new.snps$Count2 >9 & new.snps$Count9 <11
CHR <- 7
chr7.snps <- data.frame(CHR,new.snps[selc,])
gc()

load(file="obj_new_grouped_snps_chr8_2023.out")
if (ncol(new.snps)==208){new.snps <- new.snps[,-ncol(new.snps)]}
selc <- new.snps$Count0 > 9 & new.snps$Count2 >9 & new.snps$Count9 <11
CHR <- 8
chr8.snps <- data.frame(CHR,new.snps[selc,])
gc()

load(file="obj_new_grouped_snps_chr9_2023.out")
if (ncol(new.snps)==208){new.snps <- new.snps[,-ncol(new.snps)]}
selc <- new.snps$Count0 > 9 & new.snps$Count2 >9 & new.snps$Count9 <11
CHR <- 9
chr9.snps <- data.frame(CHR,new.snps[selc,])
gc()


all.ALTREF <- rbind(chr1.snps,chr2.snps,chr3.snps,chr4.snps,chr5.snps,chr6.snps,chr7.snps,chr8.snps,chr9.snps)
dim(all.ALTREF) # [1] 2 491 009     208
# save(all.ALTREF,file="obj_all.ALTREF.out")


#### a bit of an overview

## gw distribution

library(ggplot2)
colnames(all.ALTREF)[1:15]
ggplot(all.ALTREF,aes(POS))+
  geom_histogram(binwidth = 1e6)+
  facet_grid(.~CHR,space="free_x",scale="free_x")









############################ END ############################################################