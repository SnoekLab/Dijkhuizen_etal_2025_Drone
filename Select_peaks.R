### Script to find all significant peaks consdiering both the mean traits and all extended descriptives
# As shown in Supplemental_data.QTLs.all and Supplemental figure 8
library(openxlsx)
library(cowplot)
library(viridis)
library(stringr)

plot.frame <- read.xlsx(xlsxFile =  "D:/Drone-paper/Supplemental_data.xlsx", "pvalues")
plot.frame <- subset(plot.frame, select = -c(mean_clustering))
plot.frame <- na.omit(plot.frame)
head(plot.frame)

### INCLUDE DAY and diff/dira as column 
obs <- rep(NA,nrow(plot.frame))
obs[grepl("1106",plot.frame$Phenotype)] <- "Day1"
obs[grepl("2506",plot.frame$Phenotype)] <- "Day2"
obs[grepl("dira",plot.frame$Phenotype)] <- "Ratio Day1/Day2"
obs[grepl("diff",plot.frame$Phenotype)] <- "Diff"
plot.frame$obs <- obs
unique(plot.frame$obs)

#### Loop over all chromosomes and see if there are at least 3 SNPs with unique phenotypes above the threshold.
peaks <- data.frame(matrix(nrow = 0 , ncol = 8))
for (chromosome in unique(plot.frame$Chromosome)){
  print(chromosome)
  subframe <- plot.frame[plot.frame$Chromosome == chromosome,]
  
  for (row in 1:nrow(subframe)){
    row <- subframe[row,]
    if (row$Pval > 12){
      pos <- row$Position
      # Check if there are at least 3 SNPs with a value above 12 within 5 mbp
      window <- subframe[subframe$Position > pos -2.5 & subframe$Position < pos +2.5,]
      window <- window[window$Pval > 11,]
      if (length(unique(window$Phenotype)) >= 3){
        max.pos <- window[window$Pval == max(window$Pval),]
        max.pos <- max.pos[1,] # In case of identical points
        pos <- max.pos$Position
        b.window <- subframe[subframe$Position > pos -10 & subframe$Position < pos +10,]
        b.window <- b.window[b.window$Pval > 11,]
        l.bound <- b.window[b.window$Position == min(b.window$Position),][1,]$Position # lower bound
        u.bound <- b.window[b.window$Position == max(b.window$Position),][1,]$Position # upper bound
        # Now join peaks within each others lower and upper bounds
        window <- subframe[subframe$Position >= l.bound & # Draw new window to get all involved clusters
                             subframe$Position <= u.bound & 
                             subframe$Pval > 12,]
        #clusters <- toString(unique(window$all_clustering))
        max.pos <- window[window$Pval == max(window$Pval),]
        max.pos <- max.pos[1,]
        pos <- max.pos$Position
        b.window <- subframe[subframe$Position > pos -10 & subframe$Position < pos +10,]
        b.window <- b.window[b.window$Pval > 11,]
        l.bound <- b.window[b.window$Position == min(b.window$Position),][1,]$Position
        u.bound <- b.window[b.window$Position == max(b.window$Position),][1,]$Position
        # Create row to add to dataframe
        clusters <- toString(unique(b.window[b.window$Pval > 12,]$all_clustering))
        phenotypes <- toString(unique(b.window[b.window$Pval > 11,]$Phenotype))
        phenotype <- max.pos$Phenotype
        Pval <- max.pos$Pval
        Chromosome <- max.pos$Chromosome
        Position <- max.pos$Position
        locus <- c(phenotype,phenotypes, Chromosome, Position, Pval, l.bound, u.bound, clusters)
        
        peaks <- rbind(peaks, locus)
      }
    }
  }
}
peaks <- peaks[!duplicated(peaks),]
colnames(peaks) <- c("Phenotype", "Phenotypes", "Chromosome", "Position", 
                     "Pval", "l.bound", "u.bound", "clusters")
peaks$Position <- as.numeric(peaks$Position)

# If you want to remove or add any loci before drawing rectangles this is the time to do it
write.csv(peaks, "QTLsall.csv")
