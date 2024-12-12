### Script to find all significant peaks consdiering only the mean traits.
# As shown in Supplemental_data.QTLs.fig5 and table 3
library(ggplot2)
library(openxlsx)
library(cowplot)
library(viridis)
library(stringr)

plot.frame <- read.xlsx(xlsxFile =  "D:/Drone-paper/Supplemental_data.xlsx", "pvalues")
plot.frame <- subset(plot.frame, select = -c(all_clustering))
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


#### Loop over all chromosomes and see where at least 3 SNPs meet the threshold
threshold = 8
peaks <- data.frame(matrix(nrow = 0 , ncol = 8))
for (chromosome in unique(plot.frame$Chromosome)){
  print(chromosome)
  subframe <- plot.frame[plot.frame$Chromosome == chromosome,]
  
  for (row in 1:nrow(subframe)){
    row <- subframe[row,]
    if (row$Pval > threshold){
      pos <- row$Position
      # Check if there are at least 2 SNPs with a value above 10 within 5 mbp
      window <- subframe[subframe$Position > pos -2.5 & subframe$Position < pos +2.5,]
      window <- window[window$Pval > threshold,]
      if (length(window$Phenotype) >= 3){
        max.pos <- window[window$Pval == max(window$Pval),]
        max.pos <- max.pos[1,] # In case of identical points
        pos <- max.pos$Position
        b.window <- subframe[subframe$Position > pos -10 & subframe$Position < pos +10,]
        b.window <- b.window[b.window$Pval > threshold,]
        l.bound <- b.window[b.window$Position == min(b.window$Position),][1,]$Position # lower bound
        u.bound <- b.window[b.window$Position == max(b.window$Position),][1,]$Position # upper bound
        # Now join peaks within each others lower and upper bounds
        window <- subframe[subframe$Position >= l.bound & # Draw new window to get all involved clusters
                             subframe$Position <= u.bound & 
                             subframe$Pval > threshold,]
        #clusters <- toString(unique(window$all_clustering))
        max.pos <- window[window$Pval == max(window$Pval),]
        max.pos <- max.pos[1,]
        pos <- max.pos$Position
        b.window <- subframe[subframe$Position > pos -10 & subframe$Position < pos +10,]
        b.window <- b.window[b.window$Pval > threshold,]
        l.bound <- b.window[b.window$Position == min(b.window$Position),][1,]$Position
        u.bound <- b.window[b.window$Position == max(b.window$Position),][1,]$Position
        # Create row to add to dataframe
        clusters <- toString(unique(b.window[b.window$Pval > threshold,]$mean_clustering))
        phenotypes <- toString(unique(b.window[b.window$Pval > threshold,]$Phenotype))
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
write.csv(peaks, "QTLsfig5.csv")
