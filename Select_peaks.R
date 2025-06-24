### Script to find all significant peaks considering both the mean traits and all extended descriptives
# As shown in Supplemental_data.QTLs.all and Supplemental figure 9
library(ggplot2)
library(openxlsx)
library(cowplot)
library(viridis)
library(stringr)
library(readxl)

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


#### Now do all chromosomes
threshold = 9
window = 2.5
half.window = window / 2
minima.traits = 5


peaks <- data.frame(matrix(nrow = 0 , ncol = 8))
for (chromosome in unique(plot.frame$Chromosome)){
  print(chromosome)
  subframe <- plot.frame[plot.frame$Chromosome == chromosome,]
  
  for (row in 1:nrow(subframe)){
    row <- subframe[row,]
    if (row$Pval >= threshold + 1){
      pos <- row$Position
      # Check if there are at least multiple traits in the window
      window <- subframe[subframe$Position > pos - half.window & subframe$Position < pos + half.window,]
      window <- window[window$Pval > threshold,]
      if (length(unique(window$Phenotype)) >= minima.traits) {
        max.pos <- window[window$Pval == max(window$Pval),]
        max.pos <- max.pos[1,] # In case of identical points
        pos <- max.pos$Position
        b.window <- subframe[subframe$Position > pos -10 & subframe$Position < pos +10,]
        b.window <- b.window[b.window$Pval >= threshold,]
        l.bound <- b.window[b.window$Position == min(b.window$Position),][1,]$Position # lower bound
        u.bound <- b.window[b.window$Position == max(b.window$Position),][1,]$Position # upper bound
        # The window for looking for lower and upper boundaries is larger than the initial window.
        # So in case we find a new most significant SNP here we run this code again with the new window.
        window <- subframe[subframe$Position >= l.bound & # Draw new window to get all involved clusters
                             subframe$Position <= u.bound & 
                             subframe$Pval > threshold,]
        max.pos <- window[window$Pval == max(window$Pval),]
        max.pos <- max.pos[1,]
        pos <- max.pos$Position
        b.window <- subframe[subframe$Position > pos -10 & subframe$Position < pos +10,]
        b.window <- b.window[b.window$Pval > threshold,]
        l.bound <- b.window[b.window$Position == min(b.window$Position),][1,]$Position
        u.bound <- b.window[b.window$Position == max(b.window$Position),][1,]$Position
        # Create row to add to dataframe
        clusters <- toString(unique(b.window[b.window$Pval > threshold,]$all_clustering))
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
View(peaks)
write.csv(peaks, "QTLsfigall.csv")
