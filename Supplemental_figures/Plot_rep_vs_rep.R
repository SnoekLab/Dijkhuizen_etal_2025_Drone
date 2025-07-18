# Script to show the homogeneity between the differen replicates

load("corrected_phenotypes/obj.phe.cor.sat_rep1.1106.R4.3.2.out")
load("corrected_phenotypes/obj.phe.cor.sat_rep2.1106.R4.3.2.out")
load("corrected_phenotypes/obj.phe.cor.sat_rep1.2506.R4.3.2.out")
load("corrected_phenotypes/obj.phe.cor.sat_rep2.2506.R4.3.2.out")

# Getting the data in the right format
rownames(phe.sat1.1106) <- phe.sat1.1106$accession
phe.sat1.1106$accession <- NULL
colnames(phe.sat1.1106) <- paste0(colnames(phe.sat1.1106), ".1106")

rownames(phe.sat2.1106) <- phe.sat2.1106$accession
phe.sat2.1106$accession <- NULL
colnames(phe.sat2.1106) <- paste0(colnames(phe.sat2.1106), ".1106")

rownames(phe.sat1.2506) <- phe.sat1.2506$accession
phe.sat1.2506$accession <- NULL
colnames(phe.sat1.2506) <- paste0(colnames(phe.sat1.2506), ".2506")

rownames(phe.sat2.2506) <- phe.sat2.2506$accession
phe.sat2.2506$accession <- NULL
colnames(phe.sat2.2506) <- paste0(colnames(phe.sat2.2506), ".2506")

sat.full.rep1 <- cbind(phe.sat1.1106, phe.sat1.2506)
sat.full.rep2 <- cbind(phe.sat2.1106, phe.sat2.2506)

sat.full.rep1$Accessions <- rownames(sat.full.rep1)
sat.full.rep2$Accessions <- rownames(sat.full.rep2)

sat.full.rep1$Replicate <- "Rep1"
sat.full.rep2$Replicate <- "Rep2"

sum(colnames(sat.full.rep1)==colnames(sat.full.rep2)) # Sanity check

sat.full <- data.frame(rbind(sat.full.rep1,sat.full.rep2))

# Now that the data is in the right format. Let's make the plot
library(tidyr)
library(ggplot2)
library(patchwork)

sat.long <- pivot_longer(sat.full,colnames(sat.full)[!colnames(sat.full)%in%c("Accessions","Replicate")])
sat.long <- pivot_wider(sat.long,names_from = c("Replicate"),values_from = value)

trait <- as.vector(sapply(sat.long$name,function(x)strsplit(x,".",fixed =T)[[1]][1]))
statistic <- as.vector(sapply(sat.long$name,function(x)strsplit(x,".",fixed =T)[[1]][2]))
day <- as.vector(sapply(sat.long$name,function(x)strsplit(x,".",fixed =T)[[1]][3]))

sat.long$Trait <- trait
sat.long$Statistic <- statistic
sat.long$Day <- day

sel.trait <- c(paste0(rep("msp",5),1:5),"blue","red","green","height")

sat.long <- sat.long[sat.long$Trait%in%sel.trait,]

sat.long.mean <- sat.long[sat.long$Statistic=="mean",]

mean.plt <- ggplot(sat.long.mean,aes(x = Rep1,y = Rep2))+
  geom_point(size = 2)+
  facet_wrap(Trait~Day,scales = "free",ncol = 2)+
  theme_bw()


mean.plt

# Some changes to improve the plot visually
desired_order <- c("blue", "green", "red", "height", "msp1", "msp2", "msp3", "msp4", "msp5")
sat.long.mean$Trait <- factor(sat.long.mean$Trait, levels = desired_order)

sat.long.mean$Day[sat.long.mean$Day == "1106"] <- "day-78"
sat.long.mean$Day[sat.long.mean$Day == "2506"] <- "day-93"

# Subset data
sat_78 <- subset(sat.long.mean, Day == "day-78")
sat_93 <- subset(sat.long.mean, Day == "day-93")

# Create plots without the day in facet labels
plt_78 <- ggplot(sat_78, aes(x = Rep1, y = Rep2)) +
  geom_point(size = 2) +
  facet_wrap(~Trait, scales = "free", ncol = 1) +
  theme_bw() +
  ggtitle("day-78")

plt_93 <- ggplot(sat_93, aes(x = Rep1, y = Rep2)) +
  geom_point(size = 2) +
  facet_wrap(~Trait, scales = "free", ncol = 1) +
  theme_bw() +
  ggtitle("day-93")

# Combine plots side by side
mean.plt <- plt_78 + plt_93 + plot_layout(ncol = 2)
mean.plt


png(filename = "rep1_vs_rep2_mean.png",width = 500,height = 2000)
mean.plt
dev.off()











