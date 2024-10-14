### In this code I will correct the height of the plants based on differences in soil height, which are not constant
### over the entire field.

library(ggplot2)
library(openxlsx)

###
# First we show why we need to correct for soil height.
# This part is optional when running this script.
###

# Load in phenotypes soil and plants.
load(file = "Phenotypes_per_plot/obj_phe.sat1.1106.out")
load(file = "phenotypes_soil/obj_phe.sat1.1106.soil.out")

height_frame <- data.frame(Accession = 1:194,Height = phe.sat1.1106.soil$height.mean)

height_plot <- ggplot(height_frame, aes(x = Accession,y = Height))+
  geom_point() +
  geom_smooth(col = "black",method = "gam") +
  theme_minimal()+
  theme(text = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  xlab("Accession (LK nr)")+
  ylab("Mean height soil")+
  xlim(0,194)

height_plot

height_frame <- data.frame(Accession = 1:194,Height = phe.sat1.1106$height.mean)

height_plot <- ggplot(height_frame, aes(x = Accession,y = Height))+
  geom_point() +
  geom_smooth(col = "black",method = "gam") +
  theme_minimal()+
  theme(text = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  xlab("Accession (LK nr)")+
  ylab("Mean height plant")+
  xlim(0,194)

height_plot

###
# Clearly, the height of the soil depends on the accession 
#(because they were grown in different parts of the field). 
# Having shown that we need to correct for soil height, let's do this now. Simply substracting the soil height
# from the plant height and than recalculating the summary statistics should do the trick.
# Optional part over
###

# Function to extract phenotypes
phe.extr <- function(phe.raw){
  phe.mean <- mean(phe.raw,na.rm = T)
  phe.mean.tr5 <- mean(phe.raw,na.rm = T,trim = 0.05)
  phe.mean.tr10 <- mean(phe.raw,na.rm = T,trim = 0.1)
  phe.mean.tr40 <- mean(phe.raw,na.rm = T,trim = 0.4)
  phe.median <- median(phe.raw, na.rm = T)
  phe.quan <- quantile(phe.raw,c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),na.rm = T)
  phe.sd <- sd(phe.raw,na.rm = T)
  phe.var <- var(phe.raw,na.rm = T)
  third_cm <- sum((phe.raw-phe.mean)^3,na.rm = T)/length(phe.raw)
  second_cm <- sum((phe.raw-phe.mean)^2,na.rm = T)/length(phe.raw)
  fourth_cm <- sum((phe.raw-phe.mean)^4,na.rm = T)/length(phe.raw)
  skewness <- third_cm/(second_cm^1.5)
  kurtosis <- fourth_cm/(second_cm^2)
  minimum <- min(phe.raw,na.rm = T)
  maximum <- max(phe.raw,na.rm = T)
  output <- c(phe.mean,phe.mean.tr5,phe.mean.tr10,phe.mean.tr40,phe.median,phe.quan,phe.sd,skewness,kurtosis,minimum,maximum)
  names(output) <- c("mean","trimmed_mean_5","trimmed_mean_10","trimmed mean 40","median","Q05","Q10","Q25","Q50","Q75","Q90","Q95","SD","skewness","kurtosis","minimum","maximum")
  return(output)
}

### Sativa 1106. We correct for soil height by substracting the mean soil height of the plot from each pixel.
### In this way, we do not correct for differences in soil within individual plots. 

load(file = "R_objects_plots/obj_all.pl_sat_rep1_0611_rgb_dsm_msp.out")
all.pl.rep1 <- all.pl
rm(all.pl)
load(file = "R_objects_plots/obj_all.pl_sat_rep2_0611_rgb_dsm_msp.out")
all.pl.rep2 <- all.pl
rm(all.pl)

# Here we obtain a logical with TRUE if EVI > 0.25 (plant pixels) and FALSE otherwise.
evi.selc.rep1 <- 2.5*((all.pl.rep1$msp5-all.pl.rep1$msp3)/(all.pl.rep1$msp5+6*all.pl.rep1$msp3+7.5*all.pl.rep1$msp1+1))>0.25
evi.selc.rep2 <- 2.5*((all.pl.rep2$msp5-all.pl.rep2$msp3)/(all.pl.rep2$msp5+6*all.pl.rep2$msp3+7.5*all.pl.rep2$msp1+1))>0.25

## We want a vector with length number of pixels and at each index of LK... the mean soil height of LK>...
# to substract from the heigts. 
mean.soil.height.rep1.acc <- sapply(unique(all.pl.rep1$use.lk),function(x) mean(all.pl.rep1$estheight[all.pl.rep1$use.lk==x&!evi.selc.rep1]))

mean.soil.height.rep1 <- c()
for(i in 1:length(mean.soil.height.rep1.acc)){
  mean.soil.height.rep1 <- c(mean.soil.height.rep1,rep(mean.soil.height.rep1.acc[i],sum(all.pl.rep1$use.lk==names(mean.soil.height.rep1.acc)[i])))
}

mean.soil.height.rep2.acc <- sapply(unique(all.pl.rep2$use.lk),function(x) mean(all.pl.rep2$estheight[all.pl.rep2$use.lk==x&!evi.selc.rep2]))

mean.soil.height.rep2 <- c()
for(i in 1:length(mean.soil.height.rep2.acc)){
  mean.soil.height.rep2 <- c(mean.soil.height.rep2,rep(mean.soil.height.rep2.acc[i],sum(all.pl.rep2$use.lk==names(mean.soil.height.rep2.acc)[i])))
}

cor.height.rep1 <- all.pl.rep1$estheight[evi.selc.rep1]-mean.soil.height.rep1[evi.selc.rep1]
cor.height.rep2 <- all.pl.rep2$estheight[evi.selc.rep2]-mean.soil.height.rep2[evi.selc.rep2]

# Calculate new phenotypes.
phe.height.cor.rep1 <- aggregate(cor.height.rep1,list(all.pl.rep1$use.lk[evi.selc.rep1]),phe.extr)
colnames(phe.height.cor.rep1) <- gsub("x","height",colnames(phe.height.cor.rep1))

phe.height.cor.rep2 <- aggregate(cor.height.rep2,list(all.pl.rep2$use.lk[evi.selc.rep2]),phe.extr)
colnames(phe.height.cor.rep2) <- gsub("x","height",colnames(phe.height.cor.rep2))

# Calculate heritability. 
ano.height <- anova(lm(c(phe.height.cor.rep1$height[,3],phe.height.cor.rep2$height[,3])~c(phe.height.cor.rep1$Group.1,phe.height.cor.rep2$Group.1)))

ano.height$`Sum Sq`[1]/sum(ano.height$`Sum Sq`)

height_frame <- data.frame(Accession = 1:194,Height = phe.height.cor.rep1$height[,1])

load(file = "Phenotypes_per_plot/obj_phe.sat1.1106.out")
load(file = "Phenotypes_per_plot/obj_phe.sat2.1106.out")

# Calculate heritability non-corrected height.
ano.height.nocor <-  anova(lm(c(phe.sat1.1106$height.trimmed_mean_10,phe.sat2.1106$height.trimmed_mean_10)~c(phe.sat1.1106$accession,phe.sat2.1106$accession)))
ano.height.nocor$`Sum Sq`[1]/sum(ano.height.nocor$`Sum Sq`)

load(file = "phenotypes_soil/obj_phe.sat1.1106.soil.out")
load(file = "phenotypes_soil/obj_phe.sat2.1106.soil.out")

# Calculate heritabilty soil height.
ano.height.soil <-  anova(lm(c(phe.sat1.1106.soil$height.trimmed_mean_10,phe.sat2.1106.soil$height.trimmed_mean_10)~c(phe.sat1.1106.soil$accession,phe.sat2.1106.soil$accession)))
ano.height.soil$`Sum Sq`[1]/sum(ano.height.nocor$`Sum Sq`)

# Remove uncorrected height phenotypes and put in the corrected phenotypes.
phe.sat1.1106[,121:137] <- NULL
phe.sat1.1106 <- data.frame(phe.sat1.1106,as.matrix(phe.height.cor.rep1[-1]))

phe.sat2.1106[,121:137] <- NULL
phe.sat2.1106 <- data.frame(phe.sat2.1106,as.matrix(phe.height.cor.rep2[-1]))

# Save new phenotypes
save(phe.sat1.1106,file = "corrected_phenotypes/obj.phe.cor.sat1.1106.out")
save(phe.sat2.1106,file = "corrected_phenotypes/obj.phe.cor.sat2.1106.out")

#write.xlsx(phe.sat1.1106,file="corrected_phenotypes/phe_sat1_1106.xlsx",rownames=T)
#write.xlsx(phe.sat2.1106,file="corrected_phenotypes/phe_sat2_1106.xlsx",rownames=T)

### Sativa 2506

load(file = "R_objects_plots/obj_all.pl_sat_rep1_2506_rgb_dsm_msp_red_nd.out")
all.pl.rep1 <- all.pl
rm(all.pl)
load(file = "R_objects_plots/obj_all.pl_sat_rep2_2506_rgb_dsm_msp_red_nd.out")
all.pl.rep2 <- all.pl
rm(all.pl)

colnames(all.pl.rep1)[10] <- "msp1"
colnames(all.pl.rep2)[10] <- "msp1"

# Here we obtain a logical with TRUE if EVI > 0.25 (plant pixels) and FALSE otherwise.
evi.selc.rep1 <- 2.5*((all.pl.rep1$msp5-all.pl.rep1$msp3)/(all.pl.rep1$msp5+6*all.pl.rep1$msp3+7.5*all.pl.rep1$msp1+1))>0.25
evi.selc.rep2 <- 2.5*((all.pl.rep2$msp5-all.pl.rep2$msp3)/(all.pl.rep2$msp5+6*all.pl.rep2$msp3+7.5*all.pl.rep2$msp1+1))>0.25

## We want a vector with length number of pixels and at each index of LK... the mean soil height of LK>...
# to substract from the heigts. 
mean.soil.height.rep1.acc <- sapply(unique(all.pl.rep1$use.lk),function(x) mean(all.pl.rep1$estheight[all.pl.rep1$use.lk==x&!evi.selc.rep1]))

mean.soil.height.rep1 <- c()
for(i in 1:length(mean.soil.height.rep1.acc)){
  mean.soil.height.rep1 <- c(mean.soil.height.rep1,rep(mean.soil.height.rep1.acc[i],sum(all.pl.rep1$use.lk==names(mean.soil.height.rep1.acc)[i])))
}

mean.soil.height.rep2.acc <- sapply(unique(all.pl.rep2$use.lk),function(x) mean(all.pl.rep2$estheight[all.pl.rep2$use.lk==x&!evi.selc.rep2]))

mean.soil.height.rep2 <- c()
for(i in 1:length(mean.soil.height.rep2.acc)){
  mean.soil.height.rep2 <- c(mean.soil.height.rep2,rep(mean.soil.height.rep2.acc[i],sum(all.pl.rep2$use.lk==names(mean.soil.height.rep2.acc)[i])))
}

cor.height.rep1 <- all.pl.rep1$estheight[evi.selc.rep1]-mean.soil.height.rep1[evi.selc.rep1]
cor.height.rep2 <- all.pl.rep2$estheight[evi.selc.rep2]-mean.soil.height.rep2[evi.selc.rep2]

# Calculate new phenotypes.
phe.height.cor.rep1 <- aggregate(cor.height.rep1,list(all.pl.rep1$use.lk[evi.selc.rep1]),phe.extr)
colnames(phe.height.cor.rep1) <- gsub("x","height",colnames(phe.height.cor.rep1))

phe.height.cor.rep2 <- aggregate(cor.height.rep2,list(all.pl.rep2$use.lk[evi.selc.rep2]),phe.extr)
colnames(phe.height.cor.rep2) <- gsub("x","height",colnames(phe.height.cor.rep2))

# Calculate heritability. 
ano.height <- anova(lm(c(phe.height.cor.rep1$height[,3],phe.height.cor.rep2$height[,3])~c(phe.height.cor.rep1$Group.1,phe.height.cor.rep2$Group.1)))

ano.height$`Sum Sq`[1]/sum(ano.height$`Sum Sq`)

load(file = "Phenotypes_per_plot/obj_phe.sat1.2506.out")
load(file = "Phenotypes_per_plot/obj_phe.sat2.2506.out")

# Calculate heritability non-corrected height.
ano.height.nocor <-  anova(lm(c(phe.sat1.2506$height.trimmed_mean_10,phe.sat2.2506$height.trimmed_mean_10)~c(phe.sat1.2506$accession,phe.sat2.2506$accession)))
ano.height.nocor$`Sum Sq`[1]/sum(ano.height.nocor$`Sum Sq`)

load(file = "phenotypes_soil/obj_phe.sat1.2506.soil.out")
load(file = "phenotypes_soil/obj_phe.sat2.2506.soil.out")

# Calculate heritabilty soil height.
ano.height.soil <-  anova(lm(c(phe.sat1.2506.soil$height.trimmed_mean_10,phe.sat2.2506.soil$height.trimmed_mean_10)~c(phe.sat1.2506.soil$accession,phe.sat2.2506.soil$accession)))
ano.height.soil$`Sum Sq`[1]/sum(ano.height.nocor$`Sum Sq`)

# Remove uncorrected height phenotypes and put in the corrected phenotypes.
phe.sat1.2506[,138:154] <- NULL
phe.sat1.2506 <- data.frame(phe.sat1.2506,as.matrix(phe.height.cor.rep1[-1]))

phe.sat2.2506[,138:154] <- NULL
phe.sat2.2506 <- data.frame(phe.sat2.2506,as.matrix(phe.height.cor.rep2[-1]))

# Save new phenotypes
save(phe.sat1.2506,file = "corrected_phenotypes/obj.phe.cor.sat1.2506.out")
save(phe.sat2.2506,file = "corrected_phenotypes/obj.phe.cor.sat2.2506.out")

#write.xlsx(phe.sat1.2506,file="corrected_phenotypes/phe_sat1_2506.xlsx",rownames=T)
#write.xlsx(phe.sat2.2506,file="corrected_phenotypes/phe_sat2_2506.xlsx",rownames=T)