# In our manuscript we calculate broad-sense heritability with ANOVA.
# To establish that this method is viable we compare it with the lme4 broad-sense heritability.
# Which many people have already succesfully used. The results from both approaches
# are the exact same.

library(heritability)
library(lme4)
library(ggplot2)

load("corrected_phenotypes/obj.phe.cor.sat_rep1.1106.R4.3.2.out")
load("corrected_phenotypes/obj.phe.cor.sat_rep2.1106.R4.3.2.out")
load("corrected_phenotypes/obj.phe.cor.sat_rep1.2506.R4.3.2.out")
load("corrected_phenotypes/obj.phe.cor.sat_rep2.2506.R4.3.2.out")

## day 1 1106 .......................................................................
## bind the replicates and exclude the plant.px and tot.px
selc <- !colnames(phe.sat1.1106) %in% c("plant.px","tot.px")
pheno1106 <- rbind(phe.sat1.1106[,selc],phe.sat2.1106[,selc])

## get the mean squares for all traits
meansq.collect <- matrix(NA,nrow = ncol(pheno1106),ncol=2)
for ( i in 2:ncol(pheno1106)){
  ok <- anova(lm(pheno1106[,i]~pheno1106[,1]))
  meansq.collect[i,] <- ok$`Mean Sq`
}

within <- meansq.collect[,2]
dfRun <- 194
dfWithin <- 193
geno <- meansq.collect[,1]
between <- (geno-within)/((dfWithin/(dfRun+1))+1) # (S1^2-S2^2)/J
total <- between+within
between # Between genotype Variance
within # Within genotype Variance

h2.1106 <- round((between/total)*100,2)
names(h2.1106) <- colnames(pheno1106)

#### Heritability with lme4. Like in https://doi.org/10.1038/s41437-022-00528-y

alt.collect.1106 <- matrix(NA,nrow = ncol(pheno1106),ncol = 2)
for ( i in 2:ncol(pheno1106)){
  REML <- lmer(pheno1106[,i] ~ 1 + (1|pheno1106[,1]))
  Variations <- as.data.frame(VarCorr(REML))$vcov
  alt.collect.1106[i,] <- Variations
}

alt.collect.1106 <- data.frame(alt.collect.1106)
colnames(alt.collect.1106) <- c("Genotypic","Residual")
alt.collect.1106$Heritability <- alt.collect.1106$Genotypic/(alt.collect.1106$Genotypic+alt.collect.1106$Residual)
alt.collect.1106 <- alt.collect.1106[-1,]
alt.collect.1106$Trait <- colnames(pheno1106)[-1]
alt.collect.1106$Heritability_old <- h2.1106[-1]

ggplot(alt.collect.1106[grepl("mean",alt.collect.1106$Trait),], aes(x = Heritability_old, y = Heritability))+
  geom_point(size = 3)+
  theme_minimal()+
  theme(text = element_text(size = 25))+
  labs(x = "H2 anova method", y = "H2 lme4 method")

# This figure is Supplementary Figure 8 from the manuscript.
# This is only for day-78. The rest of the days are included below.

## day 2 2506 .......................................................................
## bind the replicates and exclude the plant.px and tot.px
selc <- !colnames(phe.sat1.2506) %in% c("plant.px","tot.px")
pheno2506 <- rbind(phe.sat1.2506[,selc],phe.sat2.2506[,selc])

## get the mean squares for all traits
meansq.collect <- matrix(NA,nrow = ncol(pheno2506),ncol=2)
for ( i in 2:ncol(pheno2506)){
  ok <- anova(lm(pheno2506[,i]~pheno2506[,1]))
  meansq.collect[i,] <- ok$`Mean Sq`
}

within <- meansq.collect[,2]
dfRun <- 194
dfWithin <- 193
geno <- meansq.collect[,1]
between <- (geno-within)/((dfWithin/(dfRun+1))+1) # (S1^2-S2^2)/J
total <- between+within
between # Between genotype Variance
within # Within genotype Variance

h2.2506 <- round((between/total)*100,2)
names(h2.2506) <- colnames(pheno2506)

# lme4 method day2 #########
alt.collect.2506 <- matrix(NA,nrow = ncol(pheno2506),ncol = 2)
for ( i in 2:ncol(pheno2506)){
  REML <- lmer(pheno2506[,i] ~ 1 + (1|pheno2506[,1]))
  Variations <- as.data.frame(VarCorr(REML))$vcov
  alt.collect.2506[i,] <- Variations
}

alt.collect.2506 <- data.frame(alt.collect.2506)
colnames(alt.collect.2506) <- c("Genotypic","Residual")
alt.collect.2506$Heritability <- alt.collect.2506$Genotypic/(alt.collect.2506$Genotypic+alt.collect.2506$Residual)
alt.collect.2506 <- alt.collect.2506[-1,]
alt.collect.2506$Trait <- colnames(pheno2506)[-1]
alt.collect.2506$Heritability_old <- h2.2506[-1]

ggplot(alt.collect.2506[grepl("mean",alt.collect.2506$Trait),], aes(x = Heritability_old, y = Heritability))+
  geom_point(size = 3)+
  theme_minimal()+
  theme(text = element_text(size = 25))+
  labs(x = "H2 anova method", y = "H2 lme4 method")

#### Bind and make excel file

Pheno.names <- unique(c(colnames(pheno2506),colnames(pheno1106)))
Pheno.names
h2.collect <- matrix(NA,ncol = 3,nrow = length(Pheno.names))

for ( i in 1:length(Pheno.names)){
  val1 <- c(h2.1106[Pheno.names[i]],NA)[1]
  val2 <- c(h2.2506[Pheno.names[i]],NA)[1]
  val3 <- c(h2.rat[Pheno.names[i]],NA)[1]
  h2.collect[i,] <- c(val1,val2,val3)
}
h2.collect

h2.collect <- data.frame(Pheno.names,h2.collect)
h2.collect

h2.collect[grep("height",h2.collect$Pheno.names),]


library(openxlsx)

write.xlsx(h2.collect,file="h2.collect.v4.xlsx",rownames=F)