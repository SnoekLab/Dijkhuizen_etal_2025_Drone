

load("./DroneData2023/obj.phe.cor.sat1.1106.out")
load("./DroneData2023/obj.phe.cor.sat2.1106.out")
load("./DroneData2023/obj.phe.cor.sat1.2506.out")
load("./DroneData2023/obj.phe.cor.sat2.2506.out")


phe.sat1.1106[,2]
phe.sat2.1106[,2]

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

# meansq.collect[,1]/(apply(meansq.collect,1,sum))

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

# meansq.collect[,1]/(apply(meansq.collect,1,sum))

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


## ratio day1 day 2 2506 .......................................................................
## bind the replicates and exclude the plant.px and tot.px
selc <- !colnames(phe.sat1.2506) %in% c("plant.px","tot.px")

traits.in.both.days <- intersect(colnames(phe.sat1.1106),colnames(phe.sat1.2506))
traits.in.both.days <- traits.in.both.days [!traits.in.both.days %in% c("plant.px","tot.px","accession") ]

df <- (phe.sat1.1106[,traits.in.both.days]+0)/(phe.sat1.2506[,traits.in.both.days]+0)
d1.rat <- log2((abs(df))^(sign(df)))
df <- (phe.sat2.1106[,traits.in.both.days]+0)/(phe.sat2.2506[,traits.in.both.days]+0)
d2.rat <- log2((abs(df))^(sign(df)))

pheno.diff <- rbind(d1.rat,d2.rat)


pheno.diff$accession <- phe.sat1.1106$accession

# check mean height difference
phe.sat1.1106$accession == phe.sat1.2506$accession
phe.sat1.1106$accession == phe.sat2.2506$accession


boxplot(pheno.diff$height.mean~pheno.diff$accession)
anova(lm(pheno.diff$height.mean~pheno.diff$accession))

boxplot(pheno.diff$height.median~pheno.diff$accession)
anova(lm(pheno.diff$height.median~pheno.diff$accession))


## get the mean squares for all traits
meansq.collect <- matrix(NA,nrow = ncol(pheno.diff),ncol=2)
for ( i in 1:ncol(pheno.diff)){
  if (sum(is.na(pheno.diff[,i]))==0 &sum (abs(pheno.diff[,i]) == Inf,na.rm = T)==0){
  ok <- anova(lm(pheno.diff[,i]~pheno.diff$accession))
  meansq.collect[i,] <- ok$`Mean Sq`
}}



# meansq.collect[,1]/(apply(meansq.collect,1,sum))

within <- meansq.collect[,2]
dfRun <- 194
dfWithin <- 193
geno <- meansq.collect[,1]
between <- (geno-within)/((dfWithin/(dfRun+1))+1) # (S1^2-S2^2)/J
total <- between+within
between # Between genotype Variance
within # Within genotype Variance

h2.rat <- round((between/total)*100,2)
names(h2.rat) <- colnames(pheno.diff)

cbind(colnames(pheno.diff),h2.rat)


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



## some figures on the mean traits

h2.collect.mean <- h2.collect[grepl("mean",h2.collect$Pheno.names) & !grepl("trimmed",h2.collect$Pheno.names), ]
h2.collect.mean

Trait <- rep(h2.collect.mean$Pheno.names)
Trait <- gsub(".mean","",Trait)
H2 <- c(h2.collect.mean$X1,h2.collect.mean$X2,h2.collect.mean$X3)
Time <- rep(c("Day-78","Day-93","Ratio"),each=nrow(h2.collect.mean))

to.pl <- data.frame(Trait,H2,Time)
unique(to.pl$Trait)
to.pl$Trait <- factor(to.pl$Trait,levels = gsub(".mean","",c("height.mean","coltot.mean","red.mean","green.mean","blue.mean",
                                              "msp1.mean","msp2.mean","msp3.mean","msp4.mean","msp5.mean",
                                              "ARVI.mean","EVI.mean","ndvi.mean","sipi.mean","SR.mean",
                                              "gbrat.mean","rgrat.mean","rbrat.mean","relgreen.mean","relred.mean","relblue.mean",    
                                              "cired.mean","ndre.mean","ndvi2.mean","wdvi.mean"))
                      )


library(ggplot2)
library(cowplot)



h2.fig <- ggplot(to.pl)+
          geom_col(aes(Time,H2))+
          facet_wrap(.~Trait)+
  theme_cowplot()+
  theme(panel.background = element_rect(linewidth = 0.2,color="black"),
        axis.line = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        axis.text.x = element_text(angle = 90,size=7,hjust = 1,vjust = 0.5),
        axis.text.y = element_text(size = 7),
        panel.spacing.x = unit(1,"mm"),
        panel.spacing.y = unit(1,"mm"),
        panel.grid.major = element_line(color = "grey2",linewidth =  0.1,linetype = 3),
        legend.position = "none",
        legend.justification = c(0,1),
        legend.background = element_rect(linewidth = 0.2,fill = "grey90",color = "black"),
        legend.text=element_text(size=7))


h2.fig



h2.collect.height <- h2.collect[(grepl("height",h2.collect$Pheno.names) | grepl("Height",h2.collect$Pheno.names)) & !grepl("trimmed",h2.collect$Pheno.names), ]
h2.collect.height

Trait <- rep(h2.collect.height$Pheno.names)
Trait <- gsub("height.","",Trait)
H2 <- c(h2.collect.height$X1,h2.collect.height$X2,h2.collect.height$X3)
Time <- rep(c("Day-78","Day-93","Ratio"),each=nrow(h2.collect.height))

to.pl <- data.frame(Trait,H2,Time)
unique(to.pl$Trait)
# to.pl$Trait <- factor(to.pl$Trait,levels = gsub(".mean","",c("height.mean","coltot.mean","red.mean","green.mean","blue.mean",
#                                                             "msp1.mean","msp2.mean","msp3.mean","msp4.mean","msp5.mean",
#                                                             "ARVI.mean","EVI.mean","ndvi.mean","sipi.mean","SR.mean",
#                                                             "gbrat.mean","rgrat.mean","rbrat.mean","relgreen.mean","relred.mean","relblue.mean",    
#                                                             "cired.mean","ndre.mean","ndvi2.mean","wdvi.mean"))
# )


library(ggplot2)
library(cowplot)



h2.height.fig <- ggplot(to.pl)+
  geom_col(aes(Time,H2))+
  facet_wrap(.~Trait)+
  ggtitle("Height")+
  theme_cowplot()+
  theme(panel.background = element_rect(linewidth = 0.2,color="black"),
        axis.line = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        axis.text.x = element_text(angle = 90,size=7,hjust = 1,vjust = 0.5),
        axis.text.y = element_text(size = 7),
        panel.spacing.x = unit(1,"mm"),
        panel.spacing.y = unit(1,"mm"),
        panel.grid.major = element_line(color = "grey2",linewidth =  0.1,linetype = 3),
        legend.position = "none",
        legend.justification = c(0,1),
        legend.background = element_rect(linewidth = 0.2,fill = "grey90",color = "black"),
        legend.text=element_text(size=7))


h2.height.fig






################### END #######################################################################
