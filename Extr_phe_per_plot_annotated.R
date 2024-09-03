##### This code extracts phenotypes from the drone data. We extract phenotypes on a per plot rather than a per
##### plant basis. WE use EVI to select plant pixels. At the end of the code we discuss some alternative 
##### indexes.

## libraries
library(ggplot2)
library(viridis)
library(cowplot)
library(openxlsx)
library(gplots)
library(cowplot)

## We start with the data from 11-06-2021

## Load in metadata
load(file="obj_metawco.out")
metawco

#### Each objects contains the data on one replicate of one species. As there are two species and two replicates
#### per species, we have four objects. Below code should be ran separately for each object.
#load(file = "R_objects_plots/obj_all.pl_sat_rep1_0611_rgb_dsm_msp.out") ### [[Done]]
#load(file = "R_objects_plots/obj_all.pl_ser_rep1_0611_rgb_dsm_msp.out") ### [[Done]]
#load(file = "R_objects_plots/obj_all.pl_sat_rep2_0611_rgb_dsm_msp.out") ### [[Done]]
#load(file = "R_objects_plots/obj_all.pl_ser_rep2_0611_rgb_dsm_msp.out") ### [[Done]]

colnames(all.pl)

colnames(all.pl)[10] <- "msp1"

### We want to select only the plant pixels for phenotyping. Let's take a bright green and dark red 
### plot as examples (from sativa rep 1).

mm <- all.pl[all.pl$use.lk == "LK019" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK019")

mm <- all.pl[all.pl$use.lk == "LK187" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK187")

# From these plots we now want to select only the plant pixels so we can phenotype the plant. Our method
# needs to work both for red and green plants. The most suitable method appears to be the enhanced 
# vegatation index. For a comparison with other methods see the end of the script. The EVI can be calculated
# according to the following formula: 2.5[(NIR – RED) / ((NIR) + (6RED) - (7.5BLUE) + 1)]. Another 
# advantage of EVI over e.g. ndvi or simple ratio is that is filters out shadow pixels, but not dark 
# red plants. 

evi <- 2.5*((all.pl$msp5-all.pl$msp3)/(all.pl$msp5+6*all.pl$msp3+7.5*all.pl$msp1+1))
hist(evi,breaks = 400,xlim = c(0,1))

# Here we obtain a logical with TRUE if EVI > 0.3 (plant pixels) and FALSE otherwise.
evi.selc <- 2.5*((all.pl$msp5-all.pl$msp3)/(all.pl$msp5+6*all.pl$msp3+7.5*all.pl$msp1+1))>0.25

# Fraction plant pixels.
sum(evi.selc)/nrow(all.pl)

# By plotting only the plant pixels from the same plot as earlier we can check how well our SR cutoff is
# performing (shown are lightest and darkest sativa acccessions)
mmselc <- all.pl[evi.selc&all.pl$use.lk == "LK019",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK019")

mmselc <- all.pl[evi.selc&all.pl$use.lk == "LK187",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK187")

#Plot lightest and darkest ser accessions:

mmselc <- all.pl[evi.selc&all.pl$use.lk == "LK333",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK333")

mmselc <- all.pl[evi.selc&all.pl$use.lk == "LK230",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK230")

# To find optimal cutoff, change the value of evi.selc and plot to see the effect.

# Here is an example of how to plot an index like ndvi.

mm <- all.pl[all.pl$use.lk == "LK019" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
ggplot(mm,aes(x,y,col = (msp5-msp3)/(msp5+msp3)))+
  geom_point()+
  scale_color_viridis() + 
  guides(col = guide_legend("NDVI")) +
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK019")

# Get pixel counts of accessions and pixel counts after filtering on evi.
plant.px <- table(all.pl[evi.selc,"use.lk"])
tot.px <- table(all.pl$use.lk)

### Now that we can select plant pixels we want to do phenotyping. Below we define a useful function that
### extracts various summary statistics for the input trait.

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

## We test our function by extracting the reflection of the NIR wavelength for each species. The aggregate
## function applies the function in the third index to the values specified in the first index per category
## in the second index.
out <- aggregate(all.pl$msp5[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
head(out[-1])

# Plot distribution of mean NIR over the accessions.
hist(out$x[,1],breaks = 100,xlab = "Mean NIR",main = "Histogram NIR reflection")


### Having seen the function works, we can now collect phenotypes en mass. We start with collecting 
### phenotypes from the RGB values. We collect the simple rgb values, their proportion relative to the sum
### of RGB values, log2() of pairwise ratio's and sum of rgb values.

phe.red <- aggregate(all.pl$red[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.red) <- gsub("x","red",colnames(phe.red))
phe.green <- aggregate(all.pl$green[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.green) <- gsub("x","green",colnames(phe.green))
phe.blue <- aggregate(all.pl$blue[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.blue) <- gsub("x","blue",colnames(phe.blue))
phe.relred <- aggregate((all.pl$red/apply(all.pl[,c("red","green","blue")],1,sum))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.relred) <- gsub("x","relred",colnames(phe.relred))
phe.relgreen <- aggregate((all.pl$green/apply(all.pl[,c("red","green","blue")],1,sum))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.relgreen) <- gsub("x","relgreen",colnames(phe.relgreen))
phe.relblue <- aggregate((all.pl$blue/apply(all.pl[,c("red","green","blue")],1,sum))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.relblue) <- gsub("x","relblue",colnames(phe.relblue))
phe.rgrat <- aggregate(log2((all.pl$red+1)/(all.pl$green+1))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr,simplify = T)
colnames(phe.rgrat) <- gsub("x","rgrat",colnames(phe.rgrat))
phe.rbrat <- aggregate(log2((all.pl$red+1)/(all.pl$blue+1))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.rbrat) <- gsub("x","rbrat",colnames(phe.rbrat))
phe.gbrat <- aggregate(log2((all.pl$green+1)/(all.pl$blue+1))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.gbrat) <- gsub("x","gbrat",colnames(phe.gbrat))
phe.coltot <- aggregate(apply(all.pl[,c("red","green","blue")],1,sum)[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.coltot) <- gsub("x","coltot",colnames(phe.coltot))
phe.rgobrat <- aggregate(log2((all.pl$red+all.pl$green+1)/(all.pl$blue+1))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.rgobrat) <- gsub("x","rgobrat",colnames(phe.rgobrat))
phe.rbograt <- aggregate(log2((all.pl$red+all.pl$blue+1)/(all.pl$green+1))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.rbograt) <- gsub("x","rbograt",colnames(phe.rbograt))
phe.gborrat <- aggregate(log2((all.pl$green+all.pl$blue+1)/(all.pl$red+1))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.gborrat) <- gsub("x","gborrat",colnames(phe.gborrat))

## Height 
phe.height <- aggregate(all.pl$estheight[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.height) <- gsub("x","height",colnames(phe.height))

## msp MSP (Blue: 475 nm; Green: 560 nm; Red: 668 nm; Red-edge 717 nm; Near-IR: 842 nm)
phe.msp1 <- aggregate(all.pl$msp1[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.msp1) <- gsub("x","msp1",colnames(phe.msp1))
phe.msp2 <- aggregate(all.pl$msp2[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.msp2) <- gsub("x","msp2",colnames(phe.msp2))
phe.msp3 <- aggregate(all.pl$msp3[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.msp3) <- gsub("x","msp3",colnames(phe.msp3))
phe.msp4 <- aggregate(all.pl$msp4[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.msp4) <- gsub("x","msp4",colnames(phe.msp4))
phe.msp5 <- aggregate(all.pl$msp5[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.msp5) <- gsub("x","msp5",colnames(phe.msp5))
phe.SR <- aggregate((all.pl$msp5/all.pl$msp3)[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.SR) <- gsub("x","SR",colnames(phe.SR))
#Note that the below is only an approximation of the SIPI index, to use the exact wavelengths the hyper-
#spectral data needs to be used.
phe.sipi <- aggregate(((all.pl$msp5-all.pl$msp1)/(all.pl$msp5-all.pl$msp3))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.sipi) <- gsub("x","sipi",colnames(phe.sipi))
phe.ndvi <- aggregate(((all.pl$msp5-all.pl$msp3)/(all.pl$msp5+all.pl$msp3))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.ndvi) <- gsub("x","ndvi",colnames(phe.ndvi))
phe.EVI <- aggregate(2.5*((all.pl$msp5 - all.pl$msp3) / ((all.pl$msp5) + (6*all.pl$msp3) - (7.5*all.pl$msp1) + 1))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.EVI) <- gsub("x","EVI",colnames(phe.EVI))
phe.ARVI <- aggregate(((all.pl$msp5-2*(all.pl$msp3-all.pl$msp1))/(all.pl$msp5+2*(all.pl$msp3-all.pl$msp1)))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.ARVI) <- gsub("x","ARVI",colnames(phe.ARVI))

# List all obtained phenotypes
ls()[grep("phe",ls())[-5]]

### Save data to excel.

# sat rep1 1106

phe.sat1.1106.soil <- data.frame(phe.ARVI[-1],phe.blue[-1],phe.coltot[-1],phe.EVI[-1],phe.gborrat[-1],phe.gbrat[-1],phe.green[-1],phe.height[-1],phe.msp1[-1],phe.msp2[-1],phe.msp3[-1],    
                                 phe.msp4[-1],phe.msp5[-1],phe.ndvi[-1],phe.rbrat[-1],phe.red[-1],phe.relblue[-1],phe.relgreen[-1],phe.relred[-1],phe.rbograt[-1],phe.rgobrat[-1],
                                 phe.rgrat[-1],phe.sipi[-1],phe.SR[-1],as.numeric(plant.px),as.numeric(tot.px)) 
phe.sat1.1106.soil <- data.frame(phe.blue$Group.1,as.matrix(phe.sat1.1106.soil))
colnames(phe.sat1.1106.soil)[1] <- "accession"
colnames(phe.sat1.1106.soil)[length(colnames(phe.sat1.1106.soil))-1]<-c("plant.px")
colnames(phe.sat1.1106.soil)[length(colnames(phe.sat1.1106.soil))]<-c("tot.px")

# Plot correlation matrix
png(filename = "cor_phe_sat1_1106.png",width = 1500, height = 1500)
heatmap.2(cor(phe.sat1.1106.soil[,-1],use="complete.obs"),trace = "none",col = turbo(256),na.rm = T)
dev.off()

phe.sat1.1106 <- phe.sat1.1106.soil
# save(phe.sat1.1106,file="Phenotypes_per_plot/obj_phe.sat1.1106.out")
# write.xlsx(phe.sat1.1106.soil,file="Phenotypes_per_plot/phe_sat_rep1_1106.xlsx",rownames=T)

# sat rep2 1106

phe.sat2.1106.soil <- data.frame(phe.ARVI[-1],phe.blue[-1],phe.coltot[-1],phe.EVI[-1],phe.gborrat[-1],phe.gbrat[-1],phe.green[-1],phe.height[-1],phe.msp1[-1],phe.msp2[-1],phe.msp3[-1],    
                                 phe.msp4[-1],phe.msp5[-1],phe.ndvi[-1],phe.rbrat[-1],phe.red[-1],phe.relblue[-1],phe.relgreen[-1],phe.relred[-1],phe.rbograt[-1],phe.rgobrat[-1],
                                 phe.rgrat[-1],phe.sipi[-1],phe.SR[-1],as.numeric(plant.px),as.numeric(tot.px)) 
phe.sat2.1106.soil <- data.frame(phe.blue$Group.1,as.matrix(phe.sat2.1106.soil))
colnames(phe.sat2.1106.soil)[1] <- "accession"
colnames(phe.sat2.1106.soil)[length(colnames(phe.sat2.1106.soil))-1]<-c("plant.px")
colnames(phe.sat2.1106.soil)[length(colnames(phe.sat2.1106.soil))]<-c("tot.px")

phe.sat2.1106 <- phe.sat2.1106.soil

# save(phe.sat2.1106,file="Phenotypes_per_plot/obj_phe.sat2.1106.out")
# write.xlsx(phe.sat2.1106.soil,file="Phenotypes_per_plot/phe_sat_rep2_1106.xlsx",rownames=T)

# ser rep1 1106

phe.ser1.1106.soil <- data.frame(phe.ARVI[-1],phe.blue[-1],phe.coltot[-1],phe.EVI[-1],phe.gborrat[-1],phe.gbrat[-1],phe.green[-1],phe.height[-1],phe.msp1[-1],phe.msp2[-1],phe.msp3[-1],    
                                 phe.msp4[-1],phe.msp5[-1],phe.ndvi[-1],phe.rbrat[-1],phe.red[-1],phe.relblue[-1],phe.relgreen[-1],phe.relred[-1],phe.rbograt[-1],phe.rgobrat[-1],
                                 phe.rgrat[-1],phe.sipi[-1],phe.SR[-1],as.numeric(plant.px),as.numeric(tot.px)) 
phe.ser1.1106.soil <- data.frame(phe.blue$Group.1,as.matrix(phe.ser1.1106.soil))
colnames(phe.ser1.1106.soil)[1] <- "accession"
colnames(phe.ser1.1106.soil)[length(colnames(phe.ser1.1106.soil))-1]<-c("plant.px")
colnames(phe.ser1.1106.soil)[length(colnames(phe.ser1.1106.soil))]<-c("tot.px")

phe.ser1.1106 <- phe.ser1.1106.soil

# save(phe.ser1.1106,file="Phenotypes_per_plot/obj_phe.ser1.1106.out")
# write.xlsx(phe.ser1.1106.soil,file="Phenotypes_per_plot/phe_ser_rep1_1106.xlsx",rownames=T)

# ser rep2 1106

phe.ser2.1106.soil <- data.frame(phe.ARVI[-1],phe.blue[-1],phe.coltot[-1],phe.EVI[-1],phe.gborrat[-1],phe.gbrat[-1],phe.green[-1],phe.height[-1],phe.msp1[-1],phe.msp2[-1],phe.msp3[-1],    
                                 phe.msp4[-1],phe.msp5[-1],phe.ndvi[-1],phe.rbrat[-1],phe.red[-1],phe.relblue[-1],phe.relgreen[-1],phe.relred[-1],phe.rbograt[-1],phe.rgobrat[-1],
                                 phe.rgrat[-1],phe.sipi[-1],phe.SR[-1],as.numeric(plant.px),as.numeric(tot.px)) 
phe.ser2.1106.soil <- data.frame(phe.blue$Group.1,as.matrix(phe.ser2.1106.soil))
colnames(phe.ser2.1106.soil)[1] <- "accession"
colnames(phe.ser2.1106.soil)[length(colnames(phe.ser2.1106.soil))-1]<-c("plant.px")
colnames(phe.ser2.1106.soil)[length(colnames(phe.ser2.1106.soil))]<-c("tot.px")

phe.ser2.1106 <- phe.ser2.1106.soil

# save(phe.ser2.1106,file="Phenotypes_per_plot/obj_phe.ser2.1106.out")
# write.xlsx(phe.ser2.1106.soil,file="Phenotypes_per_plot/phe_ser_rep2_1106.xlsx",rownames=T)

#### Now do the same for data from 25-06-2020, we use the same function for extracting phenotypes.

load(file="R_objects_plots/obj_all.pl_sat_rep1_2506_rgb_dsm_msp_red_nd.out") ### [[Done]]
#load(file="R_objects_plots/obj_all.pl_ser_rep1_2506_rgb_dsm_msp_red_nd.out") ### [[Done]]
#load(file="R_objects_plots/obj_all.pl_sat_rep2_2506_rgb_dsm_msp_red_nd.out") ### [[Done]]
#load(file="R_objects_plots/obj_all.pl_ser_rep2_2506_rgb_dsm_msp_red_nd.out") ### [[Done]]

colnames(all.pl)[10] <- "msp1"

# Here we obtain a logical with TRUE if EVI > 0.25 (plant pixels) and FALSE otherwise.
evi.selc <- 2.5*((all.pl$msp5-all.pl$msp3)/(all.pl$msp5+6*all.pl$msp3+7.5*all.pl$msp1+1))>0.4

# Get pixel counts of accessions and pixel counts after filtering on evi.
plant.px <- table(all.pl[evi.selc,"use.lk"])
tot.px <- table(all.pl$use.lk)

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

# Let's calculate phenotypes again. There are some additional phenotypes from 25-06, these are the last
# four phenotypes.
phe.red <- aggregate(all.pl$red[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.red) <- gsub("x","red",colnames(phe.red))
phe.green <- aggregate(all.pl$green[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.green) <- gsub("x","green",colnames(phe.green))
phe.blue <- aggregate(all.pl$blue[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.blue) <- gsub("x","blue",colnames(phe.blue))
phe.relred <- aggregate((all.pl$red/apply(all.pl[,c("red","green","blue")],1,sum))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.relred) <- gsub("x","relred",colnames(phe.relred))
phe.relgreen <- aggregate((all.pl$green/apply(all.pl[,c("red","green","blue")],1,sum))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.relgreen) <- gsub("x","relgreen",colnames(phe.relgreen))
phe.relblue <- aggregate((all.pl$blue/apply(all.pl[,c("red","green","blue")],1,sum))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.relblue) <- gsub("x","relblue",colnames(phe.relblue))
phe.rgrat <- aggregate(log2((all.pl$red+1)/(all.pl$green+1))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr,simplify = T)
colnames(phe.rgrat) <- gsub("x","rgrat",colnames(phe.rgrat))
phe.rbrat <- aggregate(log2((all.pl$red+1)/(all.pl$blue+1))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.rbrat) <- gsub("x","rbrat",colnames(phe.rbrat))
phe.gbrat <- aggregate(log2((all.pl$green+1)/(all.pl$blue+1))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.gbrat) <- gsub("x","gbrat",colnames(phe.gbrat))
phe.coltot <- aggregate(apply(all.pl[,c("red","green","blue")],1,sum)[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.coltot) <- gsub("x","coltot",colnames(phe.coltot))
phe.rgobrat <- aggregate(log2((all.pl$red+all.pl$green+1)/(all.pl$blue+1))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.rgobrat) <- gsub("x","rgobrat",colnames(phe.rgobrat))
phe.rbograt <- aggregate(log2((all.pl$red+all.pl$blue+1)/(all.pl$green+1))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.rbograt) <- gsub("x","rbograt",colnames(phe.rbograt))
phe.gborrat <- aggregate(log2((all.pl$green+all.pl$blue+1)/(all.pl$red+1))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.gborrat) <- gsub("x","gborrat",colnames(phe.gborrat))


## Height 
phe.height <- aggregate(all.pl$estheight[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.height) <- gsub("x","height",colnames(phe.height))

## msp MSP (Blue: 475 nm; Green: 560 nm; Red: 668 nm; Red-edge 717 nm; Near-IR: 842 nm)
phe.msp1 <- aggregate(all.pl$msp1[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.msp1) <- gsub("x","msp1",colnames(phe.msp1))
phe.msp2 <- aggregate(all.pl$msp2[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.msp2) <- gsub("x","msp2",colnames(phe.msp2))
phe.msp3 <- aggregate(all.pl$msp3[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.msp3) <- gsub("x","msp3",colnames(phe.msp3))
phe.msp4 <- aggregate(all.pl$msp4[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.msp4) <- gsub("x","msp4",colnames(phe.msp4))
phe.msp5 <- aggregate(all.pl$msp5[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.msp5) <- gsub("x","msp5",colnames(phe.msp5))
phe.SR <- aggregate((all.pl$msp5/all.pl$msp3)[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.SR) <- gsub("x","SR",colnames(phe.SR))
#Note that the below is only an approximation of the SIPI index, to use the exact wavelengths the hyper-
#spectral data needs to be used.
phe.sipi <- aggregate(((all.pl$msp5-all.pl$msp1)/(all.pl$msp5-all.pl$msp3))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.sipi) <- gsub("x","sipi",colnames(phe.sipi))
phe.ndvi <- aggregate(((all.pl$msp5-all.pl$msp3)/(all.pl$msp5+all.pl$msp3))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.ndvi) <- gsub("x","ndvi",colnames(phe.ndvi))
phe.EVI <- aggregate(2.5*((all.pl$msp5 - all.pl$msp3) / ((all.pl$msp5) + (6*all.pl$msp3) - (7.5*all.pl$msp1) + 1))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.EVI) <- gsub("x","EVI",colnames(phe.EVI))
phe.ARVI <- aggregate(((all.pl$msp5-2*(all.pl$msp3-all.pl$msp1))/(all.pl$msp5+2*(all.pl$msp3-all.pl$msp1)))[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.ARVI) <- gsub("x","ARVI",colnames(phe.ARVI))

## cired, ndre, ndvi, wdvi
phe.cired <- aggregate(all.pl$cired[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.cired) <- gsub("x","cired",colnames(phe.cired))
phe.ndre <- aggregate(all.pl$ndre[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.ndre) <- gsub("x","ndre",colnames(phe.ndre))
phe.ndvi2 <- aggregate(all.pl$ndvi[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.ndvi2) <- gsub("x","ndvi2",colnames(phe.ndvi2))
phe.wdvi <- aggregate(all.pl$wdvi[evi.selc],list(all.pl$use.lk[evi.selc]),phe.extr)
colnames(phe.wdvi) <- gsub("x","wdvi",colnames(phe.wdvi))


ls()[grep("phe",ls())[-6]]

### Save data to excel.

# sat rep1 2506

phe.sat1.2506.soil <- data.frame(phe.ARVI[-1],phe.blue[-1],phe.cired[-1],phe.coltot[-1],phe.EVI[-1],phe.gborrat[-1],phe.gbrat[-1],phe.green[-1],phe.height[-1],phe.msp1[-1],phe.msp2[-1],phe.msp3[-1],    
                                 phe.msp4[-1],phe.msp5[-1],phe.ndre[-1],phe.ndvi[-1],phe.ndvi2[-1],phe.rbrat[-1],phe.red[-1],phe.relblue[-1],phe.relgreen[-1],phe.relred[-1],phe.rbograt[-1],  
                                 phe.rgobrat[-1],phe.rgrat[-1],phe.sipi[-1],phe.SR[-1],phe.wdvi[-1],as.numeric(plant.px),as.numeric(tot.px)) 
phe.sat1.2506.soil <- data.frame(phe.blue$Group.1,as.matrix(phe.sat1.2506.soil))
colnames(phe.sat1.2506.soil)[1] <- "accession"
colnames(phe.sat1.2506.soil)[length(colnames(phe.sat1.2506.soil))-1]<-c("plant.px")
colnames(phe.sat1.2506.soil)[length(colnames(phe.sat1.2506.soil))]<-c("tot.px")

# Plot correlation matrix
png(filename = "cor_phe_sat1_2506.png",width = 1500, height = 1500)
heatmap.2(cor(phe.sat1.2506.soil[,-1],use="complete.obs"),trace = "none",col = turbo(256),na.rm = T)
dev.off()

phe.sat1.2506 <- phe.sat1.2506.soil

# save(phe.sat1.2506,file="Phenotypes_per_plot/obj_phe.sat1.2506.out")
# write.xlsx(phe.sat1.2506.soil,file="Phenotypes_per_plot/phe_sat_rep1_2506.xlsx",rownames=T)

# sat rep2 2506

phe.sat2.2506.soil <- data.frame(phe.ARVI[-1],phe.blue[-1],phe.cired[-1],phe.coltot[-1],phe.EVI[-1],phe.gborrat[-1],phe.gbrat[-1],phe.green[-1],phe.height[-1],phe.msp1[-1],phe.msp2[-1],phe.msp3[-1],    
                                 phe.msp4[-1],phe.msp5[-1],phe.ndre[-1],phe.ndvi[-1],phe.ndvi2[-1],phe.rbrat[-1],phe.red[-1],phe.relblue[-1],phe.relgreen[-1],phe.relred[-1],phe.rbograt[-1],  
                                 phe.rgobrat[-1],phe.rgrat[-1],phe.sipi[-1],phe.SR[-1],phe.wdvi[-1],as.numeric(plant.px),as.numeric(tot.px)) 
phe.sat2.2506.soil <- data.frame(phe.blue$Group.1,as.matrix(phe.sat2.2506.soil))
colnames(phe.sat2.2506.soil)[1] <- "accession"
colnames(phe.sat2.2506.soil)[length(colnames(phe.sat2.2506.soil))-1]<-c("plant.px")
colnames(phe.sat2.2506.soil)[length(colnames(phe.sat2.2506.soil))]<-c("tot.px")

phe.sat2.2506 <- phe.sat2.2506.soil

# save(phe.sat2.2506,file="Phenotypes_per_plot/obj_phe.sat2.2506.out")
# write.xlsx(phe.sat2.2506.soil,file="Phenotypes_per_plot/phe_sat_rep2_2506.xlsx",rownames=T)

# ser rep1 2506

phe.ser1.2506.soil <- data.frame(phe.ARVI[-1],phe.blue[-1],phe.cired[-1],phe.coltot[-1],phe.EVI[-1],phe.gborrat[-1],phe.gbrat[-1],phe.green[-1],phe.height[-1],phe.msp1[-1],phe.msp2[-1],phe.msp3[-1],    
                                 phe.msp4[-1],phe.msp5[-1],phe.ndre[-1],phe.ndvi[-1],phe.ndvi2[-1],phe.rbrat[-1],phe.red[-1],phe.relblue[-1],phe.relgreen[-1],phe.relred[-1],phe.rbograt[-1],  
                                 phe.rgobrat[-1],phe.rgrat[-1],phe.sipi[-1],phe.SR[-1],phe.wdvi[-1],as.numeric(plant.px),as.numeric(tot.px))  
phe.ser1.2506.soil <- data.frame(phe.blue$Group.1,as.matrix(phe.ser1.2506.soil))
colnames(phe.ser1.2506.soil)[1] <- "accession"
colnames(phe.ser1.2506.soil)[length(colnames(phe.ser1.2506.soil))-1]<-c("plant.px")
colnames(phe.ser1.2506.soil)[length(colnames(phe.ser1.2506.soil))]<-c("tot.px")

phe.ser1.2506 <- phe.ser1.2506.soil

# save(phe.ser1.2506,file="Phenotypes_per_plot/obj_phe.ser1.2506.out")
# write.xlsx(phe.ser1.2506.soil,file="Phenotypes_per_plot/phe_ser_rep1_2506.xlsx",rownames=T)

# ser rep2 2506

phe.ser2.2506.soil <- data.frame(phe.ARVI[-1],phe.blue[-1],phe.cired[-1],phe.coltot[-1],phe.EVI[-1],phe.gborrat[-1],phe.gbrat[-1],phe.green[-1],phe.height[-1],phe.msp1[-1],phe.msp2[-1],phe.msp3[-1],    
                                 phe.msp4[-1],phe.msp5[-1],phe.ndre[-1],phe.ndvi[-1],phe.ndvi2[-1],phe.rbrat[-1],phe.red[-1],phe.relblue[-1],phe.relgreen[-1],phe.relred[-1],phe.rbograt[-1],  
                                 phe.rgobrat[-1],phe.rgrat[-1],phe.sipi[-1],phe.SR[-1],phe.wdvi[-1],as.numeric(plant.px),as.numeric(tot.px)) 
phe.ser2.2506.soil <- data.frame(phe.blue$Group.1,as.matrix(phe.ser2.2506.soil))
colnames(phe.ser2.2506.soil)[1] <- "accession"
colnames(phe.ser2.2506.soil)[length(colnames(phe.ser2.2506.soil))-1]<-c("plant.px")
colnames(phe.ser2.2506.soil)[length(colnames(phe.ser2.2506.soil))]<-c("tot.px")

phe.ser2.2506 <- phe.ser2.2506.soil

# save(phe.ser2.2506,file="Phenotypes_per_plot/obj_phe.ser2.2506.out")
# write.xlsx(phe.ser2.2506.soil,file="Phenotypes_per_plot/phe_ser_rep2_2506.xlsx",rownames=T)

##### test height calc: so height has to be corrected for soil height.
load("obj_phe.sat1.2506.out")
load("obj_phe.sat2.2506.out")



#### Below some additional, non-essential code to show the effect of different indexes on plant pixel
### filtering. SR is the simple ratio, ndvi normalized difference vegetation index. These are both inferior
### to evi, as the code below will show. We select some representative plots, and plot the effect of the
### different filtering methods.

SR <- all.pl$msp5/all.pl$msp3 
hist(SR,breaks = 400,xlim = c(0,30))

# mega.selc is a logical which if yes corresponds to a soil/non-plant pixel, otherwise a plant pixel.
mega.selc <- SR < 5

# Fraction soil pixels.
sum(mega.selc)/nrow(all.pl)

mm <- all.pl[all.pl$use.lk == "LK198" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK198")

mmselc <- all.pl[evi.selc&all.pl$use.lk == "LK198",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK198")

mmselc <- all.pl[evi.selc&all.pl$use.lk == "LK198"&all.pl$red+all.pl$green+all.pl$blue >150,]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK198")

### Make plot filtering on ndvi for various accessions.

ndvi.selc <- (all.pl$msp5-all.pl$msp3)/(all.pl$msp5+all.pl$msp3)>0.8

mmselc <- all.pl[!mega.selc&all.pl$use.lk == "LK187",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
SR_187 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK187")

mmselc <- all.pl[ndvi.selc&all.pl$use.lk == "LK187",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
selc_187 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK187")

mm <- all.pl[all.pl$use.lk == "LK187" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_187 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK187")

mmselc <- all.pl[!mega.selc&all.pl$use.lk == "LK198",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
SR_198 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK198")

mmselc <- all.pl[ndvi.selc&all.pl$use.lk == "LK198",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
selc_198 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK198")

mm <- all.pl[all.pl$use.lk == "LK198" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_198 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK198")

mmselc <- all.pl[!mega.selc&all.pl$use.lk == "LK150",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
SR_150 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK150")

mmselc <- all.pl[ndvi.selc&all.pl$use.lk == "LK150",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
selc_150 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK150")

mm <- all.pl[all.pl$use.lk == "LK150" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_150 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK150")

mmselc <- all.pl[!mega.selc&all.pl$use.lk == "LK196",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
SR_196 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK196")

mmselc <- all.pl[ndvi.selc&all.pl$use.lk == "LK196",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
selc_196 <-ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK196")

mm <- all.pl[all.pl$use.lk == "LK196" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_196 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK196")

mm <- all.pl[all.pl$use.lk == "LK019" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_019 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK019")

mmselc <- all.pl[ndvi.selc&all.pl$use.lk == "LK019",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
selc_019 <-ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK019")

mmselc <- all.pl[!mega.selc&all.pl$use.lk == "LK019",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
SR_019 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK019")

ndvi_vs_SR <- plot_grid(plot_grid(plot_150,selc_150,SR_150,ncol = 3),plot_grid(plot_196,selc_196,SR_196,ncol = 3),plot_grid(plot_198,selc_198,SR_198,ncol = 3),plot_grid(plot_187,selc_187,SR_187,ncol = 3),plot_grid(plot_019,selc_019,SR_019,ncol = 3),nrow = 5)

png(filename = "ndvi_vs_SR.png",width = 1500,height = 1250)
ndvi_vs_SR
dev.off()

evi.selc <- 2.5*((all.pl$msp5-all.pl$msp3)/(all.pl$msp5+6*all.pl$msp3+7.5*all.pl$msp1+1))>0.25

mmselc <- all.pl[!mega.selc&all.pl$use.lk == "LK187",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
SR_187 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK187")

mmselc <- all.pl[evi.selc&all.pl$use.lk == "LK187",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
selc_187 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK187")

mm <- all.pl[all.pl$use.lk == "LK187" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_187 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK187")

mmselc <- all.pl[!mega.selc&all.pl$use.lk == "LK198",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
SR_198 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK198")

mmselc <- all.pl[evi.selc&all.pl$use.lk == "LK198",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
selc_198 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK198")

mm <- all.pl[all.pl$use.lk == "LK198" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_198 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK198")

mmselc <- all.pl[!mega.selc&all.pl$use.lk == "LK150",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
SR_150 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK150")

mmselc <- all.pl[evi.selc&all.pl$use.lk == "LK150",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
selc_150 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK150")

mm <- all.pl[all.pl$use.lk == "LK150" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_150 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK150")

mmselc <- all.pl[!mega.selc&all.pl$use.lk == "LK196",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
SR_196 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK196")

mmselc <- all.pl[evi.selc&all.pl$use.lk == "LK196",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
selc_196 <-ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK196")

mm <- all.pl[all.pl$use.lk == "LK196" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_196 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK196")

mm <- all.pl[all.pl$use.lk == "LK019" ,]
use.rgb <- rgb(red=mm$red,green = mm$green,blue = mm$blue,maxColorValue = 255)
plot_019 <- ggplot(mm)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK019")

mmselc <- all.pl[evi.selc&all.pl$use.lk == "LK019",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
selc_019 <-ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK019")

mmselc <- all.pl[!mega.selc&all.pl$use.lk == "LK019",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
SR_019 <- ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK019")

evi_vs_SR <- plot_grid(plot_grid(plot_150,selc_150,SR_150,ncol = 3),plot_grid(plot_196,selc_196,SR_196,ncol = 3),plot_grid(plot_198,selc_198,SR_198,ncol = 3),plot_grid(plot_187,selc_187,SR_187,ncol = 3),plot_grid(plot_019,selc_019,SR_019,ncol = 3),nrow = 5)

png(filename = "evi_vs_SR_025.png",width = 1500,height = 1250)
evi_vs_SR
dev.off()





mmselc <- all.pl[ndvi.selc&all.pl$use.lk == "LK001",]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
selc_001 <-ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK001")

selc_001


mmselc <- all.pl[ndvi.selc&all.pl$use.lk == "LK187"&all.pl$red+all.pl$green+all.pl$blue >100,]
use.rgb <- rgb(red=mmselc$red,green = mmselc$green,blue = mmselc$blue,maxColorValue = 255)
ggplot(mmselc)+
  geom_tile(aes(x,y),fill=use.rgb)+
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")+
  ggtitle("LK187")


ggplot(mmselc,aes(x,y,col = (msp3-msp2)/(msp2+msp3)))+
  geom_point()+
  scale_color_viridis() + 
  guides(col = guide_legend("(red-green)/(red+green)")) +
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")

ggplot(mm,aes(x,y,col = (msp3-msp2)/(msp2+msp3)))+
  geom_point()+
  scale_color_viridis() + 
  guides(col = guide_legend("(red-green)/(red+green)")) +
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")



ggplot(mm,aes(x,y,col = 2.5*((msp5-msp3)/(msp5+6*msp3+7.5*msp1+1))))+
  geom_point()+
  scale_color_viridis() + 
  guides(col = guide_legend("(red-green)/(red+green)")) +
  theme_void()+
  theme(plot.title = element_text(size  =25),
        axis.title.x=element_text(size = 25),
        axis.title.y=element_text(size = 25,angle = 90)) +
  xlab("Longitude")+
  ylab("Latitude")

