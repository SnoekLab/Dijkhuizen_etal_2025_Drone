# This script extracts the soil pixels. And extracts all phenotypes.
# The only phenotype from the soil pixels we actually use for the paper is the height.
# We use the height in the next script, "correct_heights.R".
# Here we use the height to correct for small differences in elevation between the different plots.
# There are 4 blocks of code here. two for day 1106 and 2506 and two for rep1 and 2.

library(openxlsx)

# Create output folder if it doesn't exist already
if (!dir.exists("phenotypes_soil")){
  dir.create("phenotypes_soil")
}

###############
#Day 1106 REP 1
###############
load(file = "R_objects_plots/obj_drone_ima.rep1_sat_1106_rgb_dsm_msp.R4.3.2.out ")

# Here we obtain a logical with TRUE if EVI > 0.25 (plant pixels) and FALSE otherwise.
evi.selc <- 2.5*((all.pl$msp5-all.pl$msp3)/(all.pl$msp5+6*all.pl$msp3+7.5*all.pl$msp1+1))>0.25

# Get pixel counts of accessions and pixel counts after filtering on evi.
soil.px <- table(all.pl[!evi.selc,"use.lk"])
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

### Having seen the function works, we can now collect phenotypes en mass. We start with collecting 
### phenotypes from the RGB values. We collect the simple rgb values, their proportion relative to the sum
### of RGB values, log2() of pairwise ratio's and sum of rgb values.

phe.red <- aggregate(all.pl$red[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.red) <- gsub("x","red",colnames(phe.red))
phe.green <- aggregate(all.pl$green[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.green) <- gsub("x","green",colnames(phe.green))
phe.blue <- aggregate(all.pl$blue[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.blue) <- gsub("x","blue",colnames(phe.blue))
phe.relred <- aggregate((all.pl$red/apply(all.pl[,c("red","green","blue")],1,sum))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.relred) <- gsub("x","relred",colnames(phe.relred))
phe.relgreen <- aggregate((all.pl$green/apply(all.pl[,c("red","green","blue")],1,sum))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.relgreen) <- gsub("x","relgreen",colnames(phe.relgreen))
phe.relblue <- aggregate((all.pl$blue/apply(all.pl[,c("red","green","blue")],1,sum))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.relblue) <- gsub("x","relblue",colnames(phe.relblue))
phe.rgrat <- aggregate(log2((all.pl$red+1)/(all.pl$green+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr,simplify = T)
colnames(phe.rgrat) <- gsub("x","rgrat",colnames(phe.rgrat))
phe.rbrat <- aggregate(log2((all.pl$red+1)/(all.pl$blue+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.rbrat) <- gsub("x","rbrat",colnames(phe.rbrat))
phe.gbrat <- aggregate(log2((all.pl$green+1)/(all.pl$blue+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.gbrat) <- gsub("x","gbrat",colnames(phe.gbrat))
phe.coltot <- aggregate(apply(all.pl[,c("red","green","blue")],1,sum)[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.coltot) <- gsub("x","coltot",colnames(phe.coltot))
phe.rgobrat <- aggregate(log2((all.pl$red+all.pl$green+1)/(all.pl$blue+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.rgobrat) <- gsub("x","rgobrat",colnames(phe.rgobrat))
phe.rbograt <- aggregate(log2((all.pl$red+all.pl$blue+1)/(all.pl$green+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.rbograt) <- gsub("x","rbograt",colnames(phe.rbograt))
phe.gborrat <- aggregate(log2((all.pl$green+all.pl$blue+1)/(all.pl$red+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.gborrat) <- gsub("x","gborrat",colnames(phe.gborrat))

## Height 
phe.height <- aggregate(all.pl$estheight[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.height) <- gsub("x","height",colnames(phe.height))

## msp MSP (Blue: 475 nm; Green: 560 nm; Red: 668 nm; Red-edge 717 nm; Near-IR: 842 nm)
phe.msp1 <- aggregate(all.pl$msp1[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.msp1) <- gsub("x","msp1",colnames(phe.msp1))
phe.msp2 <- aggregate(all.pl$msp2[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.msp2) <- gsub("x","msp2",colnames(phe.msp2))
phe.msp3 <- aggregate(all.pl$msp3[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.msp3) <- gsub("x","msp3",colnames(phe.msp3))
phe.msp4 <- aggregate(all.pl$msp4[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.msp4) <- gsub("x","msp4",colnames(phe.msp4))
phe.msp5 <- aggregate(all.pl$msp5[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.msp5) <- gsub("x","msp5",colnames(phe.msp5))
phe.SR <- aggregate((all.pl$msp5/all.pl$msp3)[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.SR) <- gsub("x","SR",colnames(phe.SR))
#Note that the below is only an approximation of the SIPI index, to use the exact wavelengths the hyper-
#spectral data needs to be used.
phe.sipi <- aggregate(((all.pl$msp5-all.pl$msp1)/(all.pl$msp5-all.pl$msp3))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.sipi) <- gsub("x","sipi",colnames(phe.sipi))
phe.ndvi <- aggregate(((all.pl$msp5-all.pl$msp3)/(all.pl$msp5+all.pl$msp3))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.ndvi) <- gsub("x","ndvi",colnames(phe.ndvi))
phe.EVI <- aggregate(2.5*((all.pl$msp5 - all.pl$msp3) / ((all.pl$msp5) + (6*all.pl$msp3) - (7.5*all.pl$msp1) + 1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.EVI) <- gsub("x","EVI",colnames(phe.EVI))
phe.ARVI <- aggregate(((all.pl$msp5-(2*all.pl$msp3-all.pl$msp1))/(all.pl$msp5+(2*all.pl$msp3-all.pl$msp1)))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.ARVI) <- gsub("x","ARVI",colnames(phe.ARVI))

phe.sat1.1106.soil <- data.frame(phe.ARVI[-1],phe.blue[-1],phe.coltot[-1],phe.EVI[-1],phe.gborrat[-1],phe.gbrat[-1],phe.green[-1],phe.height[-1],phe.msp1[-1],phe.msp2[-1],phe.msp3[-1],    
                                 phe.msp4[-1],phe.msp5[-1],phe.ndvi[-1],phe.rbrat[-1],phe.red[-1],phe.relblue[-1],phe.relgreen[-1],phe.relred[-1],phe.rbograt[-1],phe.rgobrat[-1],
                                 phe.rgrat[-1],phe.sipi[-1],phe.SR[-1],as.numeric(soil.px),as.numeric(tot.px)) 
phe.sat1.1106.soil <- data.frame(phe.blue$Group.1,as.matrix(phe.sat1.1106.soil))
colnames(phe.sat1.1106.soil)[1] <- "accession"
colnames(phe.sat1.1106.soil)[length(colnames(phe.sat1.1106.soil))-1]<-c("soil.px")
colnames(phe.sat1.1106.soil)[length(colnames(phe.sat1.1106.soil))]<-c("tot.px")

save(phe.sat1.1106.soil,file="phenotypes_soil/obj_pheno.sat_rep1.1106.soil.R4.3.2.out")

###############
#Day 1106 REP 2
###############
load(file = "R_objects_plots/obj_drone_ima.rep2_sat_1106_rgb_dsm_msp.R4.3.2.out ")

# Here we obtain a logical with TRUE if EVI > 0.25 (plant pixels) and FALSE otherwise.
evi.selc <- 2.5*((all.pl$msp5-all.pl$msp3)/(all.pl$msp5+6*all.pl$msp3+7.5*all.pl$msp1+1))>0.25

# Get pixel counts of accessions and pixel counts after filtering on evi.
soil.px <- table(all.pl[!evi.selc,"use.lk"])
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


phe.red <- aggregate(all.pl$red[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.red) <- gsub("x","red",colnames(phe.red))
phe.green <- aggregate(all.pl$green[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.green) <- gsub("x","green",colnames(phe.green))
phe.blue <- aggregate(all.pl$blue[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.blue) <- gsub("x","blue",colnames(phe.blue))
phe.relred <- aggregate((all.pl$red/apply(all.pl[,c("red","green","blue")],1,sum))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.relred) <- gsub("x","relred",colnames(phe.relred))
phe.relgreen <- aggregate((all.pl$green/apply(all.pl[,c("red","green","blue")],1,sum))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.relgreen) <- gsub("x","relgreen",colnames(phe.relgreen))
phe.relblue <- aggregate((all.pl$blue/apply(all.pl[,c("red","green","blue")],1,sum))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.relblue) <- gsub("x","relblue",colnames(phe.relblue))
phe.rgrat <- aggregate(log2((all.pl$red+1)/(all.pl$green+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr,simplify = T)
colnames(phe.rgrat) <- gsub("x","rgrat",colnames(phe.rgrat))
phe.rbrat <- aggregate(log2((all.pl$red+1)/(all.pl$blue+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.rbrat) <- gsub("x","rbrat",colnames(phe.rbrat))
phe.gbrat <- aggregate(log2((all.pl$green+1)/(all.pl$blue+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.gbrat) <- gsub("x","gbrat",colnames(phe.gbrat))
phe.coltot <- aggregate(apply(all.pl[,c("red","green","blue")],1,sum)[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.coltot) <- gsub("x","coltot",colnames(phe.coltot))
phe.rgobrat <- aggregate(log2((all.pl$red+all.pl$green+1)/(all.pl$blue+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.rgobrat) <- gsub("x","rgobrat",colnames(phe.rgobrat))
phe.rbograt <- aggregate(log2((all.pl$red+all.pl$blue+1)/(all.pl$green+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.rbograt) <- gsub("x","rbograt",colnames(phe.rbograt))
phe.gborrat <- aggregate(log2((all.pl$green+all.pl$blue+1)/(all.pl$red+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.gborrat) <- gsub("x","gborrat",colnames(phe.gborrat))

## Height 
phe.height <- aggregate(all.pl$estheight[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.height) <- gsub("x","height",colnames(phe.height))

## msp MSP (Blue: 475 nm; Green: 560 nm; Red: 668 nm; Red-edge 717 nm; Near-IR: 842 nm)
phe.msp1 <- aggregate(all.pl$msp1[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.msp1) <- gsub("x","msp1",colnames(phe.msp1))
phe.msp2 <- aggregate(all.pl$msp2[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.msp2) <- gsub("x","msp2",colnames(phe.msp2))
phe.msp3 <- aggregate(all.pl$msp3[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.msp3) <- gsub("x","msp3",colnames(phe.msp3))
phe.msp4 <- aggregate(all.pl$msp4[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.msp4) <- gsub("x","msp4",colnames(phe.msp4))
phe.msp5 <- aggregate(all.pl$msp5[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.msp5) <- gsub("x","msp5",colnames(phe.msp5))
phe.SR <- aggregate((all.pl$msp5/all.pl$msp3)[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.SR) <- gsub("x","SR",colnames(phe.SR))
#Note that the below is only an approximation of the SIPI index, to use the exact wavelengths the hyper-
#spectral data needs to be used.
phe.sipi <- aggregate(((all.pl$msp5-all.pl$msp1)/(all.pl$msp5-all.pl$msp3))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.sipi) <- gsub("x","sipi",colnames(phe.sipi))
phe.ndvi <- aggregate(((all.pl$msp5-all.pl$msp3)/(all.pl$msp5+all.pl$msp3))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.ndvi) <- gsub("x","ndvi",colnames(phe.ndvi))
phe.EVI <- aggregate(2.5*((all.pl$msp5 - all.pl$msp3) / ((all.pl$msp5) + (6*all.pl$msp3) - (7.5*all.pl$msp1) + 1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.EVI) <- gsub("x","EVI",colnames(phe.EVI))
phe.ARVI <- aggregate(((all.pl$msp5-(2*all.pl$msp3-all.pl$msp1))/(all.pl$msp5+(2*all.pl$msp3-all.pl$msp1)))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.ARVI) <- gsub("x","ARVI",colnames(phe.ARVI))

phe.sat2.1106.soil <- data.frame(phe.ARVI[-1],phe.blue[-1],phe.coltot[-1],phe.EVI[-1],phe.gborrat[-1],phe.gbrat[-1],phe.green[-1],phe.height[-1],phe.msp1[-1],phe.msp2[-1],phe.msp3[-1],    
                                 phe.msp4[-1],phe.msp5[-1],phe.ndvi[-1],phe.rbrat[-1],phe.red[-1],phe.relblue[-1],phe.relgreen[-1],phe.relred[-1],phe.rbograt[-1],phe.rgobrat[-1],
                                 phe.rgrat[-1],phe.sipi[-1],phe.SR[-1],as.numeric(soil.px),as.numeric(tot.px)) 
phe.sat2.1106.soil <- data.frame(phe.blue$Group.1,as.matrix(phe.sat2.1106.soil))
colnames(phe.sat2.1106.soil)[1] <- "accession"
colnames(phe.sat2.1106.soil)[length(colnames(phe.sat2.1106.soil))-1]<-c("soil.px")
colnames(phe.sat2.1106.soil)[length(colnames(phe.sat2.1106.soil))]<-c("tot.px")

save(phe.sat2.1106.soil,file="phenotypes_soil/obj_pheno.sat_rep2.1106.soil.R4.3.2.out")

###############
#Day 2506 REP 1
###############
load(file="R_objects_plots/obj_drone_ima.rep1_sat_2506_rgb_dsm_msp_red_nd.R4.3.2.out ")

# Here we obtain a logical with TRUE if EVI > 0.25 (plant pixels) and FALSE otherwise.
evi.selc <- 2.5*((all.pl$msp5-all.pl$msp3)/(all.pl$msp5+6*all.pl$msp3+7.5*all.pl$msp1+1))>0.25

# Get pixel counts of accessions and pixel counts after filtering on evi.
soil.px <- table(all.pl[!evi.selc,"use.lk"])
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
phe.red <- aggregate(all.pl$red[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.red) <- gsub("x","red",colnames(phe.red))
phe.green <- aggregate(all.pl$green[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.green) <- gsub("x","green",colnames(phe.green))
phe.blue <- aggregate(all.pl$blue[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.blue) <- gsub("x","blue",colnames(phe.blue))
phe.relred <- aggregate((all.pl$red/apply(all.pl[,c("red","green","blue")],1,sum))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.relred) <- gsub("x","relred",colnames(phe.relred))
phe.relgreen <- aggregate((all.pl$green/apply(all.pl[,c("red","green","blue")],1,sum))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.relgreen) <- gsub("x","relgreen",colnames(phe.relgreen))
phe.relblue <- aggregate((all.pl$blue/apply(all.pl[,c("red","green","blue")],1,sum))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.relblue) <- gsub("x","relblue",colnames(phe.relblue))
phe.rgrat <- aggregate(log2((all.pl$red+1)/(all.pl$green+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr,simplify = T)
colnames(phe.rgrat) <- gsub("x","rgrat",colnames(phe.rgrat))
phe.rbrat <- aggregate(log2((all.pl$red+1)/(all.pl$blue+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.rbrat) <- gsub("x","rbrat",colnames(phe.rbrat))
phe.gbrat <- aggregate(log2((all.pl$green+1)/(all.pl$blue+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.gbrat) <- gsub("x","gbrat",colnames(phe.gbrat))
phe.coltot <- aggregate(apply(all.pl[,c("red","green","blue")],1,sum)[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.coltot) <- gsub("x","coltot",colnames(phe.coltot))
phe.rgobrat <- aggregate(log2((all.pl$red+all.pl$green+1)/(all.pl$blue+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.rgobrat) <- gsub("x","rgobrat",colnames(phe.rgobrat))
phe.rbograt <- aggregate(log2((all.pl$red+all.pl$blue+1)/(all.pl$green+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.rbograt) <- gsub("x","rbograt",colnames(phe.rbograt))
phe.gborrat <- aggregate(log2((all.pl$green+all.pl$blue+1)/(all.pl$red+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.gborrat) <- gsub("x","gborrat",colnames(phe.gborrat))


## Height 
phe.height <- aggregate(all.pl$estheight[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.height) <- gsub("x","height",colnames(phe.height))

## msp MSP (Blue: 475 nm; Green: 560 nm; Red: 668 nm; Red-edge 717 nm; Near-IR: 842 nm)
phe.msp1 <- aggregate(all.pl$msp1[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.msp1) <- gsub("x","msp1",colnames(phe.msp1))
phe.msp2 <- aggregate(all.pl$msp2[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.msp2) <- gsub("x","msp2",colnames(phe.msp2))
phe.msp3 <- aggregate(all.pl$msp3[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.msp3) <- gsub("x","msp3",colnames(phe.msp3))
phe.msp4 <- aggregate(all.pl$msp4[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.msp4) <- gsub("x","msp4",colnames(phe.msp4))
phe.msp5 <- aggregate(all.pl$msp5[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.msp5) <- gsub("x","msp5",colnames(phe.msp5))
phe.SR <- aggregate((all.pl$msp5/all.pl$msp3)[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.SR) <- gsub("x","SR",colnames(phe.SR))
#Note that the below is only an approximation of the SIPI index, to use the exact wavelengths the hyper-
#spectral data needs to be used.
phe.sipi <- aggregate(((all.pl$msp5-all.pl$msp1)/(all.pl$msp5-all.pl$msp3))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.sipi) <- gsub("x","sipi",colnames(phe.sipi))
phe.ndvi <- aggregate(((all.pl$msp5-all.pl$msp3)/(all.pl$msp5+all.pl$msp3))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.ndvi) <- gsub("x","ndvi",colnames(phe.ndvi))
phe.EVI <- aggregate(2.5*((all.pl$msp5 - all.pl$msp3) / ((all.pl$msp5) + (6*all.pl$msp3) - (7.5*all.pl$msp1) + 1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.EVI) <- gsub("x","EVI",colnames(phe.EVI))
phe.ARVI <- aggregate(((all.pl$msp5-(2*all.pl$msp3-all.pl$msp1))/(all.pl$msp5+(2*all.pl$msp3-all.pl$msp1)))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.ARVI) <- gsub("x","ARVI",colnames(phe.ARVI))

## cired, ndre, ndvi, wdvi
phe.cired <- aggregate(all.pl$cired[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.cired) <- gsub("x","cired",colnames(phe.cired))
phe.ndre <- aggregate(all.pl$ndre[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.ndre) <- gsub("x","ndre",colnames(phe.ndre))
phe.ndvi2 <- aggregate(all.pl$ndvi[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.ndvi2) <- gsub("x","ndvi2",colnames(phe.ndvi2))
phe.wdvi <- aggregate(all.pl$wdvi[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.wdvi) <- gsub("x","wdvi",colnames(phe.wdvi))

phe.sat1.2506.soil <- data.frame(phe.ARVI[-1],phe.blue[-1],phe.cired[-1],phe.coltot[-1],phe.EVI[-1],phe.gborrat[-1],phe.gbrat[-1],phe.green[-1],phe.height[-1],phe.msp1[-1],phe.msp2[-1],phe.msp3[-1],    
                                 phe.msp4[-1],phe.msp5[-1],phe.ndre[-1],phe.ndvi[-1],phe.ndvi2[-1],phe.rbrat[-1],phe.red[-1],phe.relblue[-1],phe.relgreen[-1],phe.relred[-1],phe.rbograt[-1],  
                                 phe.rgobrat[-1],phe.rgrat[-1],phe.sipi[-1],phe.SR[-1],phe.wdvi[-1],as.numeric(soil.px),as.numeric(tot.px)) 
phe.sat1.2506.soil <- data.frame(phe.blue$Group.1,as.matrix(phe.sat1.2506.soil))
colnames(phe.sat1.2506.soil)[1] <- "accession"
colnames(phe.sat1.2506.soil)[length(colnames(phe.sat1.2506.soil))-1]<-c("soil.px")
colnames(phe.sat1.2506.soil)[length(colnames(phe.sat1.2506.soil))]<-c("tot.px")

save(phe.sat1.2506.soil,file="phenotypes_soil/obj_pheno.sat_rep1.2506.soil.R4.3.2.out")

###############
#Day 2506 REP 2
###############
load(file="R_objects_plots/obj_drone_ima.rep2_sat_2506_rgb_dsm_msp_red_nd.R4.3.2.out")

# Here we obtain a logical with TRUE if EVI > 0.25 (plant pixels) and FALSE otherwise.
evi.selc <- 2.5*((all.pl$msp5-all.pl$msp3)/(all.pl$msp5+6*all.pl$msp3+7.5*all.pl$msp1+1))>0.25

# Get pixel counts of accessions and pixel counts after filtering on evi.
soil.px <- table(all.pl[!evi.selc,"use.lk"])
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

phe.red <- aggregate(all.pl$red[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.red) <- gsub("x","red",colnames(phe.red))
phe.green <- aggregate(all.pl$green[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.green) <- gsub("x","green",colnames(phe.green))
phe.blue <- aggregate(all.pl$blue[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.blue) <- gsub("x","blue",colnames(phe.blue))
phe.relred <- aggregate((all.pl$red/apply(all.pl[,c("red","green","blue")],1,sum))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.relred) <- gsub("x","relred",colnames(phe.relred))
phe.relgreen <- aggregate((all.pl$green/apply(all.pl[,c("red","green","blue")],1,sum))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.relgreen) <- gsub("x","relgreen",colnames(phe.relgreen))
phe.relblue <- aggregate((all.pl$blue/apply(all.pl[,c("red","green","blue")],1,sum))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.relblue) <- gsub("x","relblue",colnames(phe.relblue))
phe.rgrat <- aggregate(log2((all.pl$red+1)/(all.pl$green+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr,simplify = T)
colnames(phe.rgrat) <- gsub("x","rgrat",colnames(phe.rgrat))
phe.rbrat <- aggregate(log2((all.pl$red+1)/(all.pl$blue+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.rbrat) <- gsub("x","rbrat",colnames(phe.rbrat))
phe.gbrat <- aggregate(log2((all.pl$green+1)/(all.pl$blue+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.gbrat) <- gsub("x","gbrat",colnames(phe.gbrat))
phe.coltot <- aggregate(apply(all.pl[,c("red","green","blue")],1,sum)[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.coltot) <- gsub("x","coltot",colnames(phe.coltot))
phe.rgobrat <- aggregate(log2((all.pl$red+all.pl$green+1)/(all.pl$blue+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.rgobrat) <- gsub("x","rgobrat",colnames(phe.rgobrat))
phe.rbograt <- aggregate(log2((all.pl$red+all.pl$blue+1)/(all.pl$green+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.rbograt) <- gsub("x","rbograt",colnames(phe.rbograt))
phe.gborrat <- aggregate(log2((all.pl$green+all.pl$blue+1)/(all.pl$red+1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.gborrat) <- gsub("x","gborrat",colnames(phe.gborrat))


## Height 
phe.height <- aggregate(all.pl$estheight[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.height) <- gsub("x","height",colnames(phe.height))

## msp MSP (Blue: 475 nm; Green: 560 nm; Red: 668 nm; Red-edge 717 nm; Near-IR: 842 nm)
phe.msp1 <- aggregate(all.pl$msp1[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.msp1) <- gsub("x","msp1",colnames(phe.msp1))
phe.msp2 <- aggregate(all.pl$msp2[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.msp2) <- gsub("x","msp2",colnames(phe.msp2))
phe.msp3 <- aggregate(all.pl$msp3[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.msp3) <- gsub("x","msp3",colnames(phe.msp3))
phe.msp4 <- aggregate(all.pl$msp4[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.msp4) <- gsub("x","msp4",colnames(phe.msp4))
phe.msp5 <- aggregate(all.pl$msp5[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.msp5) <- gsub("x","msp5",colnames(phe.msp5))
phe.SR <- aggregate((all.pl$msp5/all.pl$msp3)[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.SR) <- gsub("x","SR",colnames(phe.SR))
#Note that the below is only an approximation of the SIPI index, to use the exact wavelengths the hyper-
#spectral data needs to be used.
phe.sipi <- aggregate(((all.pl$msp5-all.pl$msp1)/(all.pl$msp5-all.pl$msp3))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.sipi) <- gsub("x","sipi",colnames(phe.sipi))
phe.ndvi <- aggregate(((all.pl$msp5-all.pl$msp3)/(all.pl$msp5+all.pl$msp3))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.ndvi) <- gsub("x","ndvi",colnames(phe.ndvi))
phe.EVI <- aggregate(2.5*((all.pl$msp5 - all.pl$msp3) / ((all.pl$msp5) + (6*all.pl$msp3) - (7.5*all.pl$msp1) + 1))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.EVI) <- gsub("x","EVI",colnames(phe.EVI))
phe.ARVI <- aggregate(((all.pl$msp5-(2*all.pl$msp3-all.pl$msp1))/(all.pl$msp5+(2*all.pl$msp3-all.pl$msp1)))[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.ARVI) <- gsub("x","ARVI",colnames(phe.ARVI))

## cired, ndre, ndvi, wdvi
phe.cired <- aggregate(all.pl$cired[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.cired) <- gsub("x","cired",colnames(phe.cired))
phe.ndre <- aggregate(all.pl$ndre[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.ndre) <- gsub("x","ndre",colnames(phe.ndre))
phe.ndvi2 <- aggregate(all.pl$ndvi[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.ndvi2) <- gsub("x","ndvi2",colnames(phe.ndvi2))
phe.wdvi <- aggregate(all.pl$wdvi[!evi.selc],list(all.pl$use.lk[!evi.selc]),phe.extr)
colnames(phe.wdvi) <- gsub("x","wdvi",colnames(phe.wdvi))

phe.sat2.2506.soil <- data.frame(phe.ARVI[-1],phe.blue[-1],phe.cired[-1],phe.coltot[-1],phe.EVI[-1],phe.gborrat[-1],phe.gbrat[-1],phe.green[-1],phe.height[-1],phe.msp1[-1],phe.msp2[-1],phe.msp3[-1],    
                                 phe.msp4[-1],phe.msp5[-1],phe.ndre[-1],phe.ndvi[-1],phe.ndvi2[-1],phe.rbrat[-1],phe.red[-1],phe.relblue[-1],phe.relgreen[-1],phe.relred[-1],phe.rbograt[-1],  
                                 phe.rgobrat[-1],phe.rgrat[-1],phe.sipi[-1],phe.SR[-1],phe.wdvi[-1],as.numeric(soil.px),as.numeric(tot.px)) 
phe.sat2.2506.soil <- data.frame(phe.blue$Group.1,as.matrix(phe.sat2.2506.soil))
colnames(phe.sat2.2506.soil)[1] <- "accession"
colnames(phe.sat2.2506.soil)[length(colnames(phe.sat2.2506.soil))-1]<-c("soil.px")
colnames(phe.sat2.2506.soil)[length(colnames(phe.sat2.2506.soil))]<-c("tot.px")

save(phe.sat2.2506.soil,file="phenotypes_soil/obj_pheno.sat_rep2.2506.soil.R4.3.2.out")
