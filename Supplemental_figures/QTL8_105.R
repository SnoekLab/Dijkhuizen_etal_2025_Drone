#Creates extra supplemental image of the peak on chromosome 8.105
library(openxlsx)
library(ggplot2)
library(cowplot)
library(ggrastr)


#Load in the pvalues
pvalues <- read.xlsx(xlsxFile =  "D:/Drone-paper/Supplemental_data.xlsx", sheet =  "pvalues")

#Load in the accessions we have
accessions <- c("LK001","LK002","LK003","LK004","LK005","LK006","LK007","LK008","LK009","LK010","LK011","LK012","LK013","LK014","LK015","LK016","LK017","LK018","LK019","LK020","LK021","LK022","LK023","LK024","LK025",
                "LK026","LK027","LK028","LK029","LK030","LK031","LK032","LK033","LK034","LK035","LK036","LK037","LK038","LK039","LK040","LK041","LK042","LK043","LK044","LK045","LK046","LK047","LK048","LK049","LK050",
                "LK051","LK052","LK053","LK054","LK055","LK056","LK057","LK058","LK059","LK060","LK061","LK062","LK063","LK064","LK065","LK066","LK067","LK068","LK069","LK070","LK071","LK072","LK073","LK074","LK075",
                "LK076","LK077","LK078","LK079","LK080","LK081","LK082","LK083","LK084","LK086","LK087","LK088","LK089","LK091","LK092","LK093","LK094","LK095","LK097","LK098","LK099","LK100","LK101","LK102","LK103",
                "LK104","LK105","LK106","LK107","LK108","LK109","LK110","LK111","LK112","LK113","LK114","LK115","LK116","LK117","LK118","LK119","LK120","LK121","LK122","LK123","LK124","LK125","LK126","LK127","LK128",
                "LK129","LK130","LK131","LK132","LK133","LK134","LK135","LK136","LK137","LK138","LK139","LK140","LK141","LK142","LK143","LK144","LK145","LK146","LK147","LK148","LK149","LK150","LK151","LK152","LK153",
                "LK154","LK155","LK156","LK157","LK158","LK159","LK160","LK161","LK162","LK163","LK164","LK165","LK166","LK168","LK169","LK170","LK171","LK172","LK173","LK174","LK175","LK176","LK177","LK178","LK179",
                "LK180","LK181","LK182","LK183","LK184","LK185","LK186","LK187","LK189","LK191","LK192","LK193","LK194","LK195","LK196","LK197","LK198","LK199","LK200")

#Load in the SNPmap
usemat <- load("GWAS_objects/obj_all.ALTREF_SNP_matrix_sat_2024_R4.3.2.out")
usemat <- eval(parse(text=usemat))
usemat <- usemat[usemat$Count1 < 9,]


# Find position peak of interest
snp <- pvalues[pvalues$Chromosome == 8,]
snp$Group <- ifelse(is.na(snp$mean_clustering), "extended descriptives", "mean trait")

manhattan <- ggplot() +
  geom_point(data = subset(snp, Group == "extended descriptives"),
             aes(x = Position, y = Pval, color = Group), alpha = 0.8,
             show.legend = TRUE) +
  geom_point(data = subset(snp, Group == "mean trait"),
             aes(x = Position, y = Pval, color = Group),
             show.legend = TRUE) +
  scale_color_manual(values = c("mean trait" = "black", 
                                "extended descriptives" = "darkgrey")) +
  geom_rect(aes(xmin = 103, xmax = 108, ymin = 38, ymax = 40),
            fill = alpha("darkgrey", 0), color = "cyan2", size = 1)+
  theme_bw() +
  labs(color = "Trait source")
manhattan  
ggsave("Script_per_figure/Figures/Sup7Manhatlegend.png", width = 15, height = 5, units = "cm") #Version with legend


manhattan <- ggplot() +
  geom_point(data = subset(snp, Group == "extended descriptives"),
             aes(x = Position, y = Pval, color = Group), alpha = 0.8,
             show.legend = FALSE) +
  geom_point(data = subset(snp, Group == "mean trait"),
             aes(x = Position, y = Pval, color = Group),
             show.legend = FALSE) +
  scale_color_manual(values = c("mean trait" = "black", 
                                "extended descriptives" = "darkgrey")) +
  geom_rect(aes(xmin = 103, xmax = 108, ymin = 38, ymax = 40),
            fill = alpha("darkgrey", 0), color = "cyan2", size = 1)+
  theme_bw() +
  labs(color = "Trait source")
manhattan  

ggsave("Script_per_figure/Figures/Sup7Manhat.png", width = 15, height = 5, units = "cm") #Version without legend

#Now select most significant SNP
snp <- snp[snp$Pval > 6,]
snp <- snp[snp$Position > 100,]
snp <- snp[snp$Position < 110,]
snp <- snp[snp$Pval == max(snp$Pval),]
position <- snp$Position
position <- position * 1000000 # convert from Mbp to bp
position <- round(position, digits = 0)
position

# Find most significant SNP at that location
loc <- usemat[usemat$CHR == 8,]
#loc <- loc[loc$POS > position-1,]
#loc <- loc[loc$POS < position+1,]
loc <- loc[loc$POS == position,]
loc
loc <- loc[,-c(1:10)]

# Remove accesions we don't have
loc <- loc[,colnames(loc)%in%accessions]

AA <- colnames(loc[,loc == 0])
AB <- colnames(loc[,loc == 1])
BB <- colnames(loc[,loc == 2]) 


AA.examples <- c("LK063", "LK173", "LK151", "LK100", "LK039", "LK189", "LK120", "LK119", "LK180", "LK078") # Random examples of AA genotype. Other examples are just as viable.
BB.examples <- BB

#Load in the image data
setwd("C:/Users/RensD/OneDrive/Work/RA/Drone_paper/Bram")
load(file="Robjects_allpl/obj_all.pl_sat_rep1_0611_rgb_dsm_msp.out") # day1.
load(file="Robjects_allpl/obj_all.pl_sat_rep1_2506_rgb_dsm_msp_red_nd.out") # day2
colnames(all.pl)[10] <- "msp1"

#Rotate the entire plot
xy <- data.matrix(all.pl[,c(1,2)])
xyr <- adespatial::rotation(xy, -18.5*pi/180)
all.pl$x <- xyr[,1]
all.pl$y <- xyr[,2]

# This code can be used to show and save individual plots
AA1 <- all.pl[all.pl$use.lk ==  AA.examples[10],]
use.rgb <- rgb(red=AA1$red,green = AA1$green,blue = AA1$blue,maxColorValue = 255)
AA1.plot <- ggplot(AA1)+
  geom_jitter(aes(x,y),col=use.rgb)+
  theme_void()
png("Script_per_figure/Figures/AA10.png")
AA1.plot
dev.off()

BB1 <- all.pl[all.pl$use.lk ==  BB.examples[9],]
use.rgb <- rgb(red=BB1$red,green = BB1$green,blue = BB1$blue,maxColorValue = 255)
BB1.plot <- ggplot(BB1)+
  geom_jitter(aes(x,y),col=use.rgb)+
  theme_void()
png("Script_per_figure/Figures/BB9.png")
BB1.plot
dev.off()

##### Make all the plots
AA1 <- all.pl[all.pl$use.lk == AA.examples[1] ,]
use.rgb <- rgb(red=AA1$red,green = AA1$green,blue = AA1$blue,maxColorValue = 255)
AA1.plot <- ggplot(AA1)+
  geom_jitter_rast(aes(x,y),col=use.rgb, raster.dpi = 100)+
  theme_void()

AA2 <- all.pl[all.pl$use.lk == AA.examples[2] ,]
use.rgb <- rgb(red=AA2$red,green = AA2$green,blue = AA2$blue,maxColorValue = 255)
AA2.plot <- ggplot(AA2)+
  geom_jitter_rast(aes(x,y),col=use.rgb, raster.dpi = 100)+
  theme_void()

AA3 <- all.pl[all.pl$use.lk == AA.examples[3] ,]
use.rgb <- rgb(red=AA3$red,green = AA3$green,blue = AA3$blue,maxColorValue = 255)
AA3.plot <- ggplot(AA3)+
  geom_jitter_rast(aes(x,y),col=use.rgb, raster.dpi = 100)+
  theme_void()

AA4 <- all.pl[all.pl$use.lk == AA.examples[4] ,]
use.rgb <- rgb(red=AA4$red,green = AA4$green,blue = AA4$blue,maxColorValue = 255)
AA4.plot <- ggplot(AA4)+
  geom_jitter_rast(aes(x,y),col=use.rgb, raster.dpi = 100)+
  theme_void()

AA5 <- all.pl[all.pl$use.lk == AA.examples[5] ,]
use.rgb <- rgb(red=AA5$red,green = AA5$green,blue = AA5$blue,maxColorValue = 255)
AA5.plot <- ggplot(AA5)+
  geom_jitter_rast(aes(x,y),col=use.rgb, raster.dpi = 100)+
  theme_void()

AA6 <- all.pl[all.pl$use.lk == AA.examples[6] ,]
use.rgb <- rgb(red=AA6$red,green = AA6$green,blue = AA6$blue,maxColorValue = 255)
AA6.plot <- ggplot(AA6)+
  geom_jitter_rast(aes(x,y),col=use.rgb, raster.dpi = 100)+
  theme_void()

AA7 <- all.pl[all.pl$use.lk == AA.examples[7] ,]
use.rgb <- rgb(red=AA7$red,green = AA7$green,blue = AA7$blue,maxColorValue = 255)
AA7.plot <- ggplot(AA7)+
  geom_jitter_rast(aes(x,y),col=use.rgb, raster.dpi = 100)+
  theme_void()

AA8 <- all.pl[all.pl$use.lk == AA.examples[8] ,]
use.rgb <- rgb(red=AA8$red,green = AA8$green,blue = AA8$blue,maxColorValue = 255)
AA8.plot <- ggplot(AA8)+
  geom_jitter_rast(aes(x,y),col=use.rgb, raster.dpi = 100)+
  theme_void()

AA9 <- all.pl[all.pl$use.lk == AA.examples[9] ,]
use.rgb <- rgb(red=AA9$red,green = AA9$green,blue = AA9$blue,maxColorValue = 255)
AA9.plot <- ggplot(AA9)+
  geom_jitter_rast(aes(x,y),col=use.rgb, raster.dpi = 100)+
  theme_void()

AA10 <- all.pl[all.pl$use.lk == AA.examples[10] ,]
use.rgb <- rgb(red=AA10$red,green = AA10$green,blue = AA10$blue,maxColorValue = 255)
AA10.plot <- ggplot(AA10)+
  geom_jitter_rast(aes(x,y),col=use.rgb, raster.dpi = 100)+
  theme_void()

AA.plots <- plot_grid(AA1.plot, AA2.plot, AA3.plot, AA4.plot, AA5.plot, AA6.plot,
                      AA7.plot, AA8.plot, AA9.plot, AA10.plot, nrow = 2)
AA.plots

# move on to BB
BB1 <- all.pl[all.pl$use.lk == BB[1] ,]
use.rgb <- rgb(red=BB1$red,green = BB1$green,blue = BB1$blue,maxColorValue = 255)
BB1.plot <- ggplot(BB1)+
  geom_jitter_rast(aes(x,y),col=use.rgb, raster.dpi = 100)+
  theme_void()

BB2 <- all.pl[all.pl$use.lk == BB[2] ,]
use.rgb <- rgb(red=BB2$red,green = BB2$green,blue = BB2$blue,maxColorValue = 255)
BB2.plot <- ggplot(BB2)+
  geom_jitter_rast(aes(x,y),col=use.rgb, raster.dpi = 100)+
  theme_void()

BB3 <- all.pl[all.pl$use.lk == BB[3] ,]
use.rgb <- rgb(red=BB3$red,green = BB3$green,blue = BB3$blue,maxColorValue = 255)
BB3.plot <- ggplot(BB3)+
  geom_jitter_rast(aes(x,y),col=use.rgb, raster.dpi = 100)+
  theme_void()

BB4 <- all.pl[all.pl$use.lk == BB[4] ,]
use.rgb <- rgb(red=BB4$red,green = BB4$green,blue = BB4$blue,maxColorValue = 255)
BB4.plot <- ggplot(BB4)+
  geom_jitter_rast(aes(x,y),col=use.rgb, raster.dpi = 100)+
  theme_void()

BB5 <- all.pl[all.pl$use.lk == BB[5] ,]
use.rgb <- rgb(red=BB5$red,green = BB5$green,blue = BB5$blue,maxColorValue = 255)
BB5.plot <- ggplot(BB5)+
  geom_jitter_rast(aes(x,y),col=use.rgb, raster.dpi = 100)+
  theme_void()

# For the BB we do all 10 since there are only 10 total
BB6 <- all.pl[all.pl$use.lk == BB[6] ,]
use.rgb <- rgb(red=BB6$red,green = BB6$green,blue = BB6$blue,maxColorValue = 255)
BB6.plot <- ggplot(BB6)+
  geom_jitter_rast(aes(x,y),col=use.rgb, raster.dpi = 100)+
  theme_void()

BB7 <- all.pl[all.pl$use.lk == BB[7] ,]
use.rgb <- rgb(red=BB7$red,green = BB7$green,blue = BB7$blue,maxColorValue = 255)
BB7.plot <- ggplot(BB7)+
  geom_jitter_rast(aes(x,y),col=use.rgb, raster.dpi = 100)+
  theme_void()

BB8 <- all.pl[all.pl$use.lk == BB[8] ,]
use.rgb <- rgb(red=BB8$red,green = BB8$green,blue = BB8$blue,maxColorValue = 255)
BB8.plot <- ggplot(BB8)+
  geom_jitter_rast(aes(x,y),col=use.rgb, raster.dpi = 100)+
  theme_void()

BB9 <- all.pl[all.pl$use.lk == BB[9] ,]
use.rgb <- rgb(red=BB9$red,green = BB9$green,blue = BB9$blue,maxColorValue = 255)
BB9.plot <- ggplot(BB9)+
  geom_jitter_rast(aes(x,y),col=use.rgb, raster.dpi = 100)+
  theme_void()

BB.plots <- plot_grid(BB1.plot, BB2.plot, BB3.plot, BB4.plot, BB5.plot,
                      BB6.plot, BB7.plot, BB8.plot, BB9.plot, nrow = 2)
BB.plots

#########
# Please note that the figure here is not the exact same as in the manuscript.
# To make the figure in the manuscript we exported the individual images and made 
# them more rectangular using the perspective shift tool in GNU image manipulator.
# And then used GNU to add the labels and organize the images.

complete.plot <- plot_grid(manhattan, AA.plots, BB.plots, rel_heights = c(0.66,1,1),
                           ncol = 1)
complete.plot

ggsave("Script_per_figure/Figures/figure8105.png", width = 15, height = 20, units = "cm" )