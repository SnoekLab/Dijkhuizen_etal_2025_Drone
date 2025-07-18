# Script to create supplementary figure to show unclumped SNPs for 5 example traits

library(ggplot2)
library(cowplot)
library(readxl)


full <- read_excel("D:/Drone-paper/Supplemental_data.xlsx", sheet = "FullSNPs5traits")

man <- ggplot(full[full$pval>2,],aes(Position_MB,pval))+
  geom_point(aes(col=pval>7), size = 3)+
  geom_hline(yintercept = 7,col="red",linewidth=0.1)+
  scale_x_continuous(breaks = c(0,50,100,150,200,250,300,350),expand = c(0.05,0.05))+
  scale_color_manual(values = c("grey70","black"))+
  xlab("Position (Mbp)") + ylab(bquote(-log[10] (p)))+
  facet_grid(Trait~Chromosome,scales = "free_x",space = "free_x")+
  theme_cowplot()+
  theme(panel.background = element_rect(linewidth = 0.2,color="black"),
        axis.line = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        axis.text.x = element_text(angle = 90,size=25,hjust = 1,vjust = 0.5),
        axis.text.y = element_text(lineheight =7, size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25),
        panel.spacing.x = unit(3.5,"mm"),
        strip.text = element_text(face="bold", size=25),
        panel.grid.major = element_line(color = "grey2",linewidth =  0.1,linetype = 3),
        legend.position = "none")

png(filename = "man_full_gwas.png",width = 1500,height = 1500)
man
dev.off()