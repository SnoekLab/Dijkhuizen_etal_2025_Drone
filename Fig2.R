#####################################################################
## Script to make figure 2 boxplot of phenotypic values of lettuce types
############################################################################

library(ggplot2)
library(viridis)
library(openxlsx)
library(cowplot)
sat.full <- read.xlsx(xlsxFile = "./DroneData2023/Necessary_data.xlsx", sheet = "Phenotypes")

sat.full.colnames <- sat.full$TraitID
sat.full <- data.frame(t(sat.full[,-(1:4)]))
colnames(sat.full) <- sat.full.colnames


LK.info <- read.xlsx(xlsxFile = "./DroneData2023/Necessary_data.xlsx", sheet = "LK_lines")

LK.info <- LK.info[LK.info$LKID%in%rownames(sat.full),]

height.mean.1106 <- sat.full$height.mean.1106
height.mean.2506 <- sat.full$height.mean.2506
height.mean.dira <- sat.full$height.mean.dira

Pheno.value <- c(height.mean.1106,height.mean.2506,height.mean.dira)
Day <- rep(c(rep("Day 78",nrow(LK.info)),rep("Day 93",nrow(LK.info)),rep("Ratio",nrow(LK.info))))
LKid <- rep(LK.info$LKID,3)
Type <- rep(LK.info$Subgroup_SB,3)
## figure 

to.pl <- data.frame(LKid,Type,Day,Pheno.value)
as.factor(to.pl$Type)

highlightlk <- c("LK061","LK147","LK087","LK183","LK108","LK108","LK198","LK194") # highlight genotypes shown in figure 1
highlightlk
to.pl.hl <- to.pl[to.pl$LKid %in% highlightlk & to.pl$Day != "Ratio",]

fig.height.mean <- ggplot(to.pl[to.pl$Day != "Ratio",])+
  geom_boxplot(aes(Type,Pheno.value,col=Day,fill=Type))+
  geom_point(data = to.pl.hl,aes(as.numeric(as.factor(to.pl.hl$Type))+rep(c(-0.2,0.2),each=7),Pheno.value,col=Day),fill="grey70",shape=21,size=3)+
  scale_y_continuous(limits = c(0,0.7))+
  scale_color_manual(values = c("black","grey60"),guide = guide_legend(direction = "horizontal"))+
  scale_fill_manual(values = turbo(8)[2:8],guide = "none")+
  ylab("Mean Height (m)")+
  theme_cowplot()+
  theme(panel.background = element_rect(linewidth = 0.2,color="black"),
        axis.line = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        #axis.text.x = element_text(angle = 45,size=12,hjust = 1,vjust = 1),
        axis.text.x = element_blank(),
        axis.text.y = element_text(lineheight =8),
        axis.title.x = element_blank(),
        panel.spacing.x = unit(1,"mm"),
        panel.spacing.y = unit(1,"mm"),
        strip.text.y = element_text(face="bold", size=9),
        panel.grid.major = element_line(color = "grey2",linewidth =  0.1,linetype = 3),
        legend.position = c(0,0.95),
        legend.title = element_blank())
  

fig.height.mean

to.pl.hl <- to.pl[to.pl$LKid %in% highlightlk & to.pl$Day == "Ratio",]
fig.height.mean.rat <- ggplot(to.pl[to.pl$Day == "Ratio",])+
  geom_boxplot(aes(Type,Pheno.value,col=Day,fill=Type))+
  geom_point(data = to.pl.hl,aes(Type,Pheno.value,col=Day),fill="grey70",shape=21,size=3)+
  scale_y_continuous()+
  scale_color_manual(values = c("black","grey60"),guide = "none")+
  scale_fill_manual(values = turbo(8)[2:8],guide = "none")+
  ylab("Log2 Mean Height ratio")+
  theme_cowplot()+
  theme(panel.background = element_rect(linewidth = 0.2,color="black"),
        axis.line = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        axis.text.x = element_text(angle = 45,size=12,hjust = 1,vjust = 1),
        axis.text.y = element_text(lineheight =8),
        axis.title.x = element_blank(),
        panel.spacing.x = unit(1,"mm"),
        panel.spacing.y = unit(1,"mm"),
        strip.text.y = element_text(face="bold", size=9),
        panel.grid.major = element_line(color = "grey2",linewidth =  0.1,linetype = 3),
        legend.position = c(0,0.95),
        legend.title = element_blank())


fig.height.mean.rat

plot_grid(fig.height.mean,fig.height.mean.rat)


msp4.mean.1106 <- sat.full$msp4.mean.1106
msp4.mean.2506 <- sat.full$msp4.mean.2506
msp4.mean.dira <- sat.full$msp4.mean.dira


Pheno.value <- c(msp4.mean.1106,msp4.mean.2506,msp4.mean.dira)
Day <- rep(c(rep("Day 78",nrow(LK.info)),rep("Day 93",nrow(LK.info)),rep("Ratio",nrow(LK.info))))
LKid <- rep(LK.info$LKID,3)
Type <- rep(LK.info$Subgroup_SB,3)
## figure 

to.pl <- data.frame(LKid,Type,Day,Pheno.value)
to.pl.hl <- to.pl[to.pl$LKid %in% highlightlk & to.pl$Day != "Ratio",]

fig.msp4.mean <- ggplot(to.pl[to.pl$Day != "Ratio",])+
  geom_boxplot(aes(Type,Pheno.value,col=Day,fill=Type))+
  geom_point(data = to.pl.hl,aes(as.numeric(as.factor(to.pl.hl$Type))+rep(c(-0.2,0.2),each=7),Pheno.value,col=Day),fill="grey70",shape=21,size=3)+
  scale_y_continuous(limits = c(0.1,0.45))+
  scale_color_manual(values = c("black","grey60"),guide = guide_legend(direction = "horizontal"))+
  scale_fill_manual(values = turbo(8)[2:8],guide = "none")+
  ylab("Mean Red Edge")+
  theme_cowplot()+
  theme(panel.background = element_rect(linewidth = 0.2,color="black"),
        axis.line = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        # axis.text.x = element_text(angle = 45,size=12,hjust = 1,vjust = 1),
        axis.text.x = element_blank(),
        axis.text.y = element_text(),
        axis.title.x = element_blank(),
        panel.spacing.x = unit(1,"mm"),
        panel.spacing.y = unit(1,"mm"),
        strip.text.y = element_text(face="bold", size=9),
        panel.grid.major = element_line(color = "grey2",linewidth =  0.1,linetype = 3),
        legend.position = c(0,0.95),
        legend.title = element_blank())


fig.msp4.mean

to.pl.hl <- to.pl[to.pl$LKid %in% highlightlk & to.pl$Day == "Ratio",]

fig.msp4.mean.rat <- ggplot(to.pl[to.pl$Day == "Ratio",])+
  geom_boxplot(aes(Type,Pheno.value,col=Day,fill=Type))+
  geom_point(data = to.pl.hl,aes(Type,Pheno.value,col=Day),fill="grey70",shape=21,size=3)+
  scale_y_continuous()+
  scale_color_manual(values = c("black","grey60"),guide = "none")+
  scale_fill_manual(values = turbo(8)[2:8],guide = "none")+
  ylab("Log2 Mean Red Edge ratio")+
  theme_cowplot()+
  theme(panel.background = element_rect(linewidth = 0.2,color="black"),
        axis.line = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        axis.text.x = element_text(angle = 45,size=12,hjust = 1,vjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_blank(),
        panel.spacing.x = unit(1,"mm"),
        panel.spacing.y = unit(1,"mm"),
        strip.text.y = element_text(face="bold", size=9),
        panel.grid.major = element_line(color = "grey2",linewidth =  0.1,linetype = 3),
        legend.position = c(0,0.95),
        legend.title = element_blank())


fig.msp4.mean.rat

plot_grid(fig.msp4.mean,fig.msp4.mean.rat)




gbrat.mean.1106 <- sat.full$gbrat.mean.1106
gbrat.mean.2506 <- sat.full$gbrat.mean.2506
gbrat.mean.dira <- sat.full$gbrat.mean.dira



Pheno.value <- c(gbrat.mean.1106,gbrat.mean.2506,gbrat.mean.dira)
Day <- rep(c(rep("Day 78",nrow(LK.info)),rep("Day 93",nrow(LK.info)),rep("Ratio",nrow(LK.info))))
LKid <- rep(LK.info$LKID,3)
Type <- rep(LK.info$Subgroup_SB,3)
## figure 

to.pl <- data.frame(LKid,Type,Day,Pheno.value)
to.pl.hl <- to.pl[to.pl$LKid %in% highlightlk & to.pl$Day != "Ratio",]

fig.gbrat.mean <- ggplot(to.pl[to.pl$Day != "Ratio",])+
  geom_boxplot(aes(Type,Pheno.value,col=Day,fill=Type))+
  geom_point(data = to.pl.hl,aes(as.numeric(as.factor(to.pl.hl$Type))+rep(c(-0.2,0.2),each=7),Pheno.value,col=Day),fill="grey70",shape=21,size=3)+
  scale_y_continuous(limits = c(-0.8,2))+
  scale_color_manual(values = c("black","grey60"),guide = guide_legend(direction = "horizontal"))+
  scale_fill_manual(values = turbo(8)[2:8],guide = "none")+
  ylab("Mean Log2 Green/Blue ratio")+
  theme_cowplot()+
  theme(panel.background = element_rect(linewidth = 0.2,color="black"),
        axis.line = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        # axis.text.x = element_text(angle = 45,size=12,hjust = 1,vjust = 1),
        axis.text.x = element_blank(),
        axis.text.y = element_text(),
        axis.title.x = element_blank(),
        panel.spacing.x = unit(1,"mm"),
        panel.spacing.y = unit(1,"mm"),
        strip.text.y = element_text(face="bold", size=9),
        panel.grid.major = element_line(color = "grey2",linewidth =  0.1,linetype = 3),
        legend.position = c(0,0.95),
        legend.title = element_blank())


fig.gbrat.mean

to.pl.hl <- to.pl[to.pl$LKid %in% highlightlk & to.pl$Day == "Ratio",]

fig.gbrat.mean.rat <- ggplot(to.pl[to.pl$Day == "Ratio",])+
  geom_boxplot(aes(Type,Pheno.value,col=Day,fill=Type))+
  geom_point(data = to.pl.hl,aes(Type,Pheno.value,col=Day),fill="grey70",shape=21,size=3)+
  scale_y_continuous()+
  scale_color_manual(values = c("black","grey60"),guide = "none")+
  scale_fill_manual(values = turbo(8)[2:8],guide = "none")+
  ylab("Log2 Log2 Green/Blue ratio ratio")+
  theme_cowplot()+
  theme(panel.background = element_rect(linewidth = 0.2,color="black"),
        axis.line = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        axis.text.x = element_text(angle = 45,size=12,hjust = 1,vjust = 1),
        axis.text.y = element_text(),
        axis.title.x = element_blank(),
        panel.spacing.x = unit(1,"mm"),
        panel.spacing.y = unit(1,"mm"),
        strip.text.y = element_text(face="bold", size=9),
        panel.grid.major = element_line(color = "grey2",linewidth =  0.1,linetype = 3),
        legend.position = c(0,0.95),
        legend.title = element_blank())


fig.gbrat.mean.rat

plot_grid(fig.gbrat.mean,fig.gbrat.mean.rat)



plot_grid(fig.height.mean,fig.msp4.mean,fig.gbrat.mean,
          fig.height.mean.rat,fig.msp4.mean.rat,fig.gbrat.mean.rat,
          ncol=3,align = "v",labels = c("A","B","C","","",""))


png(filename = "figure2wide.png",width =1500,height = 1125)
plot_grid(fig.height.mean,fig.msp4.mean,fig.gbrat.mean,
          fig.height.mean.rat,fig.msp4.mean.rat,fig.gbrat.mean.rat,
          ncol=3,align = "v",labels = c("A","B","C","","",""))
dev.off()
