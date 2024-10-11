# In this script we'll calculate the difference between the phenotypes on  11-06 and 25-06 by substracting the values
# at 11-06 from those at 25-06. Of course, this will not be that meaningful for all phenotypes, so use common sense
# when interpreting. 

#### Sativa rep 1

load(file = "corrected_phenotypes/obj.phe.cor.sat1.1106.out")
load(file = "corrected_phenotypes/obj.phe.cor.sat1.2506.out")

colnames(phe.sat1.2506)[444:460]
colnames(phe.sat1.2506)[257:273]
colnames(phe.sat1.2506)[223:239]
colnames(phe.sat1.2506)[36:52]

phe.sat1.2506<- phe.sat1.2506[,-c(36:52,223:239,257:273,444:460)]

load(file = "code_growth_rates/growth_sat1.out")
growth.frame.sat1 <- growth.frame.sat1[order(as.numeric(substring(growth.frame.sat1$accession,3))),]

diff.sat1 <- cbind(phe.sat1.1106$accession,phe.sat1.2506[-1]-phe.sat1.1106[-1],growth.frame.sat1$growth)

colnames(diff.sat1)[ncol(diff.sat1)] <- "growth_width"

height_frame <- data.frame(Accession = 1:194,Height = diff.sat1$height.mean)

height_plot <- ggplot(height_frame, aes(x = Accession,y = Height))+
  geom_point() +
  geom_smooth(col = "black",method = "gam") +
  theme_minimal()+
  theme(text = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  xlab("Accession (LK nr)")+
  ylab("Difference height")+
  xlim(0,194)

png(filename = "diff_tp_plots/Mean_plant_height_sat1.png",width = 750, height = 500)
height_plot
dev.off()

colnames(diff.sat1)[1] <- "accession"

save(diff.sat1, file = "diff_tp_phe/sat1.diff.phe.out")
write.xlsx(diff.sat1,file="diff_tp_phe/phe_sat1_diff.xlsx",rownames=T)


#### Sativa rep 2

load(file = "corrected_phenotypes/obj.phe.cor.sat2.1106.out")
load(file = "corrected_phenotypes/obj.phe.cor.sat2.2506.out")

phe.sat2.2506<- phe.sat2.2506[,-c(36:52,223:239,257:273,444:460)]

load(file = "code_growth_rates/growth_sat2.out")
growth.frame.sat2 <- growth.frame.sat2[order(as.numeric(substring(growth.frame.sat2$accession,3))),]

diff.sat2 <- cbind(phe.sat2.1106$accession,phe.sat2.2506[-1]-phe.sat2.1106[-1],growth.frame.sat2$growth)

colnames(diff.sat2)[ncol(diff.sat2)] <- "growth_width"

height_frame <- data.frame(Accession = 1:194,Height = diff.sat2$height.mean)

height_plot <- ggplot(height_frame, aes(x = Accession,y = Height))+
  geom_point() +
  geom_smooth(col = "black",method = "gam") +
  theme_minimal()+
  theme(text = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  xlab("Accession (LK nr)")+
  ylab("Difference height")+
  xlim(0,194)

png(filename = "diff_tp_plots/Mean_plant_height_sat2.png",width = 750, height = 500)
height_plot
dev.off()

colnames(diff.sat2)[1] <- "accession"

save(diff.sat2, file = "diff_tp_phe/sat2.diff.phe.out")
write.xlsx(diff.sat2,file="diff_tp_phe/phe_sat2_diff.xlsx",rownames=T)