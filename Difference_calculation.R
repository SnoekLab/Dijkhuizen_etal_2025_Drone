library(openxlsx)

########### First substracting

### Sativa rep 1 differences

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

colnames(diff.sat1)[1] <- "accession"

save(diff.sat1, file = "Differences_all/sat1.diff.phe.out")
write.xlsx(diff.sat1,file="Differences_all/phe_sat1_diff.xlsx",rownames=T)

#### Sativa rep 2

load(file = "corrected_phenotypes/obj.phe.cor.sat2.1106.out")
load(file = "corrected_phenotypes/obj.phe.cor.sat2.2506.out")

phe.sat2.2506<- phe.sat2.2506[,-c(36:52,223:239,257:273,444:460)]

load(file = "code_growth_rates/growth_sat2.out")
growth.frame.sat2 <- growth.frame.sat2[order(as.numeric(substring(growth.frame.sat2$accession,3))),]

diff.sat2 <- cbind(phe.sat2.1106$accession,phe.sat2.2506[-1]-phe.sat2.1106[-1],growth.frame.sat2$growth)

colnames(diff.sat2)[ncol(diff.sat2)] <- "growth_width"

colnames(diff.sat2)[1] <- "accession"

save(diff.sat2, file = "Differences_all/sat2.diff.phe.out")
write.xlsx(diff.sat2,file="Differences_all/phe_sat2_diff.xlsx",rownames=T)

#### Serriola rep 1

load(file = "corrected_phenotypes/obj.phe.cor.ser1.1106.out")
load(file = "corrected_phenotypes/obj.phe.cor.ser1.2506.out")

phe.ser1.2506<- phe.ser1.2506[,-c(36:52,223:239,257:273,444:460)]

load(file = "code_growth_rates/growth_ser1.out")
growth.frame.ser1 <- growth.frame.ser1[order(as.numeric(substring(growth.frame.ser1$accession,3))),]

diff.ser1 <- cbind(phe.ser1.1106$accession,phe.ser1.2506[-1]-phe.ser1.1106[-1],growth.frame.ser1$growth)

colnames(diff.ser1)[ncol(diff.ser1)] <- "growth_width"

colnames(diff.ser1)[1] <- "accession"

save(diff.ser1, file = "Differences_all/ser1.diff.phe.out")
write.xlsx(diff.ser1,file="Differences_all/phe_ser1_diff.xlsx",rownames=T)

#### serriola rep 2

load(file = "corrected_phenotypes/obj.phe.cor.ser2.1106.out")
load(file = "corrected_phenotypes/obj.phe.cor.ser2.2506.out")

phe.ser2.2506<- phe.ser2.2506[,-c(36:52,223:239,257:273,444:460)]

load(file = "code_growth_rates/growth_ser2.out")
growth.frame.ser2 <- growth.frame.ser2[order(as.numeric(substring(growth.frame.ser2$accession,3))),]

diff.ser2 <- cbind(phe.ser2.1106$accession,phe.ser2.2506[-1]-phe.ser2.1106[-1],growth.frame.ser2$growth)

colnames(diff.ser2)[ncol(diff.ser2)] <- "growth_width"

colnames(diff.ser2)[1] <- "accession"

save(diff.ser2, file = "Differences_all/ser2.diff.phe.out")
write.xlsx(diff.ser2,file="Differences_all/phe_ser2_diff.xlsx",rownames=T)

########### Now ratios

### Sativa rep 1 differences

load(file = "corrected_phenotypes/obj.phe.cor.sat1.1106.out")
load(file = "corrected_phenotypes/obj.phe.cor.sat1.2506.out")

colnames(phe.sat1.2506)[444:460]
colnames(phe.sat1.2506)[257:273]
colnames(phe.sat1.2506)[223:239]
colnames(phe.sat1.2506)[36:52]

phe.sat1.2506<- phe.sat1.2506[,-c(36:52,223:239,257:273,444:460)]

load(file = "code_growth_rates/growth_sat1.out")
growth.frame.sat1 <- growth.frame.sat1[order(as.numeric(substring(growth.frame.sat1$accession,3))),]

dira.sat1 <- cbind(phe.sat1.1106$accession,phe.sat1.2506[-1]/phe.sat1.1106[-1],growth.frame.sat1$growth)

colnames(dira.sat1)[ncol(dira.sat1)] <- "growth_width"

colnames(dira.sat1)[1] <- "accession"

save(dira.sat1, file = "Differences_all/sat1.dira.phe.out")
write.xlsx(dira.sat1,file="Differences_all/phe_sat1_dira.xlsx",rownames=T)

#### Sativa rep 2

load(file = "corrected_phenotypes/obj.phe.cor.sat2.1106.out")
load(file = "corrected_phenotypes/obj.phe.cor.sat2.2506.out")

phe.sat2.2506<- phe.sat2.2506[,-c(36:52,223:239,257:273,444:460)]

load(file = "code_growth_rates/growth_sat2.out")
growth.frame.sat2 <- growth.frame.sat2[order(as.numeric(substring(growth.frame.sat2$accession,3))),]

dira.sat2 <- cbind(phe.sat2.1106$accession,phe.sat2.2506[-1]/phe.sat2.1106[-1],growth.frame.sat2$growth)

colnames(dira.sat2)[ncol(dira.sat2)] <- "growth_width"

colnames(dira.sat2)[1] <- "accession"

save(dira.sat2, file = "Differences_all/sat2.dira.phe.out")
write.xlsx(dira.sat2,file="Differences_all/phe_sat2_dira.xlsx",rownames=T)

#### Serriola rep 1

load(file = "corrected_phenotypes/obj.phe.cor.ser1.1106.out")
load(file = "corrected_phenotypes/obj.phe.cor.ser1.2506.out")

phe.ser1.2506<- phe.ser1.2506[,-c(36:52,223:239,257:273,444:460)]

load(file = "code_growth_rates/growth_ser1.out")
growth.frame.ser1 <- growth.frame.ser1[order(as.numeric(substring(growth.frame.ser1$accession,3))),]

dira.ser1 <- cbind(phe.ser1.1106$accession,phe.ser1.2506[-1]/phe.ser1.1106[-1],growth.frame.ser1$growth)

colnames(dira.ser1)[ncol(dira.ser1)] <- "growth_width"

colnames(dira.ser1)[1] <- "accession"

save(dira.ser1, file = "Differences_all/ser1.dira.phe.out")
write.xlsx(dira.ser1,file="Differences_all/phe_ser1_dira.xlsx",rownames=T)

#### serriola rep 2

load(file = "corrected_phenotypes/obj.phe.cor.ser2.1106.out")
load(file = "corrected_phenotypes/obj.phe.cor.ser2.2506.out")

phe.ser2.2506<- phe.ser2.2506[,-c(36:52,223:239,257:273,444:460)]

load(file = "code_growth_rates/growth_ser2.out")
growth.frame.ser2 <- growth.frame.ser2[order(as.numeric(substring(growth.frame.ser2$accession,3))),]

dira.ser2 <- cbind(phe.ser2.1106$accession,phe.ser2.2506[-1]/phe.ser2.1106[-1],growth.frame.ser2$growth)

colnames(dira.ser2)[ncol(dira.ser2)] <- "growth_width"

colnames(dira.ser2)[1] <- "accession"

save(dira.ser2, file = "Differences_all/ser2.dira.phe.out")
write.xlsx(dira.ser2,file="Differences_all/phe_ser2_dira.xlsx",rownames=T)














