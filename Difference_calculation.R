library(openxlsx)

# Create output folder if it doesn't exist already
if (!dir.exists("Differences_all")){
  dir.create("Differences_all")
}

########### First substracting

### Sativa rep 1 differences
load(file = "corrected_phenotypes/obj.phe.cor.sat_rep1.1106.R4.3.2.out")
load(file = "corrected_phenotypes/obj.phe.cor.sat_rep1.2506.R4.3.2.out")

colnames(phe.sat1.2506)[444:460]
colnames(phe.sat1.2506)[257:273]
colnames(phe.sat1.2506)[223:239]
colnames(phe.sat1.2506)[36:52]

phe.sat1.2506<- phe.sat1.2506[,-c(36:52,223:239,257:273,444:460)]
diff.sat1 <- cbind(phe.sat1.1106$accession,phe.sat1.2506[-1]-phe.sat1.1106[-1])
colnames(diff.sat1)[1] <- "accession"

save(diff.sat1, file = "Differences_all/obj_sat_rep1.diff.phe.R4.3.2.out")
#write.xlsx(diff.sat1,file="Differences_all/phe_sat1_diff.xlsx",rownames=T)

#### Sativa rep 2

load(file = "corrected_phenotypes/obj.phe.cor.sat_rep2.1106.R4.3.2.out")
load(file = "corrected_phenotypes/obj.phe.cor.sat_rep2.2506.R4.3.2.out")

phe.sat2.2506<- phe.sat2.2506[,-c(36:52,223:239,257:273,444:460)]
diff.sat2 <- cbind(phe.sat2.1106$accession,phe.sat2.2506[-1]-phe.sat2.1106[-1])
colnames(diff.sat2)[1] <- "accession"

save(diff.sat2, file = "Differences_all/obj_sat_rep2.diff.phe.R4.3.2.out")
#write.xlsx(diff.sat2,file="Differences_all/phe_sat2_diff.xlsx",rownames=T)

########### Now ratios

### Sativa rep 1 differences

load(file = "corrected_phenotypes/obj.phe.cor.sat_rep1.1106.R4.3.2.out")
load(file = "corrected_phenotypes/obj.phe.cor.sat_rep1.2506.R4.3.2.out")

colnames(phe.sat1.2506)[444:460]
colnames(phe.sat1.2506)[257:273]
colnames(phe.sat1.2506)[223:239]
colnames(phe.sat1.2506)[36:52]

phe.sat1.2506<- phe.sat1.2506[,-c(36:52,223:239,257:273,444:460)]
dira.sat1 <- cbind(phe.sat1.1106$accession,phe.sat1.2506[-1]/phe.sat1.1106[-1])
colnames(dira.sat1)[1] <- "accession"

save(dira.sat1, file = "Differences_all/obj_sat_rep1.dira.phe.R4.3.2.out")
#write.xlsx(dira.sat1,file="Differences_all/phe_sat1_dira.xlsx",rownames=T)

#### Sativa rep 2

load(file = "corrected_phenotypes/obj.phe.cor.sat_rep2.1106.R4.3.2.out")
load(file = "corrected_phenotypes/obj.phe.cor.sat_rep2.2506.R4.3.2.out")

phe.sat2.2506<- phe.sat2.2506[,-c(36:52,223:239,257:273,444:460)]
dira.sat2 <- cbind(phe.sat2.1106$accession,phe.sat2.2506[-1]/phe.sat2.1106[-1])
colnames(dira.sat2)[1] <- "accession"

save(dira.sat2, file = "Differences_all/obj_sat_rep2.dira.phe.R4.3.2.out")
#write.xlsx(dira.sat2,file="Differences_all/phe_sat2_dira.xlsx",rownames=T)