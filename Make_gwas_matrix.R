# Create output folder if it doesn't exist already
if (!dir.exists("GWAS_objects")){
  dir.create("GWAS_objects")
}

load(file = "corrected_phenotypes/obj.phe.cor.sat1.1106.out")
load(file = "corrected_phenotypes/obj.phe.cor.sat2.1106.out")

load(file = "corrected_phenotypes/obj.phe.cor.sat1.2506.out")
load(file = "corrected_phenotypes/obj.phe.cor.sat2.2506.out")

load(file = "Differences_all/sat1.diff.phe.out")
load(file = "Differences_all/sat2.diff.phe.out")

load(file = "Differences_all/sat1.dira.phe.out")
load(file = "Differences_all/sat2.dira.phe.out")

#### Make Sativa matrix
mean.sat.1106 <- matrix(nrow = nrow(phe.sat1.1106),ncol = ncol(phe.sat1.1106)-1)

for(i in 1:nrow(phe.sat1.1106)){
  for(j in 1:(ncol(phe.sat1.1106)-1)){
    mean.sat.1106[i,j] <- mean(phe.sat1.1106[i,j+1],phe.sat2.1106[i,j+1])
  }
}
mean.sat.1106 <- data.frame(mean.sat.1106)

colnames(mean.sat.1106)<- paste0(colnames(phe.sat1.1106)[2:ncol(phe.sat1.1106)],".1106")

mean.sat.2506 <- matrix(nrow = nrow(phe.sat1.2506),ncol = ncol(phe.sat1.2506)-1)

for(i in 1:nrow(phe.sat1.2506)){
  for(j in 1:(ncol(phe.sat1.2506)-1)){
    mean.sat.2506[i,j] <- mean(phe.sat1.2506[i,j+1],phe.sat2.2506[i,j+1])
  }
}
mean.sat.2506 <- data.frame(mean.sat.2506)

colnames(mean.sat.2506)<- paste0(colnames(phe.sat1.2506)[2:ncol(phe.sat1.2506)],".2506")

diff.sat <- matrix(nrow = nrow(diff.sat1),ncol = ncol(diff.sat1)-1)

for(i in 1:nrow(diff.sat1)){
  for(j in 1:(ncol(diff.sat1)-1)){
    diff.sat[i,j] <- mean(as.numeric(diff.sat1[i,j+1]),as.numeric(diff.sat2[i,j+1]))
  }
}
diff.sat <- data.frame(diff.sat)

colnames(diff.sat)<- paste0(colnames(diff.sat1)[2:ncol(diff.sat1)],".diff")

dira.sat <- matrix(nrow = nrow(dira.sat1),ncol = ncol(dira.sat1)-1)

for(i in 1:nrow(dira.sat1)){
  for(j in 1:(ncol(dira.sat1)-1)){
    if(!is.na(dira.sat1[i,j+1])&!is.na(dira.sat2[i,j+1])){
      dira.sat[i,j] <- mean(as.numeric(dira.sat1[i,j+1]),as.numeric(dira.sat2[i,j+1]),na.rm = T)
    }
    else{
      dira.sat[i,j] <- NA
    }
  }
}

dira.sat <- data.frame(dira.sat)
rem.sat <- which(apply(dira.sat,2,function(x)sum(is.na(x)))>0)
dira.sat <- dira.sat[,-rem.sat]

dira.sat[dira.sat==0]<- dira.sat[dira.sat==0]+0.0001
dira.sat <- log2((abs(dira.sat))^(sign(dira.sat)))

colnames(dira.sat)<- paste0(colnames(dira.sat1)[2:ncol(dira.sat1)][-rem.sat],".dira")

sat.full <- cbind(mean.sat.1106,mean.sat.2506,diff.sat,dira.sat)

save(sat.full,file = "GWAS_objects/phe.sat.mean.1106.2506.diff.include.rat.out")
