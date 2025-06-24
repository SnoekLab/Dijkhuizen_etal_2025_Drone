# Lac_SNPs
### *Lactuca sativa* SNP Processing

For GWAS (and eGWAS) a matrix with integers indicating allele status (REF/HET/ALT) is needed. To make these the combined VCF file needs to be processed and reformatted.

##### Split Sativa and Serriola 
The combined VCF was split over sativa and serriola, both to their own VCF. These VCFs were filtered for only bi-allelic SNPs.
(*Filter_Sativa_VCF.sh + Filter_Serriola_VCF.sh*) 

##### Split by Chromosomes (contigs)
These VCF were split by chromosomes (and MT, PT, and non-ref contigs) producing one VCF per chromosome per species.
(*Process_whole_batch_2023.py --> Split_further_2023.py*).

#### SNP matrix for eGWAS, Filter and Group
To make a snpmatrix for GWAS SNPs were filtered and grouped. 

##### Filter
First SNPs were excluded from the data when they had less than 10 of either REF or ALT or more than 10 DEL. 
Both allele status as well as read depth per SNP were kept. 
(*MAF_filtered.py, Reduce_prep_clump_2023_all.py and Reduce_prep_clump_2023_Function.py*)

##### Group/Clump
Because of linkage SNPs can be grouped, this reduces the size of the data as well as the testing burden. 
After filtering SNPs were inspected for possible groups/clumps in batches of 2000 snps. Starting at the first SNP, ech following SNP that differed from this SNP less than 5 positions was grouped. These SNPs, the SNP under inspection and the SNPs grouped to that SNP) where take out of the list. Then the new first SNP was considered, and so on, the process repeated until all SNPs were assigned to a group. Positions of each SNP within a group was recorded and can be traced. The allele distribution of the SNP with the highest MAF was take as the representative.   
(*Reduce_prep_clump_2023_all.py and Reduce_prep_clump_2023_Function.py*)

The resulting SNP-matrix contains both substitutions (REF, HET, ALT) as well as presence vs absence (PRES vs ABS).

##### Classical SNP matrix
To make a REF vs ALT “classical” SNP matrix we filtered on those SNPs that have enough observations for REF (10 or more) and ALT (10 or more) and no more than 10 ABS. These SNPs were stored in a separate SNPset, which we used for the first round of GWAS and eGWAS.  
(*GRPD_txt_to_Robj_2023.R and GRPD_REFALT_PAV_split.R*)

The "classical SNP matrix is setup like this  
|CHR |                           GRP| Count0| Count1| Count2| Count9|  POS| REF| ALT|      QUAL| LK001| LK002| LK003| LK004| LK005| LK006| LK007| LK008|
|--- | -----------------------------|-------|-------|-------|-------|-----|----|----|----------|------|------|------|------|------|------|------|------|
|  1 | 1228;1247;1288;1701;1837;1943|    184|      1|     13|      0| 1228|   C|   T|   4937.63|     0|     0|     0|     0|     0|     0|     0|     0|
|  1 |               1235;1711;2014|    184|      1|     13|      0| 1235|   C|   G|   6687.88|     0|     0|     0|     0|     0|     0|     0|     0|
|  1 |               1261;1333;1717|     37|      3|    158|      0| 1261|   T|   C| 173632.00|     2|     2|     2|     2|     0|     2|     2|     2|
|  1 |                         1506|     31|      2|    159|      6| 1506|   G|   A| 213447.00|     2|     2|     2|     2|     0|     2|     2|     2|

CHR: show which chromosome he SNP group was found on  
GRP: show the position of the SNPs part of this group  
Count0: number of REF alleles present  
Count1: number of HET alleles present  
Count2: number of ALT alleles present  
Count9: number of ABS alleles present  
POS: Position in bp on chromosome  
REF: Reference nucleotide  
ALT: Alternative nucleotide  
QUAL: Quality from the VCF file (first SNP of group)  
LK001 and further: Allele status per LK accession (0 = REF ; 1 = HET ; 2 = ALT ; 9 = ABS)  

##### SNPs with poor linkage  
Some SNPs could still be potential FALSE positive and cause unwanted behaviour in GWAS. These SNPs can cause single SNPs to be linked to variation in a phenotype with non or the neighbouring SNPs to be linked as well. To screen for this the local linkage was calculated by taking a SNP and obtaining the correlation with the 500 SNPs to both sides (so 1000 in total). Then the Q80,Q90,Q95,Q99 were obtained, descibing the local linkage. These numbers can be used to filter or screen for later.   
Script: *SNP_correl.R*

##### Filter for HET before use
