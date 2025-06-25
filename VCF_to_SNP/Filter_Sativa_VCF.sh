#!/bin/bash

##Filter whole VCF to produce Sativa only VCF
bcftools view -S ./sample_list_Sativa.txt --threads 56 -Oz -o ./LK_Sativa200.vcf.gz ./LK001_399_variantcalling_merged.vcf.gz

##Filter Sativa VCF for only biallelic SNPs (M2)
bcftools view -M2 --threads 56 -Oz -o ./LK_Sativa200.biallelic.vcf.gz ./LK_Sativa200.vcf.gz
