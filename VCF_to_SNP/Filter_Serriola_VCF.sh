#!/bin/bash

##Filter whole VCF to produce Serriola only VCF
bcftools view -S ./sample_list_Serriola.txt --threads 56 -Oz -o ./LK_Serriola200.vcf.gz ./LK001_399_variantcalling_merged.vcf.gz

##Filter Serriola VCF for only biallelic SNPs (M2)
bcftools view -M2 --threads 56 -Oz -o ./LK_Serriola200.biallelic.vcf.gz ./LK_Serriola200.vcf.gz
