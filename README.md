# Dijkhuizen_etal_2025_Drone
Scripts used in Dijkhuizen etal 2025
For all scripts we give the:
Name<br>
Description<br>
Input<br>
Output<br>
<br>
Under Scripts to process the data we give the scripts to recreate our data analysis starting with the dataframe containing the original image information. <br>
Under Scripts to generate the figures we give the scripts to genereate the figures we use in the paper using the most important data summarized in Supplemental_data.xlsx. <br>

## Scripts to process the data
### *Extr_phe_per_plot_annotated.R*
Takes dataframe from previous script and extracts the phenotypes, color, color ratios and height.<br>
Inputs: "obj_drone_ima.rep1_sat_1106_rgb_dsm_msp.R4.3.2.out", "obj_drone_ima.rep2_sat_1106_rgb_dsm_msp.R4.3.2.out",<br> "obj_drone_ima.rep1_sat_2506_rgb_dsm_msp_red_nd.R4.3.2.out", "obj_drone_ima.rep2_sat_2506_rgb_dsm_msp_red_nd.R4.3.2.out" <br>
Dataframes containing for every pixel the RGB values, the MSP values, the height, and what accession they belong to. <br>
Outputs: "obj_pheno.sat_rep1.1106.R4.3.2.out", "obj_pheno.sat_rep2.1106.R4.3.2.out", "obj_pheno.sat_rep1.2506.R4.3.2.out", "obj_pheno.sat_rep2.2506.R4.3.2.out", dataframes containing the phenotypes of all accessions.

### *Extr_soil_pixels.R*
Outputs the soil pixels seperated from the plant pixels. This is necessary for the height correction in next step. <br>
Inputs: "obj_drone_ima.rep1_sat_1106_rgb_dsm_msp.R4.3.2.out ", "obj_drone_ima.rep2_sat_1106_rgb_dsm_msp.R4.3.2.out ",<br> "obj_drone_ima.rep1_sat_2506_rgb_dsm_msp_red_nd.R4.3.2.out ", "obj_drone_ima.rep2_sat_2506_rgb_dsm_msp_red_nd.R4.3.2.out" <br>
Outputs: "obj_pheno.sat_rep1.1106.soil.R4.3.2.out", "obj_pheno.sat_rep2.1106.soil.R4.3.2.out", "obj_pheno.sat_rep1.2506.soil.R4.3.2.out", "obj_pheno.sat_rep2.2506.soil.R4.3.2.out", dataframes containing the soil pixel. <br>

### *Correct_heights.R*
The different plots have slightly different elevation. This script corrects for that. <br>
Inputs: "obj_pheno.sat_rep1.1106.R4.3.2.out", "obj_pheno.sat_rep2.1106.R4.3.2.out", "obj_pheno.sat_rep1.2506.R4.3.2.out", "obj_pheno.sat_rep2.2506.R4.3.2.out" <br>
"obj_pheno.sat_rep1.1106.soil.R4.3.2.out", "obj_pheno.sat_rep2.1106.soil.R4.3.2.out", "obj_pheno.sat_rep1.2506.soil.R4.3.2.out", "obj_pheno.sat_rep2.2506.soil.R4.3.2.out" <br>
Outputs: "obj.phe.cor.sat_rep1.1106.R4.3.2.out", "obj.phe.cor.sat_rep2.1106.R4.3.2.out", "obj.phe.cor.sat_rep1.2506.R4.3.2", "obj.phe.cor.sat_rep2.2506.R4.3.2.out"

### *Difference_calculation.R*
Calculates the absolute differences, the log2 fold change differences and merges both reps into one mean result for the GWAS step. <br>
Inputs: "obj.phe.cor.sat_rep1.1106.R4.3.2.out", "obj.phe.cor.sat_rep2.1106.R4.3.2.out", "obj.phe.cor.sat_rep1.2506.R4.3.2", "obj.phe.cor.sat_rep2.2506.R4.3.2.out" <br>
all phenotypes, with correction for soil height. <br>
Outputs: "obj_sat_rep1.diff.phe.R4.3.2.out", "obj_sat_rep2.diff.phe.R4.3.2.out", "obj_sat_rep1.dira.phe.R4.3.2.out", "obj_sat_rep2.dira.phe.R4.3.2.out" <br>
Absolute and log2 fold change differences for both reps.

### *Make_gwas_matrix.R*
Merges all dataframes from previous steps into one big dataframe we use as input for the GWAS. <br>
Inputs: "obj.phe.cor.sat_rep1.1106.R4.3.2.out", "obj.phe.cor.sat_rep2.1106.R4.3.2.out", "obj.phe.cor.sat_rep1.2506.R4.3.2", "obj.phe.cor.sat_rep2.2506.R4.3.2.out",<br>
"obj_sat_rep1.diff.phe.R4.3.2.out", "obj_sat_rep2.diff.phe.R4.3.2.out", "obj_sat_rep1.dira.phe.R4.3.2.out", "obj_sat_rep2.dira.phe.R4.3.2.out" <br>
Outputs: "obj_phe.sat.mean.1106.2506.diff.include.rat.R4.3.2.out" The dataframe with all phenotypes which will be used as input for the GWAS <br>

### *cor_gwas_massive_sativa_all_snps.R*
Perform the GWAS on all sativa data.<br>
Inputs: "phe.sat.mean.1106.2506.diff.include.rat.out" dataframe containing all the phenotypes we perform GWAS on, "obj_all.ALTREF_SNP_matrix_sat_2024_R4.3.2.out" SNPmap. <br>
Outputs: The GWAS results of every single traits. Output as a dataframe and a simple manhattanplot.

### *manhat_manhat_sat_nolog_new_snps.R* 
Reads in all individual GWAS results, and outputs all significant results in 1 frame. <br>
Inputs: "obj_all.ALTREF_SNP_matrix_sat_2024_R4.3.2.out" SNPmap, All individual GWAS results. <br>
Outputs: "manhat.manhat.nolog.saw.new.snps.out" all frames aggregated into one large dataframe.

## Scripts to generate the figures
***Fig1.R*** &emsp; Script to generate figure 1<br>
inputs: "obj_drone_ima.rep1_sat_2506_rgb_dsm_msp_red_nd.R4.3.2.out" file containing the image data for sativa on day 93.<br>
***Fig2.R*** &emsp; Script to generate figure 2<br>
inputs: "Supplemental_data.xlsx" <br>
***Fig3.R*** &emsp; Script to generate figure 3, also generates the clustering on mean traits<br>
inputs: "Supplemental_data.xlsx" <br>
***Fig4.R*** &emsp; Script to generate figure 4<br>
inputs: "Supplemental_data.xlsx" <br>
***Fig5.R*** &emsp; Script to generate figure 5<br>
inputs: "Supplemental_data.xlsx" <br>
***Fig6.R*** &emsp; Script to generate figure 6<br>
inputs: "Supplemental_data.xlsx", the two example images of the bolting lettuce are not included. <br>
<br>
***Supp_Fig6*** &emsp; Script to generate supplemental figure comparing height traits<br>
inputs: "Supplemental_data.xlsx" <br>
***Supp_Fig8*** &emsp; Script to generate supplemental figure 9, also optionally generates the clustering on all traits (not just the mean traits like in Fig3.R). <br>
inputs: "Supplemental_data.xlsx" <br>

## Extra
### *Raw_drone_image_access.R*
Plots individual accessions. Not necessary, but useful for further investigation. <br>
Inputs: Choose one of the following depending on what day and rep you want: <br>
"obj_drone_ima.rep1_sat_1106_rgb_dsm_msp.R4.3.2.out", "obj_drone_ima.rep1_sat_2506_rgb_dsm_msp_red_nd.R4.3.2.out", "obj_drone_ima.rep2_sat_1106_rgb_dsm_msp.R4.3.2.out ", "obj_drone_ima.rep2_sat_2506_rgb_dsm_msp_red_nd.R4.3.2.out"
Outputs: Plot of the chosen accessions.

### *H2_2023.R*
Calculated the heritability of all traits. <br>
Inputs: "obj.phe.cor.sat_rep1.1106.R4.3.2.out", "obj.phe.cor.sat_rep2.1106.R4.3.2.out", "obj.phe.cor.sat_rep1.2506.R4.3.2", "obj.phe.cor.sat_rep2.2506.R4.3.2.out"<br>
Outputs: "h2.collect.v4.xlsx" Excel file containing the heritabilities for all traits.

### *select_mean_peaks.R*
Script to extract the locations of the peaks using only the mean traits.<br>
Inputs: "Supplemental_data.xlsx" <br>
Outputs: "QTLsfig5.csv" file containing all peaks using mean traits. This was used for the sheet QTLs.fig5 and for table 3

### *select_peaks.R*
Script to extract the locations of the peaks using all extended descriptives. <br>
Inputs: "Supplemental_data.xlsx" <br>
Outputs: "QTLsall.csv" file containg all peaks using all extended descriptives. Used in the sheet QTLs.all and Supplemental figure 8.



