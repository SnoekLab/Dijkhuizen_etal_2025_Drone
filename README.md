# Dijkhuizen_etal_2024_Drone
Scripts used in Dijkhuizen etal 2024
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
inputs: "obj_all.pl_sat_rep1_1106_rgb_dsm_msp_red_nd.out", "obj_all.pl_sat_rep2_1106_rgb_dsm_msp_red_nd.out",<br> "obj_all.pl_sat_rep1_2506_rgb_dsm_msp_red_nd.out", "obj_all.pl_sat_rep2_2506_rgb_dsm_msp_red_nd.out" <br>
Dataframes containing for every pixel the RGB values, the MSP values, the height, and what accession they belong to. <br>
Outputs: "obj_phe.sat1.1106.out", "obj_phe.sat2.1106.out", "obj_phe.sat1.2506.out", obj_phe.sat2.2506.out, dataframes containing the phenotypes of all accessions.
<code style="color:red">The script to combine both reps into one is still missing.</code>

### *cor_gwas_massive_sativa_all_snps.R*
Perform the GWAS on all sativa data.<br>
Inputs: "phe.sat.mean.1106.2506.diff.include.rat.out" dataframe containing all the phenotypes we perform GWAS on, "obj_all.ALTREF.out" SNPmap, "cov_new_snps.out" kinship matrix. <br>
Outputs: The GWAS results of every singel traits. Output as a dataframe and a simple manhattanplot.

### *manhat_manhat_sat_nolog_new_snps.R*
Reads in all individual GWAS results, and outputs all significant results in 1 frame. <br>
Inputs: "obj_all.ALTREF.out" SNPmap, All individual GWAS results. <br>
Outputs: manhat.manhat.nolog.saw.new.snps.out all frames aggregated into one large dataframe.

## Scripts to generate the figures
***Fig1.R*** &emsp; Script to generate figure 1<br>
inputs: "obj_all.pl_sat_rep1_2506_rgb_dsm_msp_red_nd.out" file containing the image data for sativa on day 93.<br>
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
***Supp_Fig8*** &emsp; Script to generate supplemental figure 8, also optionally generates the clustering on all traits (not just the mean traits like in Fig3.R). <br>
inputs: "Supplemental_data.xlsx" <br>

## Extra
### *Raw_drone_image_access.R*
Plots individual accessions. Not necessary, but useful for further investigation. <br>
Inputs: Choose one of the following depending on what day and rep you want: <br>
obj_day1.rep1_sat_rep1_1106_rgb_dsm_msp.out, obj_day2.rep1_sat_rep1_2506_rgb_dsm_msp_red_nd.out, obj_day1.rep2_sat_rep1_1106_rgb_dsm_msp.out, obj_day2.rep2_sat_rep1_2506_rgb_dsm_msp_red_nd.out
Outputs: Plot of the chosen accessions.

# Issues to resolve: <br>
Basten's Raw_drone_image_access.R script and the Extr_phe_per_plot_annotated.R script use almost exactly the same dataframes. But not exactly the same. <br>
We should fix this.
