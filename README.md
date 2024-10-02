# Dijkhuizen_etal_2024_Drone
Scripts used in Dijkhuizen etal 2024

Name<br>
Description<br>
Input<br>
Output<br>

## Scripts to process the data
### *Extract_and_combine_drone_data_annotated.R*
Extract the different plots from total image and output height, color and coordinates in a dataframe.<br>
Inputs: "obj_metawco.out" a metadata file containing the location data of all plots, "246f746c-f8e2-4bcf-beb9-6051bc535812_RGB_modified_ref_points.tif" file containing the rgb values, <br>
"246f746c-f8e2-4bcf-beb9-6051bc535812_DSM_modified.tif" file containing the plant heights, "cebe7014-1426-4a5a-a78f-14d4559875c4_MSP_modified.tif" file containing the multispectral values, <br>
"Tiff_files_11/11_GCP.shp" points for georeferencing. <br>
Outputs: "obj_all.pl_sat_rep1_1106_rgb_dsm_msp_red_nd.out", "obj_all.pl_sat_rep2_1106_rgb_dsm_msp_red_nd.out", "obj_all.pl_sat_rep1_2506_rgb_dsm_msp_red_nd.out", "obj_all.pl_sat_rep2_2506_rgb_dsm_msp_red_nd.out" <br>
Dataframes containing for every pixel the RGB values, the MSP values, the height, and what accession they belong to. <br>
<code style="color:red">@Basten, we don't actually deliver any of the tif files. Also the script actually only ouputs day 2506. To switch to 1106 we need to manually change some things</code>

### *Extr_phe_per_plot_annotated.R*
Takes dataframe from previous script and extracts the phenotypes, color, color ratios and height.<br>
inputs: "obj_all.pl_sat_rep1_1106_rgb_dsm_msp_red_nd.out", "obj_all.pl_sat_rep2_1106_rgb_dsm_msp_red_nd.out",<br> "obj_all.pl_sat_rep1_2506_rgb_dsm_msp_red_nd.out", "obj_all.pl_sat_rep2_2506_rgb_dsm_msp_red_nd.out" <br>
Dataframes containing for every pixel the RGB values, the MSP values, the height, and what accession they belong to. <br>
Outputs: "obj_phe.sat1.1106.out", "obj_phe.sat2.1106.out", "obj_phe.sat1.2506.out", obj_phe.sat2.2506.out, dataframes containing the phenotypes of all accessions.
<code style="color:red">The script to combine both reps into one is still missing.</code>

### *cor_gwas_massive_clus_sativa_all_snps.R*
Perform the GWAS on all sativa data.<br>
Inputs: "phe.sat.mean.1106.2506.diff.include.rat.out" dataframe containing all the phenotypes we perform GWAS on, "sat.accessions.out" list of all the accesions that we use, <br> "obj_all.ALTREF.out" SNPmap, "cov_new_snps.out" kinship matrix.
Outputs: The GWAS results of every singel traits. Output as a dataframe and a simple manhattanplot.

### *manhat_manhat.R*
Reads in all individual GWAS results, and outputs all significant results in 1 frame. <br>
Inputs: All individual GWAS results. <br>
Outputs: manhat.manhat.nolog.sat.out 

### *clus_sat_full_manhat_manhat.R*
Generate the clustering on all traits<br>
Inputs: "manhat.manhat.nolog.sat.out", "phe.sat.mean.1106.2506.diff.out" <br>
Outputs: "km.sat.full.12.sqrd.cor.out" clustering of all traits in 12 clusters.<br>
<code style="color:red">@Bram&Basten. Some cleanup is still required here. This script has a lot of figures we don't actually use.</code>

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
***Supp_Fig8*** &emsp; Script to generate supplemental figure 8. <br>
inputs: "Supplemental_data.xlsx" <br>

# Issues to resolve: <br>
Not all scripts load in the data the exact same way. Some use Basten's file path some use Rens'. <br>
Fig1 and 6 require the actual image data, which we don't include because it is enermous <br>
Proposed solution: Don't include Fig1.R because it is just 6 fotos and for Fig 6 only include the graph. <br>
Fig3 contains multiple versions for fig3.
