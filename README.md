# Dijkhuizen_etal_2024_Drone
Scripts used in Dijkhuizen etal 2024

Index<br>
Name &<br>
Description <br>
*Extract_and_combine_drone_data_annotated.R*
Extract the different plots from total image and output height, color and coordinates in a dataframe. Outputs in 4 seperate dataframes for Sativa and Serriola day 1 and 2.<br>
Inputs: "obj_metawco.out" a metadata file containing the location data of all plots, "246f746c-f8e2-4bcf-beb9-6051bc535812_RGB_modified_ref_points.tif" file containing the rgb values, <br>
"246f746c-f8e2-4bcf-beb9-6051bc535812_DSM_modified.tif" file containing the plant heights, "cebe7014-1426-4a5a-a78f-14d4559875c4_MSP_modified.tif" file containing the multispectral values, <br>
"Tiff_files_11/11_GCP.shp" points for georeferencing. <br>
Outputs: "obj_all.pl_sat_rep1_1106_rgb_dsm_msp_red_nd.out", "obj_all.pl_sat_rep2_1106_rgb_dsm_msp_red_nd.out", "obj_all.pl_sat_rep1_2506_rgb_dsm_msp_red_nd.out", "obj_all.pl_sat_rep2_2506_rgb_dsm_msp_red_nd.out" <br>
Dataframes containing for every pixel the RGB values, the MSP values, the height, and what accession they belong to. <br>
<code style="color:red">@Basten, we don't actually deliver any of the tif files. Also the script actually only ouputs day 2506. To switch to 1106 we need to manually change some things</code>

*Extr_phe_per_plot_annotated.R*
Takes dataframe from previous script and extracts the phenotypes, color, color ratios and height.<br>
inputs: "obj_all.pl_sat_rep2_1106_rgb_dsm_msp_red_nd.out", "obj_all.pl_sat_rep1_2506_rgb_dsm_msp_red_nd.out", "obj_all.pl_sat_rep2_2506_rgb_dsm_msp_red_nd.out" <br>
Dataframes containing for every pixel the RGB values, the MSP values, the height, and what accession they belong to. <br>
Outputs: "obj_phe.sat1.1106.out", "obj_phe.sat2.1106.out", "obj_phe.sat1.2506.out", obj_phe.sat2.2506.out, dataframes containing the phenotypes of all accessions.

<code style="color:red">The script to combine both reps into one is still missing.</code>

*cor_gwas_massive_clus_sativa_all_snps.R*
Perform the GWAS on all sativa data.<br>
Inputs: "phe.sat.mean.1106.2506.diff.include.rat.out" dataframe containing all the phenotypes we perform GWAS on, "sat.accessions.out" list of all the accesions that we use, <br> "obj_all.ALTREF.out" SNPmap, "cov_new_snps.out" kinship matrix.
Outputs: The GWAS results of every singel traits. Output as a dataframe and a simple manhattanplot.

*clus_sat_full_manhat_manhat.R*
Generate the clustering on all traits<br>
Inputs: "manhat.manhat.nolog.sat.out", "phe.sat.mean.1106.2506.diff.out", 
Outputs: "km.sat.full.12.sqrd.cor.out" clustering in 12 clusters.
<code style="color:red">@Bram&Basten. Some cleanup is still required here. We create a clustering on mean traits and on all traits. And this script has a lot of figures we don't actually use.</code>


-<br>
-<br>
-<br>
-<br>
Fig1.R                                           Script to generate figure 1<br>
Fig2.R                                           Script to generate figure 2<br>
Fig3.R                                           Script to generate figure 3, also generates the clustering on mean traits<br>
Fig4.R                                           Script to generate figure 4<br>
Fig5.R                                           Script to generate figure 5<br>
Fig6.R                                           Script to generate figure 6<br>
<br>
Supp_fig6                                        Script to generate supplemental figure comparing height traits<br>
Supp_fig8                                        Script to generate supplemental figure 8. <br>

# Issues to resolve: <br>
Not all scripts load in the data the exact same way. Some use Basten's file path some use Rens'. <br>
Fig1 and 6 require the actual image data, which we don't include because it is enermous <br>
Proposed solution: Don't include Fig1.R because it is just 6 fotos and for Fig 6 only include the graph. <br>
Fig3 contains multiple versions for fig3.
