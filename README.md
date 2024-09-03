# Dijkhuizen_etal_2024_Drone
Scripts used in Dijkhuizen etal 2024

Index<br>
Name                                  Description<br>
Extract_and_combine_drone_data_annotated.R        Extract the different plots from total image and output height, color and coordinates in a dataframe. Outputs in 4 seperate dataframes for Sativa and Serriola day 1 and 2.<br>
Extr_phe_per_plot_annotated.R                     Takes dataframe from previous script and extracts the phenotypes, color, color ratios and height.<br>
cor_gwas_massive_clus_sativa_all_snps.R                       Perform the GWAS on all sativa data.<br>
clus_sat_full_manhat_manhat.R                     Generate the clustering on all traits<br>

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
Supplemental_Figure6                             Script to generate figure <br>
Supplemental_figure7                             Script to generate figure

# Issues to resolve: <br>
Not all scripts load in the data the exact same way. Some use Basten's file path some use Rens'. <br>
Fig1 and 6 require the actual image data, which we don't include because it is enermous <br>
Proposed solution: Don't include Fig1.R because it is just 6 fotos and for Fig 6 only include the graph. <br>
Fig3 contains multiple versions for fig3.
