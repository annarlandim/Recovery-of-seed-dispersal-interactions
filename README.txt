#################
DATA AND CODE FROM
#################

"Delayed recovery of seed-dispersal interactions after deforestation"

by Landim et al. (2024)

###########
DESCRIPTION
###########

This repository contains all data files and computer code (in R language) that is needed to reproduce the analyses presented in the paper.

#######
CONTENT
#######

## Rscript
1) SD_recovery.analysis.R - Basic R script for running the analysis.

## CSV files
2) plants.csv - plants traits and plots where species were observed.
3) animals.csv - animals traits, plots where species were observed and observation method.
4) interactions.csv - fleshy-fruited plants and frugivorous animals observed interacting, plot and observation method.
5) interactions_index_fig2.xlsx - table S2 in the Supplementary Material.
6) animal_traits.xlsx - table S3 in the Supplementary Material.
7) plant_traits.xlsx - table S4 in the Supplementary Material.


############
SESSION INFO
############

R version 4.3.1 (2023-06-16 ucrt)Platform: x86_64-w64-mingw32 (64-bit)Running under: Windows 10 x64 (build 19044)Matrix products: default

locale:
[1] LC_COLLATE=English_Europe.utf8  LC_CTYPE=English_Europe.utf8    LC_MONETARY=English_Europe.utf8 LC_NUMERIC=C                   
[5] LC_TIME=English_Europe.utf8    

time zone: Europe/Berlin
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] lattice_0.21-8 rjags_4-15     coda_0.19-4.1  ggplot2_3.4.4  tidyr_1.3.1    dplyr_1.1.4    psych_2.4.1   

loaded via a namespace (and not attached):
 [1] vctrs_0.6.5       nlme_3.1-162      cli_3.6.1         rlang_1.1.1       purrr_1.0.2       generics_0.1.3    glue_1.6.2       
 [8] colorspace_2.1-0  scales_1.3.0      fansi_1.0.6       grid_4.3.1        munsell_0.5.0     tibble_3.2.1      lifecycle_1.0.4  
[15] compiler_4.3.1    pkgconfig_2.0.3   rstudioapi_0.15.0 R6_2.5.1          tidyselect_1.2.0  utf8_1.2.4        pillar_1.9.0     
[22] mnormt_2.1.1      parallel_4.3.1    magrittr_2.0.3    withr_3.0.0       tools_4.3.1       gtable_0.3.4     