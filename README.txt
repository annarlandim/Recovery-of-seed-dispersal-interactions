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

## Produced from the script:

8) jagsModel.txt - File in .txt-format containing the JAGS model code (produced by the script).


############
SESSION INFO
############

R version 4.3.1 (2023-06-16 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19044)

Matrix products: default


locale:
[1] LC_COLLATE=English_Europe.utf8  LC_CTYPE=English_Europe.utf8    LC_MONETARY=English_Europe.utf8 LC_NUMERIC=C                   
[5] LC_TIME=English_Europe.utf8    

time zone: Europe/Berlin
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] lattice_0.21-8 rjags_4-15     coda_0.19-4.1  ggplot2_3.4.4  tidyr_1.3.1    dplyr_1.1.4    psych_2.4.1    here_1.0.1    

loaded via a namespace (and not attached):
[1] miniUI_0.1.1.1    dplyr_1.1.4       compiler_4.3.1    promises_1.2.1   
 [5] tidyselect_1.2.0  Rcpp_1.0.11       stringr_1.5.1     later_1.3.2      
 [9] fastmap_1.1.1     mime_0.12         R6_2.5.1          generics_0.1.3   
[13] htmlwidgets_1.6.4 tibble_3.2.1      profvis_0.3.8     shiny_1.8.0      
[17] pillar_1.9.0      rlang_1.1.1       utf8_1.2.4        cachem_1.0.8     
[21] stringi_1.7.12    httpuv_1.6.13     fs_1.6.3          pkgload_1.3.4    
[25] memoise_2.0.1     cli_3.6.1         magrittr_2.0.3    digest_0.6.33    
[29] rstudioapi_0.15.0 xtable_1.8-4      remotes_2.4.2.1   devtools_2.4.5   
[33] lifecycle_1.0.4   vctrs_0.6.5       glue_1.6.2        urlchecker_1.0.1 
[37] sessioninfo_1.2.2 pkgbuild_1.4.3    fansi_1.0.6       purrr_1.0.2      
[41] usethis_3.1.0     tools_4.3.1       pkgconfig_2.0.3   ellipsis_0.3.2   
[45] htmltools_0.5.7     