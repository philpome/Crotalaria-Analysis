# Crotalaria-Analysis
Data analyses for Crotalaria avonensis

## survival-analysis-rcode.R
Code for the analysis of the survival of the endangered plant species Crotalaria avonensis after cryostorage. Using a modified survival analysis, this model predicts survival after cryostorage based on the time banked in LN, the protocol used for banking, and the operator who performed the original banking. 

## crotalaria-analysis-2018.Rmd
RMarkdown file that clarifies and interprets the survival-analysis-rcode for co-authors to review. 

## crotalaria-byvial.Rmd
RMarkdown walkthrough for co-authors. Analysis of shoot tips alive at four weeks post-cryostorage with the following co-variates: years banked, banking protocol, recovery medium, initial survival percentage, contamination at two weeks, age at banking pre-culture, PVS2 control survival percentage, pre-culture medium, and the operator who performed the original banking. Genotype is included in models as a random effect. Predictive models created using mixed effects models and resampling approaches. 

## crotalaria-dv-ev-comparison.R
Code for the comparison of survival after cryostorage based on cryopreservation protocol. Includes visualizations created using ggplot2.

## graphs-for-2019-paper.R
Clean code for graphs generated in ggplot2 for the publication of Crotalaria cryopreservation results. 
