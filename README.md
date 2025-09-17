The folder contains: 

1) The data for analyses done in this project: "Data_03_08_24.csv"
It's just a csv file, fine tuned for Czech language environment. Therefore, if openning the file outside R, you may experience problems with "," vs. "." dividing columns. Opening in R should always work.
The file contains the following columns:
      "Culture_Year" - this is to identify which sample it is. We have nine samples from seven countries.
      "Sex" - during the data collection, participants (those in the photos) were asked on their "sex", the variable should most closely resemble "sex assigned at birth"
      "L_skin" - CIELab L*, skin lightness.
      "L_iris" - CIELab L*, iris lightness.
      "a_iris" - CIELab a*, iris "redness".
      "b_iris" - CIELab b*, iris "yellowness".
      "L_sclera" - CIELab L*, scleral lightness.
      "a_sclera" - CIELab a*, scleral "redness".
      "b_sclera" - CIELab b*, scleral "yellowness".
      "Age" - age...
      "Attr" - perceived attractiveness, rated by local raters.
      "SexT" - perceived sextypicality, rated by local raters as "femininity" in women and "masculinity" in men.
      "Nat" - Nationality, corresponds to a country in which the data was collected.
      "Group" - Sample and Sex combined. 
      "LOCAL_DIST" - Facial shape distinctiveness, a variable obtained using landmark-based geometric morphometrics.
      "SexTypMeas" - Facial shape sextypicality, a variable obtained using landmark-based geometric morphometrics.
      "Asym" - Facial shape asymmetry, a variable obtained using landmark-based geometric morphometrics.

2) "Eye Colour Project 1.Rproj" - R project, please download this file and run every script from this file, otherwise, we cannot ensure optimal display of the scripts below.
3)  "1_Calculating_Morphometrics.R" - Records on how the morphometric variables were computed. As we are not allowed to share the facial configurations, this script is for illustrative purposes only.
4)  "2_Drawing_distribution_plots_and_deriving_tables.R" a script (active with the file "Data_03_08_24.csv"), basic descriptive statistics.
5)  "3_Modeling_Decentred_MF_Together_Standardised_Together_aka_Main_Analysis_GEOADDED.R" main analyses' record. Works well with the data. Please note that the exact results may differ slightly as the MCMC sampling is a pseudo-stochastics process.
       Note: this file includes path analyses reported in the article, too.
       After revision, it also includes analyses done on samples pooled with regard to geographic location (therefore "GEOADDED").
7)  "4_Visualisation_Main_Analyses.R" - Script to render Figure 2 and Figure 3 of the main article (returns figures prior to a small polishig procedure conducted in InkScape).
8)  "5_Modeling_decentred_Alternatives_Standardised_together.R" first script containing alternative analyses. This file shows the layout of the models that (a) excluded Iranian sample, (b) excluded skin L* from the analysis.
9)  "6_Visualisation__Alternatives_Standardised together.R" script to draw figures based on the models from the previous script.
10)  "7_Modeling_decentred_Alternatives_Standardised_separately.R" second script to contain alternative analyses. This time, the data were standardised (transfered to variables with zero mean and SD = 1) separately.
11) "8_Visualisation__Alternatives_Standardised_separately.R" script to draw figures based on the models from the previous script.
12) "10_visualisation_pooled_samples.R" script to draw figures based on the models with geographically pooled samples.
