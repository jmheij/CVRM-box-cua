# CVRM-box-cua
Model-based evaluation of CVRM-box intervention in Dutch primary care

# To run
1. Download all files, open the .Rproj file, and then open the app.R script.
2. Install all necessary packages, including those not on CRAN: devtools::install_github("DARTH-git/darthtools") as well as devtools::install_github("DARTH-git/dampack")
3. Click 'Run app' on the top-right of the open app.R script.

# To explore code
The app.R script loads all the necessary functions to run the model (allfuns_final.R). In the allfuns_final.R script, start with "15. Wrapper for CE model". From there, it is self-explanatory where to continue (i.e., which functions follow and how they interact with eachother).  

Useful sources for exploring the code: 

Please refer to the following paper for an introductory tutorial on Shiny-hosted economic models. The code structure for this model (and app) broadly aligns with the practices described there: https://wellcomeopenresearch.org/articles/5-69/v2. Reference: Smith R, Schneider P. Making health economic models Shiny: A tutorial. Wellcome Open Res. 2020 Jul 31;5:69. doi: 10.12688/wellcomeopenres.15807.2. PMID: 32904933; PMCID: PMC7459889.

Additionally, much of the code used to implement the core model are adaptations of the tutorial code published in: 
Alarid-Escudero F, Krijkamp EM, Enns EA, Yang A, Hunink MGM, Pechlivanoglou P, Jalal H. A Tutorial on Time-Dependent Cohort State-Transition Models in R using a Cost-Effectiveness Analysis Example. Medical Decision Making. 2023;43(1):21-41. https://doi.org/10.1177/0272989X221121747


