# MILC-app
Shiny app for the MILC package

On March, 13, 2019:
1) "app" contains a working app shell (working tabs, input, css)
2) "natural history simulation" - R file with MILC::nat_hist ran multiple times to simulate natural history of disease in patients with given parameters
    there is also a Kaplan-Meier estimator and a survival plot (see attached plot "SurvPlot.png")
3) "numeric predictors" - R file with a function returning some results of natural history simulation 
    (estimated time of death from lung cancer, estimated time of diagnosis, 
    estimated hazard of the onset of the first malignant cell by certain age) 
    
 (2) and (3) are supposed to be the outputs in the shiny app
