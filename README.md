# MILC-app
Shiny app for the MILC package

This application is based on the R "MILC" package by Stavroula Chrysanthopoulou and is meant to vizualize the natural history of lung cancer in a person with given characteristics without treatment. The plot reflects the probability of not being diagnosed with lung cancer through the lifetime.

1) "app" contains a working app shell (working tabs, input, css)
2) "Data simulation and KM plot" - R file with MILC::nat_hist simulation and a Kaplan-Meier plot. Parallel processing is applied to enhance functionality. 
10000 natural history simulations on a 4-core computer take around 4 minutes. 
3) "www" folder contains images and css style essentials. 

NB: To make the app work properly, "app" and "www" have to be downloaded to one folder.