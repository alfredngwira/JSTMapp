# JSTMapp (Joint spatiotemporal modelling application)
## Purpose of the application
JSTMapp was developed to ease the fitting of joint spatiotemporal models by R INLA to epidemiologists with limited programming skills. In its current status, it impliments joint spatiotemporal modelling of two diseases with four covariates other than space and time.
## How to display the application interface
### In R/ RStudio 
The application can be used in R/RStudio by launching the application from GitHub author page by executing the following command:

```shiny::runGitHub("alfredngwira/JSTMapp",subdir="inst/JSTMapp")```

Alternatively, the user can access the JSTMapp’s interface in R/RStudio by executing the following commands:

```install.packages("devtools")```

```devtools::install_github("alfredngwira/JSTMapp", ref="main")```

```library(JSTMapp)```

```run_app()```

To run the application in R/RStudio, the user needs to have the following R packages installed:

```shiny, ggplot2,INLA,gridExtra,RColorBrewer,Hmisc,shinyjs,dplyr,spdep,raster,tmap,tidyr,gstat,leaflet,dotwhisker and sn```

This is so since in this scenario all computations are done on the local computer.
### Online
The application can be accessed online by following the shiny link https://alfredngwira.shinyapps.io/JSTMapp. In this regard, computations are done on the Shiny server computer and hence the user computer does not need to have R software. The challenges associated with the online usage is that the memory associated with the free payment plan is usually not enough for complex computations which usually result in frequent user disconnections. 

## Example data
Bovine tuberculosis in cattle and extrapulmonary tuberculosis in humans in Africa is the example data used for illustration in the  article about JSTMapp which can be downloaded from [here](https://github.com/alfredngwira/JSTMapp/blob/main/inst/JSTMapp/jointcattlehuman.csv), while the shapefiles can be downloaded from [here](https://github.com/alfredngwira/JSTMapp/blob/main/inst/JSTMapp/shapefiles.zip).

## Using the JSTMapp
To display results of "Explore" tab click on "show plots". Similarly, results of "Model estimation" tab may be shown by clicking on "Summary of results". To display results of all tabs after "Model estimation" tab, click on the tab name. Note that results of all tabs after the "Model estimation" depend on those of "Model estimation" tab, and hence, will only show if model estimation is done succesfully. 

Note that in may cases, computations take time to finish, and in some cases, atleast 3 minutes. Clicking on the output tabs before data entry may trigger errors. After data specifications, and waiting for sometime, will show the desired results. 
