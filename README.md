# JSTMapp (Joint spatiotemporal modelling application)
## Purpose of the application
JSTMapp was developed to ease the fitting of joint spatiotemporal models by R INLA to epidemiologists with limited programming skills. In its current status, it impliments joint spatiotemporal modelling of two diseases with no covariates other than space and time.
## How to use the application
### In R/ RStudio 
The application can be used in R by downloading the application from Github author page by executing the following command:

```install.packages("devtools")```

```devtools::install_github("alfredngwira/JSTMapp", ref="main")```

After downloading the application in R, the web-based user interface can be accessed by executing the following commands:

```library(JSTMapp)```

```run_app()```

Alternatively, the user can access the web-based user interface by executing the following command:

```shiny::runGitHub("alfredngwira/JSTMapp",subdir="JSTMapp")```  shiny

To run the application in R, the user needs to have the following R packages installed:

```shiny, ggplot2,INLA,gridExtra,RColorBrewer,Hmisc,rmapshaper,shinyjs,dplyr,spdep,raster,tmap,tidyr,gstat```

This is so since in this scenario all computations are done on the local computer.
### Online
The application can be accessed online by following the shiny link https://alfredngwira.shinyapps.io/JSTMapp. In this regard, computations are done on the shiny server computer and hence the user computer does not need to have R software. The challenges associated with the online usage is that the memory associated with the free payment plan is usually not enough for complex computations which usually result in frequent user disconnections. 

## Example data
Bovine tuberculosis in cattle and extrapulmonary tuberculosis in humans in Africa is the example data used for illustration in the  article about JSTMapp which can be downloaded from [here](https://github.com/alfredngwira/JSTMapp/blob/main/inst/JSTMapp/jointafrica.csv), while the shapefiles can be downloaded from [here](https://github.com/alfredngwira/JSTMapp/blob/main/inst/JSTMapp/Africa_Boundaries.zip).
