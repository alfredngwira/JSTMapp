# JSTMapp (Joint spatio-temporal modelling application)
## Purpose of the software
JSTMapp was developed to ease the fitting of joint spatio-temporal models by R INLA to epidemiologists with limited programming skills. In its current status, it impliments joint spatio-temporal modelling of two diseases with no covariates other than space and time.
## How to use the software
### Online
The application can be used online by following the shiny link https://alfredngwira.shinyapps.io/JSTMapp. In this regard, computations are done on the shiny server computer and hence the user computer does not need to have R software. The challenges associated with the online usage is that the memory associated with the free plan is usually not enough for some heavy computations which results in frequent user disconnections. 
### In R/ RStudio
We therefore recommnend to use it in R by downloading the app from Github author page by executing the command: 

```devtools::install_github("alfredngwira/JSTMapp", ref="main")```

After downloading the app in R, the web-based user interface can be accessed by executing the following commands:

```library(JSTMapp)```

```run_app()```

Alternatively, the user can access the web-based user interface by executing the following command:

```shiny::runGitHub("alfredngwira/JSTMapp",subdir="JSTMapp")```

To run the app in R, the user needs to have the following R dependencies installed:

```shiny, INLA, ggplot2, gridExtra, RColorBrewer, cleangeo, Hmisc, rmapshaper, shinyjs, dplyr, spdep, raster, and tmap```

This is so since all computations are done on the local computer thereby avoiding user disconnections due to memory problems. 

## Example data
Bovine tuberculosis in cattle and extrapulmunary tuberculosis in humans in Africa is the example data for illustration used in the full article about JSTMapp which can be downloaded from "here".
