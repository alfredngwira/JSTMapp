# JSTMapp (Joint spatio-temporal modelling application)
## Purpose of the application
JSTMapp was developed to ease the fitting of joint spatio-temporal models by R INLA to epidemiologists with limited programming skills. In its current status, it impliments joint spatio-temporal modelling of two diseases with no covariates other than space and time.
## How to use the application
### Online
The application can be used online by following the shiny link https://alfredngwira.shinyapps.io/JSTMapp. In this regard, computations are done on the shiny server computer and hence the user computer does not need to have R software. The challenges associated with the online usage is that the memory associated with the free plan is usually not enough for computations which results in frequent user disconnections. 

### In R/ RStudio 
We recommnend to use it in R by downloading the app from Github author page by executing the command:

```install.packages("devtools")```

```devtools::install_github("alfredngwira/JSTMapp", ref="main")```

After downloading the app in R, the web-based user interface can be accessed by executing the following commands:

```library(JSTMapp)```

```run_app()```

Alternatively, the user can access the web-based user interface by executing the following command:

```shiny::runGitHub("alfredngwira/JSTMapp",subdir="JSTMapp")```

To run the app in R, the user needs to have the following R dependencies installed:

```shiny, ggplot2,INLA,gridExtra,RColorBrewer,Hmisc,rmapshaper,shinyjs,dplyr,spdep,raster,tmap,tidyr,gstat```

This is so since all computations are done on the local computer thereby avoiding user disconnections due to memory problems.

## Example data
Bovine tuberculosis in cattle and extrapulmunary tuberculosis in humans in Africa is the example data used for illustration in the full article about JSTMapp which can be downloaded from [here](https://github.com/alfredngwira/JSTMapp/blob/main/JSTMapp/joint.csv), while the shape files can be downloaded from [here](https://github.com/alfredngwira/JSTMapp/blob/main/JSTMapp/Africa_Boundaries.zip).
