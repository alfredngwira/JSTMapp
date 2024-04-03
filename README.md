# JSTMapp (Joint spatio-temporal modelling application)
## Purpose of the software
JSTMapp was developed to ease the fitting of joint spatio-temporal models by R INLA to epidemiologists with limited programming skills. In its current status, it impliments joint spatio-temporal modelling of two diseases with no covariates other than space and time.
## How to use the software
### Online
The app can used online by following the shiny link https://alfredngwira.shinyapps.io/JSTMapp. In this regard, computations are done at the shiny server and the user computer does not need to have R software. The challenges associated with the online usage is that the memory associated with the free plan at shiny server memory is usually not eneough for some heavy computations which results in disconnections. 
### In R/ RStudio
This can be done by downloading the app from Github author page while in R by executing the command: 

```devtools::install_github("alfredngwira/JSTMapp", ref="main")```

After downloading the app in R, the web-based user interface can be brought by executing the following commands:

```library(JSTMapp)```

```run_app()```

Alternatively, the user can access the web-based user interface by executing the folloing command:

To run the app in R, the user needs to have all the R dependencies installed. The use of the app in R is better than online since there are no memory problems as computations are done on the local computer.

