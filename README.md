# JSTMapp (Joint spatio-temporal modelling application)
### Purpose of the software
JSTMapp was developed to ease the fitting of joint spatio-temporal models by R INLA to epidemiologists with limited programming skills. In its current status, it impliments joint spatio-temporal modelling of two diseases with no covariates other than space and time.
### How to use the software
The app can used by following the shiny page https://alfredngwira.shinyapps.io/JSTMapp.

It can also be used by downloading the app from Github author page in R software as 

```devtools::install_github("alfredngwira/JSTMapp", ref="main")```

After downloading the app in R, it can then be run by executing the following commands:

```library(JSTMapp)```

```run_app()```
