#Loading required packages
library(shiny)
library(ggplot2) 
library(INLA)
library(gridExtra)
library(RColorBrewer)
library(Hmisc) 
library(shinyjs) 
library(dplyr) 
library(spdep)
library(raster)
library(tmap)
library(tidyr)
library(gstat)
library(leaflet)
library(dotwhisker)
library(sn)

#Setting upload size for files and R

options(shiny.maxRequestSize=70*1024^2)

memory.size(max = FALSE)

ui <- fluidPage(
useShinyjs(), tags$head(
        tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      }
    "))
    ), 
   titlePanel(title=div(img(src="JSTMapp_logo.png", align = "right", height = 30, width = 100), "JSTM(Joint spatiotemporal modelling)")),
  sidebarLayout(
      sidebarPanel(
      fileInput(inputId="filedata", label="Upload data file (.csv):", accept = c("text/csv")),
      helpText("Select area, time, cases and population:"),
      fluidRow(column(6, uiOutput("id.area")),column(6, uiOutput("id.time"))),
      fluidRow(column(6, uiOutput("observed.1")),column(6, uiOutput("observed.2"))),
      fluidRow(column(6, uiOutput("population.1")),column(6, uiOutput("population.2")))
      ,
      conditionalPanel(condition="input.conditionedPanels==2",
      helpText("Select number of covariates, model and precision prior:"),
      fluidRow(
        column(4,radioButtons("numbercovariates","Covariates", choices = c("None", "One","Two","Three","Four"),inline=F,selected = "None") ),
        column(4,radioButtons("model","Model", choices = c("Spatial","Temporal","Spatial + Temporal","Spatiotemporal"),inline=F,selected = "Spatial") ), 
        column(4,radioButtons("precprior","Precision prior", choices = c("LogGamma", "Uniform","Half-Cauchy"),inline=F,selected = "LogGamma") )
       ),
      helpText("Select covariates (optional):"),
      fluidRow(column(6, uiOutput("covariate1")),column(6, uiOutput("covariate2"))),
      fluidRow(column(6, uiOutput("covariate3")),column(6, uiOutput("covariate4")))
         ),
      fileInput(inputId = "filemap", label = "Upload shapefile (.shp):",accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple=TRUE),
      conditionalPanel(condition="input.conditionedPanels==1",fluidRow(
        column(6, actionButton("showplots", "Show plots")))), conditionalPanel(condition="input.conditionedPanels==2", fluidRow(
      column(6, actionButton("showsummary", "Summary of results"))))    
)
 ,
mainPanel(
    tabsetPanel(type="pills",id = "conditionedPanels", 
        tabPanel("Explore",value = 1,fluidRow(fluidRow(
                            splitLayout(cellWidths = c("50%", "50%"),div(style = "margin-left:25px",leafletOutput("map1",height="300px")),leafletOutput("map2",height="300px"))
                          ), div(style = "margin-left:12px",plotOutput("plot1",height="350px"))
                        )
                 ),
        tabPanel("Model estimation",value = 2, verbatimTextOutput("summary"),plotOutput("plot2")),
        tabPanel("Spatial and temporal risk",value = 3, fluidRow(fluidRow(
                            splitLayout(div(style = "margin-left:40px",leafletOutput("map3",height="300px")), leafletOutput("map4",height="300px"),leafletOutput("map5",height="300px"))
                          ), fluidRow(
                            splitLayout(div(style = "margin-left:40px",leafletOutput("map6",height="200px")), leafletOutput("map7",height="200px"),leafletOutput("map8",height="200px"))

                          ),div(style = "margin-left:12px",plotOutput("plot4",height="200px")))
                        
                         
                        ),

        tabPanel("Spatiotemporal risk", value = 4,fluidRow(fluidRow(
                            splitLayout(cellWidths = c("50%", "50%"),div(style = "margin-left:40px",plotOutput("map9",height="250px")), plotOutput("map10",height="250px"))
                          ), fluidRow(
                            splitLayout(cellWidths = c("50%", "50%"),div(style = "margin-left:40px",plotOutput("map11",height="250px")), plotOutput("map12",height="250px"))
                          )
                        )
                ),

        tabPanel("Prediction",value = 5,fluidRow(fluidRow(splitLayout(cellWidths = c("50%", "50%"),div(style = "margin-left:40px",leafletOutput("map13",height="320px")), leafletOutput("map14",height="320px"))), 
                            fluidRow(splitLayout(cellWidths = c("50%", "50%"),div(style = "margin-left:40px",plotOutput("map15",height="250px")), plotOutput("map16",height="250px"))
                          )
                        )

                 ),
        tabPanel("Correlation",value = 6,width="auto",verbatimTextOutput("corr"))
                          ))
            ))

server <- function(input, output, session) {

   data.upload <- reactive({
    inFile<-input$filedata
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath)
  })

    
output$id.area <- renderUI({
    selectInput("id.area", label = "Area", choices = c("",colnames(data.upload())),multiple = F, 
selected=NULL)  
  })
output$id.time <- renderUI({
    selectInput("id.time", label = "Time", choices = c("",colnames(data.upload())),multiple = F, 
selected=NULL) 
  })
output$observed.1 <- renderUI({
    selectInput("observed.1", label = "Disease 1 observed", choices = c("",colnames(data.upload())),multiple = F, 
selected=NULL)  
  })
output$observed.2 <- renderUI({
    selectInput("observed.2", label = "Disease 2 observed", choices = c("",colnames(data.upload())),multiple = F, 
selected=NULL) 
  })
output$population.1 <- renderUI({
    selectInput("population.1", label = "Disease 1 population", choices = c("",colnames(data.upload())),multiple = F, 
selected=NULL)  
  })
output$population.2 <- renderUI({
    selectInput("population.2", label = "Disease 2 population", choices = c("",colnames(data.upload())),multiple = F, 
selected=NULL) 
  })
output$covariate1 <- renderUI({
   
    selectInput("covariate1", label = "Covariate 1", choices = c("",colnames(data.upload())),multiple = F, 
selected=NULL) 
  })
output$covariate2 <- renderUI({
    selectInput("covariate2", label = "Covariate 2", choices = c("",colnames(data.upload())),multiple = F, 
selected=NULL) 
  })
output$covariate3 <- renderUI({
    selectInput("covariate3", label = "Covariate 3", choices = c("",colnames(data.upload())),multiple = F, 
selected=NULL) 
  })
output$covariate4 <- renderUI({
    selectizeInput("covariate4", label = "Covariate 4", choices = c("",colnames(data.upload())),multiple = F, 
selected=NULL) 
  })

data.select0 <- reactive({
    data.select0 <- data.frame(data.upload()[[input$id.area]], data.upload()[[input$id.time]],data.upload()[[input$population.1]], data.upload()[[input$population.2]],
    data.upload()[[input$observed.1]], data.upload()[[input$observed.2]])
    colnames(data.select0) <- c("id.area","id.time","population.1","population.2","observed.1","observed.2")
    data.select0
  })


data.select1 <- reactive({
    data.select1 <- data.frame(data.upload()[[input$id.area]], data.upload()[[input$id.time]],data.upload()[[input$population.1]], data.upload()[[input$population.2]],
    data.upload()[[input$observed.1]], data.upload()[[input$observed.2]],data.upload()[[input$covariate1]])
    colnames(data.select1) <- c("id.area","id.time","population.1","population.2","observed.1","observed.2","covariate1")
    data.select1
  })
data.select2 <- reactive({
    data.select2 <- data.frame(data.upload()[[input$id.area]], data.upload()[[input$id.time]],data.upload()[[input$population.1]], data.upload()[[input$population.2]],
    data.upload()[[input$observed.1]], data.upload()[[input$observed.2]],data.upload()[[input$covariate1]],data.upload()[[input$covariate2]])
    colnames(data.select2) <- c("id.area","id.time","population.1","population.2","observed.1","observed.2","covariate1","covariate2")
    data.select2
  })
data.select3 <- reactive({
    data.select3 <- data.frame(data.upload()[[input$id.area]], data.upload()[[input$id.time]],data.upload()[[input$population.1]], data.upload()[[input$population.2]],
    data.upload()[[input$observed.1]], data.upload()[[input$observed.2]],data.upload()[[input$covariate1]],data.upload()[[input$covariate2]],
     data.upload()[[input$covariate3]])
    colnames(data.select3) <- c("id.area","id.time","population.1","population.2","observed.1","observed.2","covariate1","covariate2","covariate3")
    data.select3
  })
data.select4 <- reactive({
    data.select4 <- data.frame(data.upload()[[input$id.area]], data.upload()[[input$id.time]],data.upload()[[input$population.1]], data.upload()[[input$population.2]],
    data.upload()[[input$observed.1]], data.upload()[[input$observed.2]],data.upload()[[input$covariate1]],data.upload()[[input$covariate2]],
     data.upload()[[input$covariate3]],data.upload()[[input$covariate4]])
    colnames(data.select4) <- c("id.area","id.time","population.1","population.2","observed.1","observed.2","covariate1","covariate2","covariate3","covariate4")
    data.select4
  })


map <- reactive({
        shapejstm <- input$filemap
        if(is.null(shapejstm)){
        return()
        }
        previouswd <- getwd()
        uploaddirectory <- dirname(shapejstm$datapath[1])
        setwd(uploaddirectory)
        for(i in 1:nrow(shapejstm)){
        file.rename(shapejstm$datapath[i], shapejstm$name[i])
        }
        setwd(previouswd)
        map <- shapefile(paste(uploaddirectory, shapejstm$name[grep(pattern="*.shp$", shapejstm$name)], sep="/"))
        map
    })

##Explore output
observeEvent(input$showplots,{
output$map1 <- renderLeaflet({
region.data <- data.select0() %>%
  group_by(id.area) %>%
  summarise(
    count = n(),
    sumobserved.1 = sum(observed.1,na.rm=TRUE),
    sumpopulation.1=sum(population.1,na.rm=TRUE),
      )
region.data$ratio.1=sum(region.data$sumobserved.1)/sum(region.data$sumpopulation.1)
region.data$e1=region.data$sumpopulation.1*region.data$ratio.1
region.data$rr1<-region.data$sumobserved.1/region.data$e1
region.data$id<-region.data$id.area

#Map data
sfpolygon<-st_as_sf(map(),fill = TRUE, group = TRUE)
sfpolygon$id<-row.names(sfpolygon)
carto <- merge(sfpolygon,region.data, by="id")

paleta <- brewer.pal(10,"RdYlGn")[10:1]
values <- c(-Inf,0.05,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,Inf)
tmap_leaflet(tm_shape(carto)+ 
  tm_polygons(fill="rr1", col="black",col_alpha=0,fill.scale = tm_scale_intervals(values = paleta,breaks=values,
  value.na = "grey95"),fill.legend = tm_legend(show = T,title="Raw incidence ratio",reverse=T,na.show=T,size=0.7))+
tm_layout(inner.margins = c(0.00, 0.00, 0.00, 0.00),outer.margins = c(0.00, 0.00, 0.00, 0.00))+tm_title(input$observed.1)
,
in.shiny = TRUE)
   })
})

observeEvent(input$showplots,{
output$map2 <- renderLeaflet({
region.data <- data.select0() %>%
  group_by(id.area) %>%
  summarise(
    count = n(),
    sumobserved.2 = sum(observed.2,na.rm=TRUE),
    sumpopulation.2=sum(population.2,na.rm=TRUE)
      )
region.data$ratio.2=sum(region.data$sumobserved.2)/sum(region.data$sumpopulation.2)
region.data$e2=region.data$sumpopulation.2*region.data$ratio.2
region.data$rr2<-region.data$sumobserved.2/region.data$e2
region.data$id<-region.data$id.area

#Map data
sfpolygon<-st_as_sf(map(),fill = TRUE, group = TRUE)
sfpolygon$id<-row.names(sfpolygon)
carto <- merge(sfpolygon,region.data, by="id")

paleta <- brewer.pal(10,"RdYlGn")[10:1]
values <- c(-Inf,0.05,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,Inf)

tmap_leaflet(tm_shape(carto)+
  tm_polygons(fill="rr2",col="black",col_alpha=0,fill.scale = tm_scale_intervals(values = paleta, style = "fixed",breaks=values, midpoint = NA, interval.closure="left",
  value.na = "grey95"),fill.legend = tm_legend(show = T,title="Raw incidence ratio",reverse=T,na.show=T,size=0.7))+
tm_layout(legend.outside.position = tm_pos_out("right", "bottom"),inner.margins = c(0.00, 0.00, 0.00, 0.00),outer.margins = c(0.00, 0.00, 0.00, 0.00))+tm_title(input$observed.2),
in.shiny = TRUE)

  })
})


observeEvent(input$showplots,{
output$plot1 <- renderPlot({
temporal.data <- data.select0() %>%
                group_by(id.time) %>%
                summarise(
                count = n(),
                sumobserved.1 = sum(observed.1,na.rm=TRUE),
                sumobserved.2 = sum(observed.2,na.rm=TRUE),
                )

plot1<-ggplot(temporal.data, aes(x=id.time, y=sumobserved.1))+geom_line(size=1,linetype = "dotdash")+geom_text(aes(label = round(sumobserved.1, 1)),vjust=-0.5)+
       ylim(min(temporal.data$sumobserved.1)-0.1*min(temporal.data$sumobserved.1), max(temporal.data$sumobserved.1)+0.1*max(temporal.data$sumobserved.1))+ 
       labs(size=14, x="\nTime", y = "Number of cases\n")+scale_x_continuous(breaks=seq(min(temporal.data$id.time),max(temporal.data$id.time),1))+ggtitle(input$observed.1)+
       theme(plot.title = element_text(hjust = 0))

plot2<-ggplot(temporal.data, aes(x=id.time, y=sumobserved.2))+ylim(min(temporal.data$sumobserved.2)-0.1*min(temporal.data$sumobserved.2), max(temporal.data$sumobserved.2)+0.1*max(temporal.data$sumobserved.2))+
       geom_line(size=1,linetype = "dotdash")+labs(size=14, x="\nTime", y = "Number of cases\n")+geom_text(aes(label = round(sumobserved.2, 1)),vjust=-0.5)+
       scale_x_continuous(breaks=seq(min(temporal.data$id.time),max(temporal.data$id.time),1))+ggtitle(input$observed.2)+theme(plot.title = element_text(hjust = 0))


grid.arrange(plot1,plot2,nrow=2,ncol=1)  
})
})

### Setting up model data
model.data <- reactive({
### Setting up data for non covariate model
if (input$numbercovariates == "None"){
Data.select0<-data.frame(data.select0())
sum<- Data.select0 %>%
  group_by(id.time) %>%
  summarise(
  count = n(),
  sumobserved.1=sum(observed.1,na.rm=TRUE),
  sumpopulation.1=sum(population.1,na.rm=TRUE),
  ratio.1=sumobserved.1/sumpopulation.1,
  sumobserved.2 = sum(observed.2,na.rm=TRUE),
  sumpopulation.2 = sum(population.2,na.rm=TRUE),
  ratio.2=sumobserved.2/sumpopulation.2
  )

Data.select0<-merge(Data.select0,sum,by="id.time", all.x=T)
Data.select0$e1<-(Data.select0$population.1)*(Data.select0$ratio.1)
Data.select0$e2<-(Data.select0$population.2)*(Data.select0$ratio.2)

Data.select0 <- Data.select0[order(Data.select0$id.time,Data.select0$id.area), ]

model.data0<-data.frame(id.area=rep(Data.select0$id.area,2),id.time=rep(Data.select0$id.time,2),id.area.time=rep(seq(1,length(rep(Data.select0$id.area))),2))

model.data0$expected<-with(Data.select0,c(e1,e2)) 
model.data0$observed<-with(Data.select0,c(observed.1,observed.2))
 
#number of space-time units per disease
n.st<-nrow(Data.select0)
model.data0$disease<-rep(c("1","2"),each=n.st) 

#index for spatial counties ranges

model.data0$sp.idx<-model.data0$id.area

#index for time
model.data0$tm.idx<-model.data0$id.time-min(model.data0$id.time)+1

#index for space-time interaction
model.data0$sp.tm.idx=rep(seq(1,length(rep(Data.select0$id.area))),2)

#indices for effects
model.data0$dis.idx=rep(1:2,each=n.st)
model.data0$intercept=as.factor(model.data0$dis.idx)

#dummy indices for space, time and space-time
model.data0$sp.dum=NA
model.data0$tm.dum=NA
model.data0$sp.tm.dum=model.data0$id.area.time

#creating spatial indices for specific effects
model.data0$sp.1=NA
model.data0$sp.1[model.data0$dis.idx==1]=model.data0$sp.idx[model.data0$dis.idx==1]
model.data0$sp.2=NA
model.data0$sp.2[model.data0$dis.idx==2]=model.data0$sp.idx[model.data0$dis.idx==2]

#creating temporal indices for specific effects
model.data0$tm.1=NA
model.data0$tm.1[model.data0$dis.idx==1]=model.data0$tm.idx[model.data0$dis.idx==1]
model.data0$tm.2=NA
model.data0$tm.2[model.data0$dis.idx==2]=model.data0$tm.idx[model.data0$dis.idx==2]

#creating spatio-temporal indices for specific effects
model.data0$sp.tm.1=NA
model.data0$sp.tm.1[model.data0$dis.idx==1]=model.data0$sp.tm.idx[model.data0$dis.idx==1]
model.data0$sp.tm.2=NA
model.data0$sp.tm.2[model.data0$dis.idx==2]=model.data0$sp.tm.idx[model.data0$dis.idx==2]

#indices for spatial disease specific effects
model.data0$sp.idx1=model.data0$sp.1
model.data0$sp.idx2=model.data0$sp.2

#indices for temporal disease specific effects
model.data0$tm.idx1=model.data0$tm.1
model.data0$tm.idx2=model.data0$tm.2

#indices for spatiotemporal disease specific effects
model.data0$sp.tm.idx1=model.data0$sp.tm.1
model.data0$sp.tm.idx2=model.data0$sp.tm.2
return(model.data0)
}

else if (input$numbercovariates == "One"){
Data.select1<-data.frame(data.select1())
sum<- Data.select1 %>%
  group_by(id.time) %>%
  summarise(
  count = n(),
  sumobserved.1=sum(observed.1,na.rm=TRUE),
  sumpopulation.1=sum(population.1,na.rm=TRUE),
  ratio.1=sumobserved.1/sumpopulation.1,
  sumobserved.2 = sum(observed.2,na.rm=TRUE),
  sumpopulation.2 = sum(population.2,na.rm=TRUE),
  ratio.2=sumobserved.2/sumpopulation.2
  )

Data.select1<-merge(Data.select1,sum,by="id.time", all.x=T)
Data.select1$e1<-(Data.select1$population.1)*(Data.select1$ratio.1)
Data.select1$e2<-(Data.select1$population.2)*(Data.select1$ratio.2)

Data.select1 <- Data.select1[order(Data.select1$id.time,Data.select1$id.area), ]

model.data1<-data.frame(id.area=rep(Data.select1$id.area,2),id.time=rep(Data.select1$id.time,2),id.area.time=rep(seq(1,length(rep(Data.select1$id.area))),2),
covariate1=rep(Data.select1$covariate1,2))
model.data1$expected<-with(Data.select1,c(e1,e2)) 
model.data1$observed<-with(Data.select1,c(observed.1,observed.2))
 
#number of space-time units per disease
n.st<-nrow(Data.select1)
model.data1$disease<-rep(c("1","2"),each=n.st) 

#index for spatial counties ranges

model.data1$sp.idx<-model.data1$id.area

#index for time
model.data1$tm.idx<-model.data1$id.time-min(model.data1$id.time)+1

#index for space-time interaction
model.data1$sp.tm.idx=rep(seq(1,length(rep(Data.select1$id.area))),2)

#indices for effects
model.data1$dis.idx=rep(1:2,each=n.st)
model.data1$intercept=as.factor(model.data1$dis.idx)

#dummy indices for space, time and space-time
model.data1$sp.dum=NA
model.data1$tm.dum=NA
model.data1$sp.tm.dum=model.data1$id.area.time

#creating spatial indices for specific effects
model.data1$sp.1=NA
model.data1$sp.1[model.data1$dis.idx==1]=model.data1$sp.idx[model.data1$dis.idx==1]
model.data1$sp.2=NA
model.data1$sp.2[model.data1$dis.idx==2]=model.data1$sp.idx[model.data1$dis.idx==2]

#creating temporal indices for specific effects
model.data1$tm.1=NA
model.data1$tm.1[model.data1$dis.idx==1]=model.data1$tm.idx[model.data1$dis.idx==1]
model.data1$tm.2=NA
model.data1$tm.2[model.data1$dis.idx==2]=model.data1$tm.idx[model.data1$dis.idx==2]

#creating spatio-temporal indices for specific effects
model.data1$sp.tm.1=NA
model.data1$sp.tm.1[model.data1$dis.idx==1]=model.data1$sp.tm.idx[model.data1$dis.idx==1]
model.data1$sp.tm.2=NA
model.data1$sp.tm.2[model.data1$dis.idx==2]=model.data1$sp.tm.idx[model.data1$dis.idx==2]

#indices for spatial disease specific effects
model.data1$sp.idx1=model.data1$sp.1
model.data1$sp.idx2=model.data1$sp.2

#indices for temporal disease specific effects
model.data1$tm.idx1=model.data1$tm.1
model.data1$tm.idx2=model.data1$tm.2

#indices for spatiotemporal disease specific effects
model.data1$sp.tm.idx1=model.data1$sp.tm.1
model.data1$sp.tm.idx2=model.data1$sp.tm.2

#creating effects for covariates

model.data1$covariate1.1=NA
model.data1$covariate1.1[model.data1$dis.idx==1]=model.data1$covariate1[model.data1$dis.idx==1]

model.data1$covariate1.2=NA
model.data1$covariate1.2[model.data1$dis.idx==2]=model.data1$covariate1[model.data1$dis.idx==2]
return(model.data1)
} 
else if (input$numbercovariates == "Two"){
Data.select2<-data.frame(data.select2())
sum<- Data.select2 %>%
  group_by(id.time) %>%
  summarise(
  count = n(),
  sumobserved.1=sum(observed.1,na.rm=TRUE),
  sumpopulation.1=sum(population.1,na.rm=TRUE),
  ratio.1=sumobserved.1/sumpopulation.1,
  sumobserved.2 = sum(observed.2,na.rm=TRUE),
  sumpopulation.2 = sum(population.2,na.rm=TRUE),
  ratio.2=sumobserved.2/sumpopulation.2
  )

Data.select2<-merge(Data.select2,sum,by="id.time", all.x=T)
Data.select2$e1<-(Data.select2$population.1)*(Data.select2$ratio.1)
Data.select2$e2<-(Data.select2$population.2)*(Data.select2$ratio.2)

Data.select2 <- Data.select2[order(Data.select2$id.time,Data.select2$id.area), ]

model.data2<-data.frame(id.area=rep(Data.select2$id.area,2),id.time=rep(Data.select2$id.time,2),id.area.time=rep(seq(1,length(rep(Data.select2$id.area))),2),
covariate1=rep(Data.select2$covariate1,2),covariate2=rep(Data.select2$covariate2,2))
model.data2$expected<-with(Data.select2,c(e1,e2)) 
model.data2$observed<-with(Data.select2,c(observed.1,observed.2))
 
#number of space-time units per disease
n.st<-nrow(Data.select2)
model.data2$disease<-rep(c("1","2"),each=n.st) 

#index for spatial counties ranges

model.data2$sp.idx<-model.data2$id.area

#index for time
model.data2$tm.idx<-model.data2$id.time-min(model.data2$id.time)+1

#index for space-time interaction
model.data2$sp.tm.idx=rep(seq(1,length(rep(Data.select2$id.area))),2)

#indices for effects
model.data2$dis.idx=rep(1:2,each=n.st)
model.data2$intercept=as.factor(model.data2$dis.idx)

#dummy indices for space, time and space-time
model.data2$sp.dum=NA
model.data2$tm.dum=NA
model.data2$sp.tm.dum=model.data2$id.area.time

#creating spatial indices for specific effects
model.data2$sp.1=NA
model.data2$sp.1[model.data2$dis.idx==1]=model.data2$sp.idx[model.data2$dis.idx==1]
model.data2$sp.2=NA
model.data2$sp.2[model.data2$dis.idx==2]=model.data2$sp.idx[model.data2$dis.idx==2]

#creating temporal indices for specific effects
model.data2$tm.1=NA
model.data2$tm.1[model.data2$dis.idx==1]=model.data2$tm.idx[model.data2$dis.idx==1]
model.data2$tm.2=NA
model.data2$tm.2[model.data2$dis.idx==2]=model.data2$tm.idx[model.data2$dis.idx==2]

#creating spatio-temporal indices for specific effects
model.data2$sp.tm.1=NA
model.data2$sp.tm.1[model.data2$dis.idx==1]=model.data2$sp.tm.idx[model.data2$dis.idx==1]
model.data2$sp.tm.2=NA
model.data2$sp.tm.2[model.data2$dis.idx==2]=model.data2$sp.tm.idx[model.data2$dis.idx==2]

#indices for spatial disease specific effects
model.data2$sp.idx1=model.data2$sp.1
model.data2$sp.idx2=model.data2$sp.2

#indices for temporal disease specific effects
model.data2$tm.idx1=model.data2$tm.1
model.data2$tm.idx2=model.data2$tm.2

#indices for spatiotemporal disease specific effects
model.data2$sp.tm.idx1=model.data2$sp.tm.1
model.data2$sp.tm.idx2=model.data2$sp.tm.2

#creating effects for covariates

model.data2$covariate1.1=NA
model.data2$covariate1.1[model.data2$dis.idx==1]=model.data2$covariate1[model.data2$dis.idx==1]

model.data2$covariate1.2=NA
model.data2$covariate1.2[model.data2$dis.idx==2]=model.data2$covariate1[model.data2$dis.idx==2]

model.data2$covariate2.1=NA
model.data2$covariate2.1[model.data2$dis.idx==1]=model.data2$covariate2[model.data2$dis.idx==1]

model.data2$covariate2.2=NA
model.data2$covariate2.2[model.data2$dis.idx==2]=model.data2$covariate2[model.data2$dis.idx==2]

return(model.data2)
}
else if (input$numbercovariates == "Three"){
Data.select3<-data.frame(data.select3())
sum<- Data.select3 %>%
  group_by(id.time) %>%
  summarise(
  count = n(),
  sumobserved.1=sum(observed.1,na.rm=TRUE),
  sumpopulation.1=sum(population.1,na.rm=TRUE),
  ratio.1=sumobserved.1/sumpopulation.1,
  sumobserved.2 = sum(observed.2,na.rm=TRUE),
  sumpopulation.2 = sum(population.2,na.rm=TRUE),
  ratio.2=sumobserved.2/sumpopulation.2
  )

Data.select3<-merge(Data.select3,sum,by="id.time", all.x=T)
Data.select3$e1<-(Data.select3$population.1)*(Data.select3$ratio.1)
Data.select3$e2<-(Data.select3$population.2)*(Data.select3$ratio.2)
Data.select3 <- Data.select3[order(Data.select3$id.time,Data.select3$id.area), ]

model.data3<-data.frame(id.area=rep(Data.select3$id.area,2),id.time=rep(Data.select3$id.time,2),id.area.time=rep(seq(1,length(rep(Data.select3$id.area))),2),
covariate1=rep(Data.select3$covariate1,2),covariate2=rep(Data.select3$covariate2,2),covariate3=rep(Data.select3$covariate3,2))
model.data3$expected<-with(Data.select3,c(e1,e2)) 
model.data3$observed<-with(Data.select3,c(observed.1,observed.2))
 
#number of space-time units per disease
n.st<-nrow(Data.select3)
model.data3$disease<-rep(c("1","2"),each=n.st) 

#index for spatial counties ranges

model.data3$sp.idx<-model.data3$id.area

#index for time
model.data3$tm.idx<-model.data3$id.time-min(model.data3$id.time)+1

#index for space-time interaction
model.data3$sp.tm.idx=rep(seq(1,length(rep(Data.select3$id.area))),2)

#indices for effects
model.data3$dis.idx=rep(1:2,each=n.st)
model.data3$intercept=as.factor(model.data3$dis.idx)

#dummy indices for space, time and space-time
model.data3$sp.dum=NA
model.data3$tm.dum=NA
model.data3$sp.tm.dum=model.data3$id.area.time

#creating spatial indices for specific effects
model.data3$sp.1=NA
model.data3$sp.1[model.data3$dis.idx==1]=model.data3$sp.idx[model.data3$dis.idx==1]
model.data3$sp.2=NA
model.data3$sp.2[model.data3$dis.idx==2]=model.data3$sp.idx[model.data3$dis.idx==2]

#creating temporal indices for specific effects
model.data3$tm.1=NA
model.data3$tm.1[model.data3$dis.idx==1]=model.data3$tm.idx[model.data3$dis.idx==1]
model.data3$tm.2=NA
model.data3$tm.2[model.data3$dis.idx==2]=model.data3$tm.idx[model.data3$dis.idx==2]

#creating spatio-temporal indices for specific effects
model.data3$sp.tm.1=NA
model.data3$sp.tm.1[model.data3$dis.idx==1]=model.data3$sp.tm.idx[model.data3$dis.idx==1]
model.data3$sp.tm.2=NA
model.data3$sp.tm.2[model.data3$dis.idx==2]=model.data3$sp.tm.idx[model.data3$dis.idx==2]

#indices for spatial disease specific effects
model.data3$sp.idx1=model.data3$sp.1
model.data3$sp.idx2=model.data3$sp.2

#indices for temporal disease specific effects
model.data3$tm.idx1=model.data3$tm.1
model.data3$tm.idx2=model.data3$tm.2

#indices for spatiotemporal disease specific effects
model.data3$sp.tm.idx1=model.data3$sp.tm.1
model.data3$sp.tm.idx2=model.data3$sp.tm.2

#creating effects for covariates

model.data3$covariate1.1=NA
model.data3$covariate1.1[model.data3$dis.idx==1]=model.data3$covariate1[model.data3$dis.idx==1]

model.data3$covariate1.2=NA
model.data3$covariate1.2[model.data3$dis.idx==2]=model.data3$covariate1[model.data3$dis.idx==2]

model.data3$covariate2.1=NA
model.data3$covariate2.1[model.data3$dis.idx==1]=model.data3$covariate2[model.data3$dis.idx==1]

model.data3$covariate2.2=NA
model.data3$covariate2.2[model.data3$dis.idx==2]=model.data3$covariate2[model.data3$dis.idx==2]

model.data3$covariate3.1=NA
model.data3$covariate3.1[model.data3$dis.idx==1]=model.data3$covariate3[model.data3$dis.idx==1]

model.data3$covariate3.2=NA
model.data3$covariate3.2[model.data3$dis.idx==2]=model.data3$covariate3[model.data3$dis.idx==2]

return(model.data3)
}
else if (input$numbercovariates == "Four"){
Data.select4<-data.frame(data.select4())
sum<- Data.select4 %>%
  group_by(id.time) %>%
  summarise(
  count = n(),
  sumobserved.1=sum(observed.1,na.rm=TRUE),
  sumpopulation.1=sum(population.1,na.rm=TRUE),
  ratio.1=sumobserved.1/sumpopulation.1,
  sumobserved.2 = sum(observed.2,na.rm=TRUE),
  sumpopulation.2 = sum(population.2,na.rm=TRUE),
  ratio.2=sumobserved.2/sumpopulation.2
  )

Data.select4<-merge(Data.select4,sum,by="id.time", all.x=T)
Data.select4$e1<-(Data.select4$population.1)*(Data.select4$ratio.1)
Data.select4$e2<-(Data.select4$population.2)*(Data.select4$ratio.2)

Data.select4 <- Data.select4[order(Data.select4$id.time,Data.select4$id.area), ]

model.data4<-data.frame(id.area=rep(Data.select4$id.area,2),id.time=rep(Data.select4$id.time,2),id.area.time=rep(seq(1,length(rep(Data.select4$id.area))),2),
covariate1=rep(Data.select4$covariate1,2),covariate2=rep(Data.select4$covariate2,2),covariate3=rep(Data.select4$covariate3,2),covariate4=rep(Data.select4$covariate4,2))
model.data4$expected<-with(Data.select4,c(e1,e2)) 
model.data4$observed<-with(Data.select4,c(observed.1,observed.2))
 
#number of space-time units per disease
n.st<-nrow(Data.select4)
model.data4$disease<-rep(c("1","2"),each=n.st) 

#index for spatial counties ranges

model.data4$sp.idx<-model.data4$id.area

#index for time
model.data4$tm.idx<-model.data4$id.time-min(model.data4$id.time)+1

#index for space-time interaction
model.data4$sp.tm.idx=rep(seq(1,length(rep(Data.select4$id.area))),2)

#indices for effects
model.data4$dis.idx=rep(1:2,each=n.st)
model.data4$intercept=as.factor(model.data4$dis.idx)

#dummy indices for space, time and space-time
model.data4$sp.dum=NA
model.data4$tm.dum=NA
model.data4$sp.tm.dum=model.data4$id.area.time

#creating spatial indices for specific effects
model.data4$sp.1=NA
model.data4$sp.1[model.data4$dis.idx==1]=model.data4$sp.idx[model.data4$dis.idx==1]
model.data4$sp.2=NA
model.data4$sp.2[model.data4$dis.idx==2]=model.data4$sp.idx[model.data4$dis.idx==2]

#creating temporal indices for specific effects
model.data4$tm.1=NA
model.data4$tm.1[model.data4$dis.idx==1]=model.data4$tm.idx[model.data4$dis.idx==1]
model.data4$tm.2=NA
model.data4$tm.2[model.data4$dis.idx==2]=model.data4$tm.idx[model.data4$dis.idx==2]

#creating spatio-temporal indices for specific effects
model.data4$sp.tm.1=NA
model.data4$sp.tm.1[model.data4$dis.idx==1]=model.data4$sp.tm.idx[model.data4$dis.idx==1]
model.data4$sp.tm.2=NA
model.data4$sp.tm.2[model.data4$dis.idx==2]=model.data4$sp.tm.idx[model.data4$dis.idx==2]

#indices for spatial disease specific effects
model.data4$sp.idx1=model.data4$sp.1
model.data4$sp.idx2=model.data4$sp.2

#indices for temporal disease specific effects
model.data4$tm.idx1=model.data4$tm.1
model.data4$tm.idx2=model.data4$tm.2

#indices for spatiotemporal disease specific effects
model.data4$sp.tm.idx1=model.data4$sp.tm.1
model.data4$sp.tm.idx2=model.data4$sp.tm.2

#creating effects for covariates

model.data4$covariate1.1=NA
model.data4$covariate1.1[model.data4$dis.idx==1]=model.data4$covariate1[model.data4$dis.idx==1]

model.data4$covariate1.2=NA
model.data4$covariate1.2[model.data4$dis.idx==2]=model.data4$covariate1[model.data4$dis.idx==2]

model.data4$covariate2.1=NA
model.data4$covariate2.1[model.data4$dis.idx==1]=model.data4$covariate2[model.data4$dis.idx==1]

model.data4$covariate2.2=NA
model.data4$covariate2.2[model.data4$dis.idx==2]=model.data4$covariate2[model.data4$dis.idx==2]

model.data4$covariate3.1=NA
model.data4$covariate3.1[model.data4$dis.idx==1]=model.data4$covariate3[model.data4$dis.idx==1]

model.data4$covariate3.2=NA
model.data4$covariate3.2[model.data4$dis.idx==2]=model.data4$covariate3[model.data4$dis.idx==2]

model.data4$covariate4.1=NA
model.data4$covariate4.1[model.data4$dis.idx==1]=model.data4$covariate4[model.data4$dis.idx==1]

model.data4$covariate4.2=NA
model.data4$covariate4.2[model.data4$dis.idx==2]=model.data4$covariate4[model.data4$dis.idx==2]

return(model.data4)
}

})

##Precision priors
prior.prec<-reactive ({
if (input$precprior == "LogGamma"){
prior.prec = list(prior = "loggamma", param = c(0.5, 0.0005), initial = 0)

return(prior.prec)

} 
else if (input$precprior == "Uniform"){
prior.prec = list(prior = 
  "expression:
   logdens = -log_precision / 2;
   return(logdens)",
  initial = 0)

return(prior.prec)

} 
else if (input$precprior == "Half-Cauchy"){
 prior.prec = list(prior = 
  "expression:
   scale_param = 25;
   logdens = -3.670459 -log(1 + (exp(-theta) / (scale_param^2))) - (theta/2);
   return(logdens)",
  initial = 0)
return(prior.prec)
}
})

## Model fitting
joint.inla<-reactive({
#spatial adjacency matrix
nb <- poly2nb(map())
w.sp=as(nb2mat(nb,style="B",zero.policy=TRUE),"Matrix")

#temporal adjacency
t<-length(unique(model.data()$id.time))
w.tm=Diagonal(t,x=0)
w.tm[1,1+1]=1
w.tm[t,(t-1)]=1
for (i in 2:(t-1)) {
  w.tm[i,i-1]=1
  w.tm[i,i+1]=1
}

#prior for coefficients of copied effects
prior.beta.s=list(prior="normal",param=c(0,1/5.9), fixed=FALSE,
                  initial=0.01)
prior.beta.t=list(prior="normal",param=c(0,1/5.9), fixed=FALSE,
                  initial=0.01)
prior.beta.s.t=list(prior="normal",param=c(0,1/5.9), fixed=FALSE,
                    initial=0.01)

prior.fixed <- list(mean.intercept = 0, prec.intercept = 0.001,
                    mean = 0, prec = 0.001)

#scale models?
inla.scale<-FALSE

#Model formula
if (input$numbercovariates=="None") {
    if (input$model=="Spatial") {
  formula<-observed~-1+intercept+
  f(sp.idx1,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.idx2,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.dum,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.1,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(sp.2,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))
    } else if (input$model=="Temporal") {
  formula<-observed~-1+intercept+
    f(tm.idx1,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
    f(tm.idx2,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
    f(tm.dum,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
    f(tm.1,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))+
    f(tm.2,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))
    } else if (input$model=="Spatial + Temporal") {
formula<-observed~-1+intercept+
  f(sp.idx1,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.idx2,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.dum,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.1,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(sp.2,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(tm.idx1,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.idx2,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.dum,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.1,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))+
  f(tm.2,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))
  } else if (input$model=="Spatiotemporal"){
  formula<-observed~-1+intercept+
  f(sp.idx1,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.idx2,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.dum,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.1,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(sp.2,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(tm.idx1,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.idx2,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.dum,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.1,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))+
  f(tm.2,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))+
  f(sp.tm.idx1,model="iid",hyper=list(prec=prior.prec()))+
  f(sp.tm.idx2,model="iid",hyper=list(prec=prior.prec()))+
  f(sp.tm.dum,model="iid",hyper=list(prec=prior.prec()))+
  f(sp.tm.1,copy="sp.tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s.t))+
  f(sp.tm.2,copy="sp.tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s.t))
    }
} else if (input$numbercovariates=="One") {
  if (input$model=="Spatial") {
  formula<-observed~-1+intercept+covariate1.1+covariate1.2+
  f(sp.idx1,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.idx2,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.dum,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.1,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(sp.2,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))
  } else if (input$model=="Temporal") {
formula<-observed~-1+intercept+covariate1.1+covariate1.2+
  f(tm.idx1,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.idx2,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.dum,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.1,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))+
  f(tm.2,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))
    } else if (input$model=="Spatial + Temporal") {
formula<-observed~-1+intercept+covariate1.1+covariate1.2+
  f(sp.idx1,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.idx2,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.dum,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.1,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(sp.2,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(tm.idx1,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.idx2,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.dum,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.1,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))+
  f(tm.2,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))
  } 
else if (input$model=="Spatiotemporal") {
formula<-observed~-1+intercept+covariate1.1+covariate1.2+
  f(sp.idx1,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.idx2,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.dum,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.1,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(sp.2,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(tm.idx1,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.idx2,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.dum,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.1,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))+
  f(tm.2,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))+
  f(sp.tm.idx1,model="iid",hyper=list(prec=prior.prec()))+
  f(sp.tm.idx2,model="iid",hyper=list(prec=prior.prec()))+
  f(sp.tm.dum,model="iid",hyper=list(prec=prior.prec()))+
  f(sp.tm.1,copy="sp.tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s.t))+
  f(sp.tm.2,copy="sp.tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s.t))
    }
}else if (input$numbercovariates=="Two") {
if (input$model=="Spatial") {
formula<-observed~-1+intercept+covariate1.1+covariate1.2+covariate2.1+covariate2.2+
  f(sp.idx1,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.idx2,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.dum,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.1,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(sp.2,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))
} else if (input$model=="Temporal") {
formula<-observed~-1+intercept+covariate1.1+covariate1.2+covariate2.1+covariate2.2+
  f(tm.idx1,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.idx2,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.dum,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.1,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))+
  f(tm.2,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))
} else if (input$model=="Spatial + Temporal") {
formula<-observed~-1+intercept+covariate1.1+covariate1.2+covariate2.1+covariate2.2+
  f(sp.idx1,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.idx2,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.dum,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.1,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(sp.2,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(tm.idx1,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.idx2,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.dum,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.1,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))+
  f(tm.2,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))

} else if (input$model=="Spatiotemporal") {
formula<-observed~-1+intercept+covariate1.1+covariate1.2+covariate2.1+covariate2.2+
  f(sp.idx1,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.idx2,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.dum,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.1,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(sp.2,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(tm.idx1,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.idx2,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.dum,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.1,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))+
  f(tm.2,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))+
  f(sp.tm.idx1,model="iid",hyper=list(prec=prior.prec()))+
  f(sp.tm.idx2,model="iid",hyper=list(prec=prior.prec()))+
  f(sp.tm.dum,model="iid",hyper=list(prec=prior.prec()))+
  f(sp.tm.1,copy="sp.tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s.t))+
  f(sp.tm.2,copy="sp.tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s.t))
  }

} else if (input$numbercovariates=="Three") {
if (input$model=="Spatial") {
formula<-observed~-1+intercept+covariate1.1+covariate1.2+covariate2.1+covariate2.2+covariate3.1+covariate3.2+
  f(sp.idx1,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.idx2,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.dum,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.1,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(sp.2,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))

} else if (input$model=="Temporal") {
formula<-observed~-1+intercept+covariate1.1+covariate1.2+covariate2.1+covariate2.2+covariate3.1+covariate3.2+
  f(tm.idx1,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.idx2,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.dum,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.1,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))+
  f(tm.2,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))
} else if (input$model=="Spatial + Temporal") {
formula<-observed~-1+intercept+covariate1.1+covariate1.2+covariate2.1+covariate2.2+covariate3.1+covariate3.2+
  f(sp.idx1,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.idx2,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.dum,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.1,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(sp.2,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(tm.idx1,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.idx2,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.dum,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.1,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))+
  f(tm.2,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))
} else if (input$model=="Spatiotemporal") {
formula<-observed~-1+intercept+covariate1.1+covariate1.2+covariate2.1+covariate2.2+covariate3.1+covariate3.2+
  f(sp.idx1,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.idx2,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.dum,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.1,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(sp.2,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(tm.idx1,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.idx2,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.dum,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.1,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))+
  f(tm.2,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))+
  f(sp.tm.idx1,model="iid",hyper=list(prec=prior.prec()))+
  f(sp.tm.idx2,model="iid",hyper=list(prec=prior.prec()))+
  f(sp.tm.dum,model="iid",hyper=list(prec=prior.prec()))+
  f(sp.tm.1,copy="sp.tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s.t))+
  f(sp.tm.2,copy="sp.tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s.t))
  }
} else if (input$numbercovariates=="Four") {
if (input$model=="Spatial") {
formula<-observed~-1+intercept+covariate1.1+covariate1.2+covariate2.1+covariate2.2+covariate3.1+covariate3.2+covariate4.1+covariate4.2+
  f(sp.idx1,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.idx2,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.dum,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.1,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(sp.2,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))
} else if (input$model=="Temporal") {
formula<-observed~-1+intercept+covariate1.1+covariate1.2+covariate2.1+covariate2.2+covariate3.1+covariate3.2+covariate4.1+covariate4.2+
  f(tm.idx1,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.idx2,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.dum,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.1,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))+
  f(tm.2,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))
} else if (input$model=="Spatial + Temporal") {
formula<-observed~-1+intercept+covariate1.1+covariate1.2+covariate2.1+covariate2.2+covariate3.1+covariate3.2+covariate4.1+covariate4.2+
  f(sp.idx1,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.idx2,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.dum,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.1,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(sp.2,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(tm.idx1,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.idx2,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.dum,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.1,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))+
  f(tm.2,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))
} else if (input$model=="Spatiotemporal") {
formula<-observed~-1+intercept+covariate1.1+covariate1.2+covariate2.1+covariate2.2+covariate3.1+covariate3.2+covariate4.1+covariate4.2+
  f(sp.idx1,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.idx2,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.dum,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(sp.1,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(sp.2,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(tm.idx1,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.idx2,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.dum,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec()))+
  f(tm.1,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))+
  f(tm.2,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))+
  f(sp.tm.idx1,model="iid",hyper=list(prec=prior.prec()))+
  f(sp.tm.idx2,model="iid",hyper=list(prec=prior.prec()))+
  f(sp.tm.dum,model="iid",hyper=list(prec=prior.prec()))+
  f(sp.tm.1,copy="sp.tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s.t))+
  f(sp.tm.2,copy="sp.tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s.t))
  }
}

fit<-inla(formula,family="poisson",E=expected,data=model.data(),
                control.predictor=list(compute=TRUE),verbose=TRUE,
                control.compute=list(return.marginals.predictor=TRUE,config=TRUE,dic=TRUE,cpo=TRUE),
                control.inla(npoints=21,strategy="laplace"))
return(fit)

})


##Model estimation summaries

observeEvent(input$showsummary,{

output$summary <- renderPrint({
summary(joint.inla()) 

  })
})

observeEvent(input$showsummary,{
output$plot2 <- renderPlot({
fixed<-data.frame(joint.inla()$summary.fixed)
fixed_table <- data.frame(
  term = rownames(fixed),
  estimate = fixed[,1],
  conf.low = fixed[,3],
  conf.high = fixed[,5]
  )

hyperpar<-data.frame(joint.inla()$summary.hyperpar)
hyperpar_table <- data.frame(
  term = rownames(hyperpar),
  estimate = hyperpar[,1],
  conf.low = hyperpar[,3],
  conf.high = hyperpar[,5]
  )

plot1<-dwplot(fixed_table,dot_args = list(color = "blue"), whisker_args = list(color = "blue"))+theme(legend.position = "none")+ggtitle("Fixed effects")
plot2<-dwplot(hyperpar_table,dot_args = list(color = "blue"), whisker_args = list(color = "blue"))+theme(legend.position = "none")+ggtitle("Random effects")

grid.arrange(plot1,plot2,nrow=1,ncol=2)  

  })
})


##Spatial and temporal risk 
spatial.data <- reactive({
a=0
Prob1<-lapply(joint.inla()$marginals.random[[1]], function(X){
  1-inla.pmarginal(a, X)
})
Prob1=unlist(Prob1)

a=0
Prob2<-lapply(joint.inla()$marginals.random[[2]], function(X){
  1-inla.pmarginal(a, X)
})
Prob2=unlist(Prob2)

a=0
Prob3<-lapply(joint.inla()$marginals.random[[3]], function(X){
  1-inla.pmarginal(a, X)
})
Prob3=unlist(Prob3)
sfpolygon<-st_as_sf(map(),fill = TRUE, group = TRUE)
sfpolygon$id<-row.names(sfpolygon)

n<-length(unique(sfpolygon$id))

da1<-data.frame(id=1:n, rr=matrix(exp(joint.inla()$summary.random$sp.idx1[,"mean"]),nrow=n,ncol=1))
da1$prob<-Prob1

da2<-data.frame(id=1:n, rr=matrix(exp(joint.inla()$summary.random$sp.idx2[,"mean"]),nrow=n,ncol=1))
da2$prob<-Prob2

da3<-data.frame(id=1:n, rr=matrix(exp(joint.inla()$summary.random$sp.dum[,"mean"]),nrow=n,ncol=1))
da3$prob<-Prob3

carto1 <- merge(sfpolygon,da1, by="id")
carto2 <- merge(sfpolygon,da2, by="id")
carto3 <- merge(sfpolygon,da3, by="id")

return(
    list(
      sfpolygon = sfpolygon,
      carto1 = carto1,
      carto2 = carto2,
      carto3 = carto3
    )
  )

})

output$map3 <- renderLeaflet({
validate(
  need(
    if (input$model=="Spatial" | input$model=="Spatial + Temporal" | input$model=="Spatiotemporal") {
      TRUE
    } else {
      FALSE
    },
    "map omitted-your model is non spatial"
  )
)

paleta <- brewer.pal(10,"RdYlGn")[10:1]
values <- c(-Inf,0.05,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,Inf)
tmap_leaflet(tm_shape(spatial.data()$carto1)+
  tm_polygons(fill="rr",col="black",col_alpha=0,fill.scale = tm_scale_intervals(values = paleta, style = "fixed",breaks=values, midpoint = NA, interval.closure="left"),
  fill.legend = tm_legend(show = T,title="Relative risk",reverse=T,size=0.8))+
tm_layout(inner.margins = c(0.00, 0.00, 0.00, 0.00),outer.margins = c(0.00, 0.00, 0.00, 0.00))+tm_title(input$observed.1),in.shiny=TRUE)
   
})


output$map4 <- renderLeaflet({
validate(
  need(
    if (input$model=="Spatial" | input$model=="Spatial + Temporal" | input$model=="Spatiotemporal") {
      TRUE
    } else {
      FALSE
    },
    "map omitted-your model is non spatial"
  )
)
paleta <- brewer.pal(10,"RdYlGn")[10:1]
values <- c(-Inf,0.05,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,Inf)
tmap_leaflet(tm_shape(spatial.data()$carto2)+
  tm_polygons(fill="rr",col="black",col_alpha=0,fill.scale = tm_scale_intervals(values = paleta, style = "fixed",breaks=values, midpoint = NA, interval.closure="left"),
  fill.legend = tm_legend(show = T,title="Relative risk",reverse=T,size=0.8))+
tm_layout(inner.margins = c(0.00, 0.00, 0.00, 0.00),outer.margins = c(0.00, 0.00, 0.00, 0.00))+tm_title(input$observed.2)
,in.shiny=TRUE)
})


output$map5 <- renderLeaflet({
validate(
  need(
    if (input$model=="Spatial" | input$model=="Spatial + Temporal" | input$model=="Spatiotemporal") {
      TRUE
    } else {
      FALSE
    },
    "map omitted-your model is non spatial"
  )
)
paleta <- brewer.pal(8,"RdYlGn")[8:1]
values <- c(-Inf,0.05,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,Inf)
tmap_leaflet(tm_shape(spatial.data()$carto3)+
  tm_polygons(fill="rr",col="black",col_alpha=0,fill.scale = tm_scale_intervals(values = paleta, style = "fixed",breaks=values, midpoint = NA, interval.closure="left"),
  fill.legend = tm_legend(show = T,title="Relative risk",reverse=T,size=0.8))+
tm_layout(inner.margins = c(0.00, 0.00, 0.00, 0.00),outer.margins = c(0.00, 0.00, 0.00, 0.00))+tm_title("Shared")
,in.shiny=TRUE)
})


output$map6 <- renderLeaflet({
validate(
  need(
    if (input$model=="Spatial" | input$model=="Spatial + Temporal" | input$model=="Spatiotemporal") {
      TRUE
    } else {
      FALSE
    },
    "map omitted-your model is non spatial"
  )
)
#Posterior probability plot
paleta <- brewer.pal(4,"RdYlGn")[4:1]
values <- c(0,0.5,0.8,0.9,1)
tmap_leaflet(tm_shape(spatial.data()$carto1)+ 
tm_polygons(fill="prob",col="black",col_alpha=0,fill.scale = tm_scale_intervals(values = paleta, style = "fixed",breaks=values, midpoint = NA, interval.closure="left"),
  fill.legend = tm_legend(show = T,title="Pr(Risk>1)",reverse=T, size=0.8))+
tm_layout(inner.margins = c(0.00, 0.00, 0.00, 0.00),outer.margins = c(0.00, 0.00, 0.00, 0.00))+tm_title(input$observed.1)
,in.shiny=TRUE)
})

output$map7 <- renderLeaflet({
validate(
  need(
    if (input$model=="Spatial" | input$model=="Spatial + Temporal" | input$model=="Spatiotemporal") {
      TRUE
    } else {
      FALSE
    },
    "map omitted-your model is non spatial"
  )
)
#Posterior probability plot
paleta <- brewer.pal(4,"RdYlGn")[4:1]
values <- c(0,0.5,0.8,0.9,1)
tmap_leaflet(tm_shape(spatial.data()$carto2)+ 
tm_polygons(fill="prob",col="black",col_alpha=0,fill.scale = tm_scale_intervals(values = paleta, style = "fixed",breaks=values, midpoint = NA, interval.closure="left"),
  fill.legend = tm_legend(show = T,title="Pr(Risk>1)",reverse=T,size=0.8))+
tm_layout(inner.margins = c(0.00, 0.00, 0.00, 0.00),outer.margins = c(0.00, 0.00, 0.00, 0.00))+tm_title(input$observed.2),in.shiny=TRUE)
})


output$map8 <- renderLeaflet({
validate(
  need(
    if (input$model=="Spatial" | input$model=="Spatial + Temporal" | input$model=="Spatiotemporal") {
      TRUE
    } else {
      FALSE
    },
    "map omitted-your model is non spatial"
  )
)
#Posterior probability plot
paleta <- brewer.pal(4,"RdYlGn")[4:1]
values <- c(0,0.5,0.8,0.9,1)
tmap_leaflet(tm_shape(spatial.data()$carto3)+ 
tm_polygons(fill="prob",col="black",col_alpha=0,fill.scale = tm_scale_intervals(values = paleta, style = "fixed",breaks=values, midpoint = NA, interval.closure="left"),
  fill.legend = tm_legend(show = T,title="Pr(Risk>1)",reverse=T,size=0.8))+
tm_layout(inner.margins = c(0.00, 0.00, 0.00, 0.00),outer.margins = c(0.00, 0.00, 0.00, 0.00))+tm_title("Shared"),in.shiny=TRUE)

})

output$plot4 <- renderPlot({
validate(
  need(
    if (input$model=="Temporal" | input$model=="Spatial + Temporal" | input$model=="Spatiotemporal") {
      TRUE
    } else {
      FALSE
    },
    "plot omitted-your model has no temporal component"
  )
)
rr<-exp(joint.inla()$summary.random$tm.idx1[,"mean"])
temporal.data1<-data.frame(time=min(data.select0()$id.time):max(data.select0()$id.time),rr)
temporal.data1$disease=input$observed.1

rr<-exp(joint.inla()$summary.random$tm.idx2[,"mean"])
temporal.data2<-data.frame(time=min(data.select0()$id.time):max(data.select0()$id.time),rr)
temporal.data2$disease=input$observed.2

rr<-exp(joint.inla()$summary.random$tm.dum[,"mean"])
temporal.data3<-data.frame(time=min(data.select0()$id.time):max(data.select0()$id.time),rr)
temporal.data3$disease="Shared"

temporal.data<-rbind(temporal.data1,temporal.data2,temporal.data3)

ggplot(temporal.data, aes(x=time, y=rr, color =disease, shape=disease)) + scale_shape_manual(values=c(15,16,15))+
geom_line(size=1,linetype = "dotdash")+labs(color="Category",size=14, x="\nTime", y = "Relative risk\n") +scale_color_manual(values=c("orange", "green", "yellow"))+
scale_x_continuous(limits=c(NA,max(data.select0()$id.time)),breaks=seq(min(data.select0()$id.time),max(data.select0()$id.time),1))+guides(shape="none")
  
})


##Spatiotemporal risk
output$map9<- renderPlot({
model.data<-data.frame(model.data())
model.data$rr <- joint.inla()$summary.fitted.values[,"mean"] 
a<-1
probability<-unlist(lapply(joint.inla()$marginals.fitted.values, function(X){
  1-inla.pmarginal(a, X)
}))
model.data$prob<-probability
model.data$id<-model.data$id.area
dat1<-model.data[model.data$disease=="1",]
sfpolygon<-st_as_sf(map(),fill = TRUE, group = TRUE)
sfpolygon$id<-row.names(sfpolygon)
carto <- merge(sfpolygon,dat1,by="id", all.x=TRUE)
paleta <- brewer.pal(10,"RdYlGn")[10:1]
values <- c(-Inf,0.05,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,Inf)
tm_shape(carto)+
  tm_polygons(fill="rr",col="black",col_alpha=0,fill.scale = tm_scale_intervals(values = paleta, style = "fixed",breaks=values, midpoint = NA, interval.closure="left",value.na = "white"),
  fill.legend = tm_legend(show = T,title="Relative risk",reverse=T,size=0.25,frame=F,na.show=T))+tm_title(input$observed.1) +
  tm_layout(panel.label.size=1,panel.labels=as.character(round(seq(min(data.select0()$id.time), max(data.select0()$id.time),length.out=(length(unique(data.select0()$id.time))))))) +
  tm_facets(by="id.time",free.coords = TRUE,drop.NA.facets=T)
 })

output$map10<- renderPlot({
model.data<-data.frame(model.data())
model.data$rr <- joint.inla()$summary.fitted.values[,"mean"] 
a<-1
probability<-unlist(lapply(joint.inla()$marginals.fitted.values, function(X){
  1-inla.pmarginal(a, X)
}))
model.data$prob<-probability
model.data$id<-model.data$id.area
dat2<-model.data[model.data$disease=="2",]
sfpolygon<-st_as_sf(map(),fill = TRUE, group = TRUE)
sfpolygon$id<-row.names(sfpolygon)
carto <- merge(sfpolygon,dat2,by="id", all.x=TRUE)
paleta <- brewer.pal(10,"RdYlGn")[10:1]
values <- c(-Inf,0.05,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,Inf)

tm_shape(carto)+
  tm_polygons(fill="rr",col="black",col_alpha=0,fill.scale = tm_scale_intervals(values = paleta, style = "fixed",breaks=values, midpoint = NA, interval.closure="left",value.na = "white"),
  fill.legend = tm_legend(show = T,title="Relative risk",reverse=T,size=0.25,frame=F,na.show=T))+tm_title(input$observed.2) +
  tm_layout(panel.label.size=1,panel.labels=as.character(round(seq(min(data.select0()$id.time), max(data.select0()$id.time),length.out=(length(unique(data.select0()$id.time))))))) + tmap_options(component.autoscale = TRUE)+
  tm_facets(by="id.time",drop.NA.facets=T)
})


output$map11<- renderPlot({
model.data<-data.frame(model.data())
model.data$rr <- joint.inla()$summary.fitted.values[,"mean"] 
a<-1
probability<-unlist(lapply(joint.inla()$marginals.fitted.values, function(X){
  1-inla.pmarginal(a, X)
}))
model.data$prob<-probability
model.data$id<-model.data$id.area
dat1<-model.data[model.data$disease=="1",]
sfpolygon<-st_as_sf(map(),fill = TRUE, group = TRUE)
sfpolygon$id<-row.names(sfpolygon)
carto <- merge(sfpolygon,dat1,by="id", all.x=TRUE)
#Posterior probability plot
paleta <- brewer.pal(4,"RdYlGn")[4:1]
values <- c(0,0.5,0.8,0.9,1)
tm_shape(carto)+ 
tm_polygons(fill="prob",col="black",col_alpha=0,fill.scale = tm_scale_intervals(values = paleta, style = "fixed",breaks=values, midpoint = NA, interval.closure="left",value.na = "white"),
  fill.legend = tm_legend(show = T,title="Pr(Risk>1)",reverse=T,size=0.25,frame=F,na.show=T))+tm_title(input$observed.1) +
  tm_layout(panel.label.size=1,panel.labels=as.character(round(seq(min(data.select0()$id.time), max(data.select0()$id.time),length.out=(length(unique(data.select0()$id.time))))))) + tmap_options(component.autoscale = TRUE)+
  tm_facets(by="id.time",drop.NA.facets=T)
})


output$map12<- renderPlot({
model.data<-data.frame(model.data())
model.data$rr <- joint.inla()$summary.fitted.values[,"mean"] 
a<-1
probability<-unlist(lapply(joint.inla()$marginals.fitted.values, function(X){
  1-inla.pmarginal(a, X)
}))
model.data$prob<-probability
model.data$id<-model.data$id.area
dat2<-model.data[model.data$disease=="2",]
sfpolygon<-st_as_sf(map(),fill = TRUE, group = TRUE)
sfpolygon$id<-row.names(sfpolygon)
carto <- merge(sfpolygon,dat2,by="id", all.x=TRUE)
#Posterior probability plot
paleta <- brewer.pal(4,"RdYlGn")[4:1]
values <- c(0,0.5,0.8,0.9,1)
tm_shape(carto)+ 
tm_polygons(fill="prob",col="black",col_alpha=0,fill.scale = tm_scale_intervals(values = paleta, style = "fixed",breaks=values, midpoint = NA, interval.closure="left",value.na = "white"),
  fill.legend = tm_legend(show = T,title="Pr(Risk>1)",reverse=T,size=0.25,frame=F,na.show=T))+tm_title(input$observed.2) +
  tm_layout(panel.label.size=1,panel.labels=as.character(round(seq(min(data.select0()$id.time), max(data.select0()$id.time),length.out=(length(unique(data.select0()$id.time))))))) + tmap_options(component.autoscale = TRUE)+
  tm_facets(by="id.time",drop.NA.facets=T)
})

#13 and 14
prediction.raw <- reactive({
region.data1 <- data.select0() %>%
  group_by(id.area) %>%
  summarise(
    count = n(),
    sumobserved = sum(observed.1,na.rm=TRUE),
    sumpopulation=sum(population.1,na.rm=TRUE),
      )
region.data2 <- data.select0() %>%
  group_by(id.area) %>%
  summarise(
    count = n(),
    sumobserved = sum(observed.2,na.rm=TRUE),
    sumpopulation=sum(population.2,na.rm=TRUE)
)
region.data1$ratio=sum(region.data1$sumobserved)/sum(region.data1$sumpopulation)
region.data2$ratio=sum(region.data2$sumobserved)/sum(region.data2$sumpopulation)
region.data1$e=region.data1$sumpopulation*region.data1$ratio
region.data2$e=region.data2$sumpopulation*region.data2$ratio
region.data1$rr<-region.data1$sumobserved/region.data1$e
region.data2$rr<-region.data2$sumobserved/region.data2$e
region.data1$disease<-"1"
region.data2$disease<-"2"
region.data<- rbind(region.data1, region.data2)

krige.data<-region.data[,c("id.area","disease","rr")]
krige.data1<-pivot_wider(krige.data,names_from="disease",names_prefix="RR",values_from="rr")
krige.data2<-data.frame(long=coordinates(map())[,1],lat=coordinates(map())[,2],id.area=row.names(coordinates(map())))
krige.data3<-merge(krige.data1,krige.data2, by="id.area",all=TRUE)
coordinates(krige.data3)<-~long+lat
proj4string(krige.data3)<- CRS("+proj=longlat +datum=WGS84")

grid<- makegrid(map(),n = 10000)
grid <- SpatialPoints(grid, proj4string = CRS(proj4string(map())))
grid <- grid[map(), ]
gridded(grid)=TRUE

int<-list()
for (i in names(krige.data3[ ,grepl("RR",names(krige.data3))])) {
data<-krige.data3[!is.na(krige.data3[[i]]),]
formula<-as.formula(paste0(i, " ~ 1"))
int[[i]] <- krige(formula,data,grid)
}

RRforecast <-int[[1]][,-c(1,2)]
for (i in names(krige.data3[ ,grepl("RR",names(krige.data3))])) {
RRforecast[[i]]<-int[[i]][[1]]
}

RRforecast<-data.frame(RRforecast)

RRforecast<- RRforecast %>% 
pivot_longer(cols=starts_with("RR"),
names_to = c(".value", "disease"),
names_pattern = "(.*?)(\\d+)" 
)

RRforecast1<-subset(RRforecast, RRforecast$disease==1)
RRforecast2<-subset(RRforecast, RRforecast$disease==2)

RRforecastsf1  <- st_as_sf(RRforecast1, coords = c("x1", "x2"), crs = 4326)
RRforecastsf2  <- st_as_sf(RRforecast2, coords = c("x1", "x2"), crs = 4326)
return(
    list(
      RRforecastsf1 = RRforecastsf1,
      RRforecastsf2 = RRforecastsf2
    )
  )

})

output$map13<- renderLeaflet({
paleta <- brewer.pal(10,"RdYlGn")[10:1]
values <- c(-Inf,0.05,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,Inf)
tmap_leaflet(tm_shape(prediction.raw()$RRforecastsf1) + tm_dots(fill="RR",fill.scale = tm_scale_intervals(values = paleta, breaks=values),fill.legend = tm_legend(show=F,size=0.8))+
tm_title(input$observed.1)+
tm_layout(inner.margins = c(0.00, 0.00, 0.00, 0.00),outer.margins = c(0.00, 0.00, 0.00, 0.00))+tm_add_legend(type="polygons", 
fill = paleta,col_alpha=0,labels = c("Less than 0.05","0.05 to 0.10","0.10 to 0.20","0.20 to 0.40","0.40 to 0.60","0.60 to 0.80","0.80 to 1.00","1.00 to 1.20","1.20 to 1.40","1.40 or more"),
reverse=T,title = "Raw incidence ratio")
,in.shiny=TRUE)
})

output$map14<- renderLeaflet({
paleta <- brewer.pal(10,"RdYlGn")[10:1]
values <- c(-Inf,0.05,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,Inf)
tmap_leaflet(tm_shape(prediction.raw()$RRforecastsf2) + tm_dots(fill="RR",fill.scale = tm_scale_intervals(values = paleta, breaks=values),fill.legend = tm_legend(show=F,size=0.8))+
tm_title(input$observed.2)+
tm_layout(inner.margins = c(0.00, 0.00, 0.00, 0.00),outer.margins = c(0.00, 0.00, 0.00, 0.00))+tm_add_legend(type="polygons", 
fill = paleta,col_alpha=0,labels = c("Less than 0.05","0.05 to 0.10","0.10 to 0.20","0.20 to 0.40","0.40 to 0.60","0.60 to 0.80","0.80 to 1.00","1.00 to 1.20","1.20 to 1.40","1.40 or more"),
reverse=T,title = "Raw incidence ratio")
,in.shiny=TRUE)
})

output$map15<- renderPlot({
model.data<-data.frame(model.data())

model.data$rr <- joint.inla()$summary.fitted.values[,"mean"]   #Extracting all risk  values

krige.data<-model.data[model.data$disease=="1",c("id.area","id.time","rr")]
krige.data1<-pivot_wider(krige.data,names_from="id.time",names_prefix="RR",values_from="rr")

krige.data2<-data.frame(long=coordinates(map())[,1],lat=coordinates(map())[,2],id.area=row.names(coordinates(map())))
krige.data3<-merge(krige.data1,krige.data2, by="id.area",all=TRUE)
coordinates(krige.data3)<-~long+lat
proj4string(krige.data3) <- CRS("+proj=longlat +datum=WGS84")

grid<- makegrid(map(),n = 10000)    #Creating a prediction grid
grid <- SpatialPoints(grid, proj4string = CRS(proj4string(map())))
grid <- grid[map(), ]
gridded(grid)=TRUE

int<-list()
for (i in names(krige.data3[ ,grepl('RR',names(krige.data3))])) {
data<-krige.data3[!is.na(krige.data3[[i]]),]
formula<-as.formula(paste0(i, " ~ 1"))
 int[[i]] <- krige(formula,data,grid)
}

RRforecast <-int[[1]][,-c(1,2)]
for (i in names(krige.data3[ ,grepl("RR",names(krige.data3))])) {
RRforecast[[i]]<-int[[i]][[1]]
}

RRforecast<-as.data.frame(RRforecast)

RRforecast<- RRforecast %>% 
    pivot_longer(cols=starts_with("RR"),
    names_to = c(".value", "id.time"),
    names_pattern = "(.*?)(\\d+)" 
)

RRforecastsf <- st_as_sf(RRforecast,coords = c('x1', 'x2'), crs = 4326)

paleta <- brewer.pal(10,"RdYlGn")[10:1]
values <- c(-Inf,0.05,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,Inf)
tm_shape(RRforecastsf) + tm_dots(fill="RR",fill.scale = tm_scale_intervals(values = paleta, breaks=values),fill.legend = tm_legend(show=F))+
tm_title(input$observed.1)+
tm_layout(inner.margins = c(0.00, 0.00, 0.00, 0.00),outer.margins = c(0.00, 0.00, 0.00, 0.00),panel.label.size=1)+tm_add_legend(type="polygons", 
fill = paleta,col_alpha=0,labels = c("Less than 0.05","0.05 to 0.10","0.10 to 0.20","0.20 to 0.40","0.40 to 0.60","0.60 to 0.80","0.80 to 1.00","1.00 to 1.20","1.20 to 1.40","1.40 or more"),
reverse=T,title = "Estimated risk")+tm_facets(by="id.time")

})


output$map16<- renderPlot({
model.data<-data.frame(model.data())

model.data$rr <- joint.inla()$summary.fitted.values[,"mean"]  #Extracting all risk  values

krige.data<-model.data[model.data$disease=="2",c("id.area","id.time","rr")]  #Subsetting risk value data for disease 2
krige.data1<-pivot_wider(krige.data,names_from="id.time",names_prefix="RR",values_from="rr")

krige.data2<-data.frame(long=coordinates(map())[,1],lat=coordinates(map())[,2],id.area=row.names(coordinates(map())))
krige.data3<-merge(krige.data1,krige.data2, by="id.area",all=TRUE)
coordinates(krige.data3)<-~long+lat
proj4string(krige.data3) <- CRS("+proj=longlat +datum=WGS84")

grid<- makegrid(map(),n = 10000)
grid <- SpatialPoints(grid, proj4string = CRS(proj4string(map())))
grid <- grid[map(), ]
gridded(grid)=TRUE

int<-list()
for (i in names(krige.data3[ ,grepl('RR',names(krige.data3))])) {
data<-krige.data3[!is.na(krige.data3[[i]]),]
formula<-as.formula(paste0(i, " ~ 1"))
 int[[i]] <- krige(formula,data,grid)
}

RRforecast <-int[[1]][,-c(1,2)]
for (i in names(krige.data3[ ,grepl("RR",names(krige.data3))])) {
RRforecast[[i]]<-int[[i]][[1]]
}

RRforecast<-as.data.frame(RRforecast)

RRforecast<- RRforecast %>% 
    pivot_longer(cols=starts_with("RR"),
    names_to = c(".value", "id.time"),
    names_pattern = "(.*?)(\\d+)" 
)

RRforecastsf <- st_as_sf(RRforecast,coords = c('x1', 'x2'), crs = 4326)

paleta <- brewer.pal(10,"RdYlGn")[10:1]
values <- c(-Inf,0.05,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,Inf)
tm_shape(RRforecastsf) + tm_dots(fill="RR",fill.scale = tm_scale_intervals(values = paleta, breaks=values),fill.legend = tm_legend(show = F))+
tm_title(input$observed.2)+
tm_layout(inner.margins = c(0.00, 0.00, 0.00, 0.00),outer.margins = c(0.00, 0.00, 0.00, 0.00),panel.label.size=1)+
tm_add_legend(type="polygons", 
fill = paleta,bg.alpha = 0,col_alpha=0,labels = c("Less than 0.05","0.05 to 0.10","0.10 to 0.20","0.20 to 0.40","0.40 to 0.60","0.60 to 0.80","0.80 to 1.00","1.00 to 1.20","1.20 to 1.40","1.40 or more"),
reverse=T,title = "Estimated risk")+tm_facets(by="id.time")
})


##Correlation
output$corr <- renderPrint({
samples <- inla.posterior.sample(2000, joint.inla())
if (input$model=="Spatial" | input$model=="Temporal") {
betas <- sapply(1:2000, function(x) (samples[[x]]$hyperpar)[4:5])
} else if (input$model=="Spatial + Temporal") {
betas <- sapply(1:2000, function(x) (samples[[x]]$hyperpar)[7:10])
} else if (input$model=="Spatiotemporal") {
betas <- sapply(1:2000, function(x) (samples[[x]]$hyperpar)[10:15])
}
betas <- as.data.frame(t(betas))
rcorr(as.matrix(betas))
})


}

shinyApp(ui = ui, server = server)

