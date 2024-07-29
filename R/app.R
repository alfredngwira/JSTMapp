rm(list=ls())

#Loading required packages
library("shiny")
library("ggplot2") 
library("INLA")
library("gridExtra")
library("RColorBrewer")
library("Hmisc") 
library("rmapshaper")
library("shinyjs") 
library("dplyr") 
library("spdep") 
library("raster")
library("tmap")
library("tidyr")
library("gstat")



#Setting upload size for files and R

options(shiny.maxRequestSize=70*1024^2)

memory.size(max = FALSE)

# define the user interface object with the appearance of the app

ui <- fluidPage(
useShinyjs(),
  titlePanel("JSTMapp"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId="filedata", label="Upload data file (csv)", accept = c("text/csv")),helpText("Select variables"),
      uiOutput("id.area"),uiOutput("id.time"),uiOutput("observed.1"),
      uiOutput("observed.2"),uiOutput("population.1"),uiOutput("population.2"),
      fileInput(inputId = "filemap", label = "Upload the shapefile (optional)",accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple=TRUE)
)
 ,
mainPanel(
    tabsetPanel(type="pills",
        tabPanel("Explore", fluidRow(fluidRow(
                            splitLayout(cellWidths = c("50%", "50%"),div(style = "margin-left:21px",plotOutput("map1",height="200px")),plotOutput("map2",height="200px"))
                          ), div(style = "margin-left:12px",plotOutput("plot1",height="350px"))
                        )
                 ),
        tabPanel("Model estimation", verbatimTextOutput("summary")),
        tabPanel("Spatial and temporal risk", plotOutput("map3",height="250px"),plotOutput("plot2",height="250px")),
        tabPanel("Spatio-temporal risk", fluidRow(fluidRow(
                            splitLayout(cellWidths = c("50%", "50%"),div(style = "margin-left:40px",plotOutput("map4",height="250px")), plotOutput("map5",height="250px"))
                          ), fluidRow(
                            splitLayout(cellWidths = c("50%", "50%"),div(style = "margin-left:40px",plotOutput("map6",height="250px")), plotOutput("map7",height="250px"))
                          )
                        )
                ),

        tabPanel("Future prediction",fluidRow(fluidRow(
                            splitLayout(cellWidths = c("50%", "50%"),div(style = "margin-left:36px",plotOutput("map8",height="250px")),plotOutput("map9",height="250px"))
                          ), div(style = "margin-left:21px",plotOutput("plot3",height="250px"))
                        )
                 ),
        tabPanel("Correlation",width="auto",verbatimTextOutput("corr"))
                          ))
            ))

server <- function(input, output, session) {

  data.upload <- reactive({
    inFile<-input$filedata
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath)
  })
output$id.area <- renderUI({
    selectInput("id.area", label = "Area", choices = colnames(data.upload()))  
  })
output$id.time <- renderUI({
    selectInput("id.time", label = "Time", choices = colnames(data.upload()) ) 
  })
output$observed.1 <- renderUI({
    selectInput("observed.1", label = "Observed 1", choices = colnames(data.upload()))  
  })
output$observed.2 <- renderUI({
    selectInput("observed.2", label = "Observed 2", choices = colnames(data.upload()) ) 
  })
output$population.1 <- renderUI({
    selectInput("population.1", label = "Population 1", choices = colnames(data.upload()))  
  })
output$population.2 <- renderUI({
    selectInput("population.2", label = "Population 2", choices = colnames(data.upload()) ) 
  })

data.select <- reactive({
    select <- data.frame(data.upload()[[input$id.area]], data.upload()[[input$id.time]],data.upload()[[input$population.1]], data.upload()[[input$population.2]],
    data.upload()[[input$observed.1]], data.upload()[[input$observed.2]])
    colnames(select) <- c("id.area","id.time","population.1","population.2","observed.1","observed.2")
    select
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
output$map1 <- renderPlot({
region.data <- data.select() %>%
  group_by(id.area) %>%
  summarise(
    count = n(),
    sumobserved.1 = sum(observed.1,na.rm=TRUE),
    sumobserved.2 = sum(observed.2,na.rm=TRUE),
    sumpopulation.1=sum(population.1,na.rm=TRUE),
    sumpopulation.2=sum(population.2,na.rm=TRUE)
      )

region.data$ratio.1=sum(region.data$sumobserved.1)/sum(region.data$sumpopulation.1)
region.data$ratio.2=sum(region.data$sumobserved.2)/sum(region.data$sumpopulation.2)
region.data$e1=region.data$sumpopulation.1*region.data$ratio.1
region.data$e2=region.data$sumpopulation.2*region.data$ratio.2
region.data$rr<-region.data$sumobserved.1/region.data$e1
region.data$id<-region.data$id.area

#Map data
shape<-ms_filter_islands(map(), min_area = 12391399903) #Removing small islands
sfpolygon<-st_as_sf(shape,fill = TRUE, group = TRUE)
sfpolygon$id<-row.names(sfpolygon)
carto1 <- merge(sfpolygon,region.data, by="id")

region.data$rr<-region.data$sumobserved.2/region.data$e2
carto2 <- merge(sfpolygon,region.data, by="id")
carto1$disease = "Disease 1"
carto2$disease = "Disease 2"
carto.total <- rbind(carto1,carto2)

paleta <- brewer.pal(6,"RdYlGn")[6:1]
values <- c(-Inf,0.05,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,Inf)
tm_shape(carto.total)+
  tm_polygons(col="rr",
              palette=paleta,title="Raw incidence ratio", legend.show=T, border.col="transparent",
              legend.reverse=T, style="fixed", breaks=values, midpoint = NA, interval.closure="left",showNA=TRUE,colorNA="white") +
  tm_layout(main.title= "",panel.label.size=1,
            legend.outside=T,legend.outside.position="right",
            legend.outside.size=0.25)+tmap_options(check.and.fix = TRUE)+
  tm_facets(by="disease",showNA=FALSE)
})

output$map2 <- renderPlot({
region.data1 <- data.select() %>%
  group_by(id.area) %>%
  summarise(
    count = n(),
    sumobserved = sum(observed.1,na.rm=TRUE),
    sumpopulation=sum(population.1,na.rm=TRUE),
      )
region.data2 <- data.select() %>%
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
shape<-ms_filter_islands(map(), min_area = 12391399903)        #Removing small islands
krige.data2<-data.frame(long=coordinates(shape)[,1],lat=coordinates(shape)[,2],id.area=row.names(coordinates(shape)))
krige.data3<-merge(krige.data1,krige.data2, by="id.area",all=TRUE)
coordinates(krige.data3)<-~long+lat
proj4string(krige.data3)<- CRS("+proj=longlat +datum=WGS84")

grid<- makegrid(shape,n = 10000)
grid <- SpatialPoints(grid, proj4string = CRS(proj4string(shape)))
grid <- grid[shape, ]
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

RRforecastsf  <- st_as_sf(RRforecast, coords = c("x1", "x2"), crs = 4326)

paleta <- brewer.pal(10,"RdYlGn")[10:1]
values <- c(-Inf,0.05,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,Inf)
tm_shape(RRforecastsf) + tm_dots(col="RR",breaks=values,palette=paleta,legend.show=F) +
tm_layout(main.title= "", panel.labels = c("Disease 1","Disease 2"),panel.label.size=1,legend.outside.size=0.25)+tm_facets(by="disease",nrow=1)+tm_add_legend(type="fill", 
col = paleta,border.alpha=0,size=0.25,title = "Predicted incidence ratio",labels = c("Less than 0.05","0.05 to 0.10","0.10 to 0.20","0.20 to 0.40","0.40 to 0.60","0.60 to 0.80","0.80 to 1.00","1.00 to 1.20","1.20 to 1.40","1.40 or more"),
reverse=T)
})

output$plot1 <- renderPlot({
temporal.data <- data.select() %>%
                group_by(id.time) %>%
                summarise(
                count = n(),
                sumobserved.1 = sum(observed.1,na.rm=TRUE),
                sumobserved.2 = sum(observed.2,na.rm=TRUE),
                )

plot1<-ggplot(temporal.data, aes(x=id.time, y=sumobserved.1))+geom_line(size=1,linetype = "dotdash")+geom_text(aes(label = round(sumobserved.1, 1)),vjust=-0.5)+
       ylim(min(temporal.data$sumobserved.1)-0.1*min(temporal.data$sumobserved.1), max(temporal.data$sumobserved.1)+0.1*max(temporal.data$sumobserved.1))+ 
       labs(size=14, x="\nTime", y = "Number of cases\n")+scale_x_continuous(breaks=seq(min(temporal.data$id.time),max(temporal.data$id.time),1))+ggtitle("Disease 1")+
       theme(plot.title = element_text(hjust = 0))+theme_classic(base_size = 12)

plot2<-ggplot(temporal.data, aes(x=id.time, y=sumobserved.2))+ylim(min(temporal.data$sumobserved.2)-0.1*min(temporal.data$sumobserved.2), max(temporal.data$sumobserved.2)+0.1*max(temporal.data$sumobserved.2))+
       geom_line(size=1,linetype = "dotdash")+labs(size=14, x="\nTime", y = "Number of cases\n")+geom_text(aes(label = round(sumobserved.2, 1)),vjust=-0.5)+
       scale_x_continuous(breaks=seq(min(temporal.data$id.time),max(temporal.data$id.time),1))+ggtitle("Disease 2")+theme(plot.title = element_text(hjust = 0))+theme_classic(base_size = 12)


grid.arrange(plot1,plot2,nrow=2,ncol=1)  

})

## Model data
model.data <- reactive({
select.data<-data.frame(data.select())
sum<- select.data %>%
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

select.data<-merge(select.data,sum,by="id.time", all.x=T)
select.data$e1<-(select.data$population.1)*(select.data$ratio.1)
select.data$e2<-(select.data$population.2)*(select.data$ratio.2)

extended.data<-select.data %>% 
          mutate(id.time = list((max(id.time)+1):(max(id.time)+2)))%>% 
          group_by(id.area)%>% 
          unnest(cols = c(id.time))%>% 
          ungroup()%>%
          dplyr::select(id.area,id.time)%>%
          distinct

extended.data$observed.1<-NA
extended.data$observed.2<-NA
extended.data$e1 <- mean(select.data$e1)
extended.data$e2 <- mean(select.data$e2)
select.data<-full_join(select.data,extended.data)

select.data <- select.data[order(select.data$id.time,select.data$id.area), ]
 
model.data<-data.frame(id.area=rep(select.data$id.area,2),id.time=rep(select.data$id.time,2),id.area.time=rep(seq(1,length(rep(select.data$id.area))),2))
model.data$expected<-with(select.data,c(e1,e2)) 
model.data$observed<-with(select.data,c(observed.1,observed.2))
 
#number of space-time units per disease
n.st<-nrow(select.data)
model.data$disease<-rep(c("disease1","disease2"),each=n.st) 

#index for spatial counties ranges

model.data$sp.idx<-model.data$id.area

#index for time
model.data$tm.idx<-model.data$id.time-min(model.data$id.time)+1

#index for space-time interaction
model.data$sp.tm.idx=rep(seq(1,length(rep(select.data$id.area))),2)

#indices for effects
model.data$dis.idx=rep(1:2,each=n.st)
model.data$intercept=as.factor(model.data$dis.idx)

#dummy indices for space, time and space-time
model.data$sp.dum=NA
model.data$tm.dum=NA
model.data$sp.tm.dum=model.data$id.area.time

#creating spatial indices for specific effects
model.data$sp.1=NA
model.data$sp.1[model.data$dis.idx==1]=model.data$sp.idx[model.data$dis.idx==1]
model.data$sp.2=NA
model.data$sp.2[model.data$dis.idx==2]=model.data$sp.idx[model.data$dis.idx==2]

#creating temporal indices for specific effects
model.data$tm.1=NA
model.data$tm.1[model.data$dis.idx==1]=model.data$tm.idx[model.data$dis.idx==1]
model.data$tm.2=NA
model.data$tm.2[model.data$dis.idx==2]=model.data$tm.idx[model.data$dis.idx==2]

#creating spatio-temporal indices for specific effects
model.data$sp.tm.1=NA
model.data$sp.tm.1[model.data$dis.idx==1]=model.data$sp.tm.idx[model.data$dis.idx==1]
model.data$sp.tm.2=NA
model.data$sp.tm.2[model.data$dis.idx==2]=model.data$sp.tm.idx[model.data$dis.idx==2]

#indices for spatial disease specific effects
model.data$sp.idx1=model.data$sp.1
model.data$sp.idx2=model.data$sp.2

#indices for temporal disease specific effects
model.data$tm.idx1=model.data$tm.1
model.data$tm.idx2=model.data$tm.2

#indices for spatiotemporal disease specific effects
model.data$sp.tm.idx1=model.data$sp.tm.1
model.data$sp.tm.idx2=model.data$sp.tm.2

model.data
})

## Model fitting
joint.inla<-reactive({
#spatial adjacency matrix
shape<-ms_filter_islands(map(), min_area = 12391399903) #Removing small islands from the map
nb <- poly2nb(shape)
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

# Gamma prior on precision
prior.prec = list(prior = "loggamma", param = c(0.5, 0.0005), initial = 0)

#scale models?
inla.scale=FALSE

#Model formula
formula<-observed~-1+intercept+
  f(sp.idx1,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec))+
  f(sp.idx2,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec))+
  f(sp.dum,model="besag",graph=w.sp,scale.model=inla.scale,
    hyper=list(prec=prior.prec))+
  f(sp.1,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(sp.2,copy="sp.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s))+
  f(tm.idx1,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec))+
  f(tm.idx2,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec))+
  f(tm.dum,model="besag",graph=w.tm,scale.model=inla.scale,
    hyper=list(prec=prior.prec))+
  f(tm.1,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))+
  f(tm.2,copy="tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.t))+
  f(sp.tm.idx1,model="iid",hyper=list(prec=prior.prec))+
  f(sp.tm.idx2,model="iid",hyper=list(prec=prior.prec))+
  f(sp.tm.dum,model="iid",hyper=list(prec=prior.prec))+
  f(sp.tm.1,copy="sp.tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s.t))+
  f(sp.tm.2,copy="sp.tm.dum",range=c(0,Inf),hyper=list(beta=prior.beta.s.t))

#Calling INLA to fit the model
fit<-inla(formula,family="poisson",E=expected,data=model.data(),
                control.predictor=list(compute=TRUE),verbose=TRUE,
                control.compute=list(return.marginals.predictor=TRUE,config=TRUE,dic=TRUE,cpo=TRUE),
                control.inla(npoints=21,strategy="laplace"))
return(
    list(
      fit = fit,
      shape = shape
    )
  )

})


##Model estimation summaries
output$summary <- renderPrint({
  summary(joint.inla()$fit) 
})


##Spatial and temporal risk 
spatial.data <- reactive({
a=0
Prob1<-lapply(joint.inla()$fit$marginals.random[[1]], function(X){
  1-inla.pmarginal(a, X)
})
Prob1=unlist(Prob1)

a=0
Prob2<-lapply(joint.inla()$fit$marginals.random[[2]], function(X){
  1-inla.pmarginal(a, X)
})
Prob2=unlist(Prob2)

a=0
Prob3<-lapply(joint.inla()$fit$marginals.random[[3]], function(X){
  1-inla.pmarginal(a, X)
})
Prob3=unlist(Prob3)
sfpolygon<-st_as_sf(joint.inla()$shape,fill = TRUE, group = TRUE)
sfpolygon$id<-row.names(sfpolygon)

n<-length(unique(sfpolygon$id))

da1<-data.frame(id=1:n, rr=matrix(exp(joint.inla()$fit$summary.random$sp.idx1[,"mean"]),nrow=n,ncol=1))
da1$prob<-Prob1

da2<-data.frame(id=1:n, rr=matrix(exp(joint.inla()$fit$summary.random$sp.idx2[,"mean"]),nrow=n,ncol=1))
da2$prob<-Prob2

da3<-data.frame(id=1:n, rr=matrix(exp(joint.inla()$fit$summary.random$sp.dum[,"mean"]),nrow=n,ncol=1))
da3$prob<-Prob3

carto1 <- merge(sfpolygon,da1, by="id")
carto2 <- merge(sfpolygon,da2, by="id")
carto3 <- merge(sfpolygon,da3, by="id")
carto1$disease = "Disease 1"
carto2$disease = "Disease 2"
carto3$disease = "Shared"
carto.total <- rbind(carto1, carto2, carto3)

return(
    list(
      sfpolygon = sfpolygon,
      carto.total = carto.total
    )
  )

})

output$map3 <- renderPlot({
paleta <- brewer.pal(6,"RdYlGn")[6:1]
values <- c(-Inf,0.05,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,Inf)
tm_shape(spatial.data()$carto.total)+
  tm_polygons(col="rr",
              palette=paleta,title="Relative risk", legend.show=T, border.col="transparent",
              legend.reverse=T, style="fixed", breaks=values, midpoint = NA, interval.closure="left") +
  tm_layout(main.title= "",panel.label.size=1,
            legend.outside=T,legend.outside.position="right",
            legend.outside.size=0.25,asp=1)+tmap_options(check.and.fix = TRUE)+
  tm_facets(by="disease")
})

output$plot2 <- renderPlot({
rr<-exp(joint.inla()$fit$summary.random$tm.idx1[,"mean"])
temporal.data1<-data.frame(time=min(data.select()$id.time):(max(data.select()$id.time)+2),rr)
temporal.data1$disease="Disease 1"

rr<-exp(joint.inla()$fit$summary.random$tm.idx2[,"mean"])
temporal.data2<-data.frame(time=min(data.select()$id.time):(max(data.select()$id.time)+2),rr)
temporal.data2$disease="Disease 2"

rr<-exp(joint.inla()$fit$summary.random$tm.dum[,"mean"])
temporal.data3<-data.frame(time=min(data.select()$id.time):(max(data.select()$id.time)+2),rr)
temporal.data3$disease="Shared"

temporal.data<-rbind(temporal.data1,temporal.data2,temporal.data3)

ggplot(temporal.data, aes(x=time, y=rr, color =disease, shape=disease)) + scale_shape_manual(values=c(15,16,15))+
geom_line(size=1,linetype = "dotdash")+labs(color="Category",size=14, x="\nTime", y = "Relative risk\n") +scale_color_manual(values=c("orange", "green", "yellow"))+
scale_x_continuous(limits=c(NA,max(data.select()$id.time)),breaks=seq(min(data.select()$id.time),max(data.select()$id.time),1))+guides(shape=FALSE)+ theme_classic(base_size=12)
})

##Spatio-temporal risk
carto<-reactive ({
model.data<-data.frame(model.data())
model.data$rr <- joint.inla()$fit$summary.fitted.values[,"mean"] 
model.data$id<-model.data$id.area
dat1<-model.data[c(1:length(data.select()$id.time)),]  #Subsettting estimated values for disease 1
dat1$rr1<-dat1$rr
dat2<-model.data[c((((length(model.data$disease))/2)+1):(((length(model.data$disease))/2)+length(data.select()$id.time))),]  #Subsettting estimated values for disease 1
dat2$rr2<-dat2$rr
merge.data<-merge(dat1,dat2, by=c("id","id.time"))
carto <- merge(spatial.data()$sfpolygon,merge.data,by="id", all.x=TRUE)
carto
})
output$map4<- renderPlot({
paleta <- brewer.pal(10,"RdYlGn")[10:1]
values <- c(-Inf,0.05,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,Inf)
tm_shape(carto())+
  tm_polygons(col="rr1",
              palette=paleta,title="Estimated relative risk", legend.show=T, border.col="transparent",
              legend.reverse=T, style="fixed", breaks=values, interval.closure="left",showNA=TRUE,colorNA="white") +
  tm_layout(main.title= "Disease 1", main.title.position ="left", panel.label.size=1,
            legend.outside=T, legend.outside.position="right",legend.frame=F,
            legend.outside.size=0.25,
            panel.labels=as.character(round(seq(min(data.select()$id.time), max(data.select()$id.time),length.out=(length(unique(data.select()$id.time))))))) +tmap_options(check.and.fix = TRUE)+
  tm_facets(by="id.time",showNA=FALSE)
})

output$map5<- renderPlot({
paleta <- brewer.pal(10,"RdYlGn")[10:1]
values <- c(-Inf,0.05,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,Inf)

tm_shape(carto())+
  tm_polygons(col="rr2",
              palette=paleta,title="Estimated relative risk", legend.show=T, border.col="transparent",
              legend.reverse=T, style="fixed", breaks=values, interval.closure="left",showNA=TRUE,colorNA="white") +
  tm_layout(main.title= "Disease 2", main.title.position ="left", panel.label.size=1,
            legend.outside=T, legend.outside.position="right",legend.frame=F,
            legend.outside.size=0.25,
            panel.labels=as.character(round(seq(min(data.select()$id.time), max(data.select()$id.time),length.out=(length(unique(data.select()$id.time))))))) +tmap_options(check.and.fix = TRUE)+
  tm_facets(by="id.time",showNA=FALSE)
})


output$map6<- renderPlot({
model.data<-data.frame(model.data())

model.data$rr <- joint.inla()$fit$summary.fitted.values[,"mean"]   #Extracting all risk  values

krige.data<-model.data[model.data$disease=="disease1",c("id.area","id.time","rr")]
krige.data<-krige.data[krige.data$id.time<((max(krige.data$id.time))-1),]
krige.data1<-pivot_wider(krige.data,names_from="id.time",names_prefix="RR",values_from="rr")

krige.data2<-data.frame(long=coordinates(joint.inla()$shape)[,1],lat=coordinates(joint.inla()$shape)[,2],id.area=row.names(coordinates(joint.inla()$shape)))
krige.data3<-merge(krige.data1,krige.data2, by="id.area",all=TRUE)
coordinates(krige.data3)<-~long+lat
proj4string(krige.data3) <- CRS("+proj=longlat +datum=WGS84")

grid<- makegrid(joint.inla()$shape,n = 10000)    #Creating a prediction grid
grid <- SpatialPoints(grid, proj4string = CRS(proj4string(joint.inla()$shape)))
grid <- grid[joint.inla()$shape, ]
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
tm_shape(RRforecastsf) + tm_dots(col="RR",breaks=values,palette=paleta,legend.show=F) +
tm_layout(main.title= "Disease 1",panel.label.size=1,legend.outside.size=0.25)+tm_facets(by="id.time")+tm_add_legend(type="fill", 
col = paleta,border.alpha=0,labels = c("Less than 0.05","0.05 to 0.10","0.10 to 0.20","0.20 to 0.40","0.40 to 0.60","0.60 to 0.80","0.80 to 1.00","1.00 to 1.20","1.20 to 1.40","1.40 or more"),
reverse=T,title = "Predicted relative risk")

})

output$map7<- renderPlot({
model.data<-data.frame(model.data())

model.data$rr <- joint.inla()$fit$summary.fitted.values[,"mean"]  #Extracting all risk  values

krige.data<-model.data[model.data$disease=="disease2",c("id.area","id.time","rr")]  #Subsetting risk value data for disease 2
krige.data<-krige.data[krige.data$id.time<((max(krige.data$id.time))-1),]
krige.data1<-pivot_wider(krige.data,names_from="id.time",names_prefix="RR",values_from="rr")

krige.data2<-data.frame(long=coordinates(joint.inla()$shape)[,1],lat=coordinates(joint.inla()$shape)[,2],id.area=row.names(coordinates(joint.inla()$shape)))
krige.data3<-merge(krige.data1,krige.data2, by="id.area",all=TRUE)
coordinates(krige.data3)<-~long+lat
proj4string(krige.data3) <- CRS("+proj=longlat +datum=WGS84")

grid<- makegrid(joint.inla()$shape,n = 10000)
grid <- SpatialPoints(grid, proj4string = CRS(proj4string(joint.inla()$shape)))
grid <- grid[joint.inla()$shape, ]
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
tm_shape(RRforecastsf) + tm_dots(col="RR",breaks=values,palette=paleta,legend.show=F) +
tm_layout(main.title= "Disease 2",panel.label.size=1,legend.outside.size=0.25)+tm_facets(by="id.time")+tm_add_legend(type="fill", 
col = paleta,border.alpha=0,labels = c("Less than 0.05","0.05 to 0.10","0.10 to 0.20","0.20 to 0.40","0.40 to 0.60","0.60 to 0.80","0.80 to 1.00","1.00 to 1.20","1.20 to 1.40","1.40 or more"),
reverse=T,title = "Predicted relative risk")

})


output$map8<- renderPlot({
model.data<-data.frame(model.data())
model.data$rr <- joint.inla()$fit$summary.fitted.values[,"mean"] #Extracting all risk  values
model.data$id<-model.data$id.area
dpred<-model.data[c((length(data.select()$id.time)+1):(length(model.data()$id.time)/2)),]  #Subsettting predicted values for disease 1

dat1<-data.frame(dpred$rr,dpred$id,dpred$id.time)
dat1$id<-dat1$dpred.id
dat1$rr<-dat1$dpred.rr
dat1$id.time<-dat1$dpred.id.time

dpred<-model.data[c((length(model.data()$id.time)/2)+(length(data.select()$id.time)+1):length(model.data()$id.time)),]  #Subsettting predicted values for disease 2

dat2<-data.frame(dpred$rr,dpred$id,dpred$id.time)
dat2$id<-dat2$dpred.id
dat2$rr<-dat2$dpred.rr
dat2$id.time<-dat2$dpred.id.time

carto1 <- merge(spatial.data()$sfpolygon,dat1,by="id", all.x=TRUE)
carto2 <- merge(spatial.data()$sfpolygon,dat2,by="id", all.x=TRUE)
carto1$disease = "Disease 1"
carto2$disease = "Disease 2"
carto_total <- rbind(carto1, carto2)

paleta <- brewer.pal(10,"RdYlGn")[10:1]
values <- c(-Inf,0.05,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,Inf)
map<-tm_shape(carto_total)+
  tm_polygons(col="rr",
              palette=paleta,title="Estimated relative risk", legend.show=T, border.col="transparent",
              legend.reverse=T, style="fixed", breaks=values, midpoint = NA, interval.closure="left",showNA=TRUE,colorNA="white") +
  tm_layout(main.title= "", main.title.position =c("left","top"), panel.label.size=1,
            legend.outside=T,legend.outside.position="right",panel.label.rot=c(0,0),
            legend.outside.size=0.25)+tmap_options(check.and.fix = TRUE)+
  tm_facets(by=c("id.time","disease"),showNA=FALSE)
map
})

output$map9<- renderPlot({
model.data<-data.frame(model.data())
model.data$rr<- joint.inla()$fit$summary.fitted.values[,"mean"]   #Extracting all risk values

krige.data<-model.data[model.data$disease=="disease1",c("id.area","id.time","rr")]   #Subsetting data frame for disease 1
krige.data<-krige.data[krige.data$id.time>((max(krige.data$id.time))-2),]
krige.data1<-pivot_wider(krige.data,names_from="id.time",names_prefix="RR",values_from="rr")
krige.data2<-data.frame(long=coordinates(joint.inla()$shape)[,1],lat=coordinates(joint.inla()$shape)[,2],id.area=row.names(coordinates(joint.inla()$shape)))
krige.data3<-merge(krige.data1,krige.data2,by="id.area",all=TRUE)
coordinates(krige.data3)<-~long+lat
proj4string(krige.data3) <- CRS("+proj=longlat +datum=WGS84")

grid<- makegrid(joint.inla()$shape,n = 10000)
grid <- SpatialPoints(grid, proj4string = CRS(proj4string(joint.inla()$shape)))
grid <- grid[joint.inla()$shape, ]
gridded(grid)=TRUE

int<-list()
for (i in names(krige.data3[ ,grepl("RR",names(krige.data3))])) {
data<-krige.data3[!is.na(krige.data3[[i]]),]
formula<-as.formula(paste0(i, " ~ 1"))
 int[[i]] <- krige(formula,data,grid)
}

RRforecast1 <-int[[1]][,-c(1,2)]
for (i in names(krige.data3[ ,grepl("RR",names(krige.data3))])) {
RRforecast1[[i]]<-int[[i]][[1]]
}

RRforecast1<-as.data.frame(RRforecast1)

RRforecast1<- RRforecast1 %>% 
    pivot_longer(cols=starts_with("RR"),
    names_to = c(".value", "id.time"),
    names_pattern = "(.*?)(\\d+)" 
)
RRforecast1$disease<-"Disease 1"

krige.data<-model.data[model.data$disease=="disease2",c("id.area","id.time","rr")]  #Subseting data frame for disease 2
krige.data<-krige.data[krige.data$id.time>((max(krige.data$id.time))-2),]
krige.data1<-pivot_wider(krige.data,names_from="id.time",names_prefix="RR",values_from="rr")

krige.data3<-merge(krige.data1,krige.data2,by="id.area",all=TRUE)  #krige.data2 is based on code before disease 2
coordinates(krige.data3)<-~long+lat
proj4string(krige.data3)<- CRS("+proj=longlat +datum=WGS84")

int<-list()
for (i in names(krige.data3[ ,grepl("RR",names(krige.data3))])) {
data<-krige.data3[!is.na(krige.data3[[i]]),]
formula<-as.formula(paste0(i, " ~ 1"))
 int[[i]] <- krige(formula,data,grid)
}

RRforecast2 <-int[[1]][,-c(1,2)]
for (i in names(krige.data3[ ,grepl("RR",names(krige.data3))])) {
RRforecast2[[i]]<-int[[i]][[1]]
}

RRforecast2<-as.data.frame(RRforecast2)

RRforecast2<- RRforecast2 %>% 
    pivot_longer(cols=starts_with("RR"),
    names_to = c(".value", "id.time"),
    names_pattern = "(.*?)(\\d+)" 
)
RRforecast2$disease<-"Disease 2"

RRforecast<-rbind(RRforecast1,RRforecast2)

RRforecastsf <- st_as_sf(RRforecast,coords = c('x1', 'x2'), crs = 4326)

paleta <- brewer.pal(10,"RdYlGn")[10:1]
values <- c(-Inf,0.05,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,Inf)
tm_shape(RRforecastsf) + tm_dots(col="RR",breaks=values,palette=paleta,midpoint = NA,legend.show=F) +
tm_layout(main.title= "", panel.label.size=1,legend.outside.size=0.25,panel.label.rot=c(0,0))+tm_facets(by=c("id.time","disease"))+tm_add_legend(type="fill", 
col = paleta,border.alpha=0,labels = c("Less than 0.05","0.05 to 0.10","0.10 to 0.20","0.20 to 0.40","0.40 to 0.60","0.60 to 0.80","0.80 to 1.00","1.00 to 1.20","1.20 to 1.40","1.40 or more"),
reverse=T,title = "Predicted relative risk")
})


output$plot3<- renderPlot({
model.data<-data.frame(model.data())
model.data$rr <- joint.inla()$fit$summary.fitted.values[1:((length(model.data()$disease))/2),"mean"] #Subset data for disease 1
summaryRR1 <- model.data %>%
  group_by(id.time) %>%
  summarise(
    count = n(),
    meanRR = mean(rr,na.rm=TRUE),
    medianRR = median(rr,na.rm=TRUE),
    sdRR = sd(rr, na.rm=FALSE),
    seRR = sdRR/sqrt(count),
    ci95lower = meanRR - seRR*1.96,
    ci95upper = meanRR + seRR*1.96,
    disease="Disease 2"
   )

model.data$rr <- joint.inla()$fit$summary.fitted.values[(((length(model.data()$disease))/2)+1):(length(model.data()$disease)),"mean"] #Subset data for disease 2
summaryRR2 <- model.data %>%
  group_by(id.time) %>%
    summarise(
    count = n(),
    meanRR = mean(rr,na.rm=TRUE),
    medianRR = median(rr,na.rm=TRUE),
    sdRR = sd(rr, na.rm=FALSE),
    seRR = sdRR/sqrt(count),
    ci95lower = meanRR - seRR*1.96,
    ci95upper = meanRR + seRR*1.96,
    disease="Disease 1"
  )
summaryRR<-rbind(summaryRR1,summaryRR2)

ggplot(summaryRR, aes(x=id.time, y=meanRR, color=disease, shape=disease)) + 
  geom_line(size=1,linetype = "dotdash") +
  labs(color="Category",size=14, x="\nTime", y = "Relative risk\n") +
  scale_x_continuous(breaks=seq(min(model.data()$id.time), max(model.data()$id.time),1)) +
  scale_color_manual(values=c("yellow", "green"))+
  scale_shape_manual(values=c(15,16,15)) +
  guides(shape=FALSE) +geom_vline(xintercept=max(data.select()$id.time),size=1,colour="red",linetype = "dotdash")+theme_classic(base_size = 12)+coord_cartesian(ylim=c(0, NA))
})

##Correlation
output$corr <- renderPrint({
samples <- inla.posterior.sample(1000, joint.inla()$fit)
betas <- sapply(1:1000, function(x) (samples[[x]]$hyperpar)[10:15])
betas <- as.data.frame(t(betas))
rcorr(as.matrix(betas))
})


}

shinyApp(ui = ui, server = server)

