
#Loading required packages
#options(repos=c("INLA"="https://inla.r-inla-download.org/R/stable","CRAN" = "https://cloud.r-project.org/"))
#options(repos=c("tmap"="https://r-tmap.r-universe.dev","CRAN" = "https://cloud.r-project.org/"))

library("shiny")
library("INLA")
library("ggplot2")
library("gridExtra")
library("RColorBrewer")
library("cleangeo")
library("Hmisc") # rcorr()
library("rmapshaper")
library("shinyjs") # ui to appear
library("dplyr") # summarize ()
library("spdep") # poly2nb
library("raster")

#Setting upload size for shapefiles

options(shiny.maxRequestSize=40*1024^2)


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
        tabPanel("Explore", plotOutput("map1"),plotOutput("plot1")),
        tabPanel("Model estimation", verbatimTextOutput("summary")),
        tabPanel("Spatial and temporal risk", plotOutput("map2",height="250px"),plotOutput("plot2",height="250px")),
        tabPanel("Estimated spatio-temporal risk",plotOutput("map3",height="300px"),plotOutput("map4",height="300px")),
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
    select <- data.frame(data.upload()[[input$id.area]], data.upload()[[input$id.time]],data.upload()[[input$observed.1]], data.upload()[[input$observed.2]],
    data.upload()[[input$population.1]], data.upload()[[input$population.2]])
    colnames(select) <- c("id.area","id.time","observed.1","observed.2","population.1","population.2")
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
summary.data <- data.select() %>%
  group_by(id.area) %>%
  summarise(
    count = n(),
    sumobserved.1 = sum(observed.1,na.rm=TRUE),
    sumobserved.2 = sum(observed.2,na.rm=TRUE),
    population.1=sum(population.1,na.rm=TRUE),
    population.2=sum(population.2,na.rm=TRUE)
      )
poissonmodel1 <- glm(sumobserved.1 ~ population.1, family = poisson, data = summary.data)
summary.data$e1<-poissonmodel1$fitted.values
poissonmodel2 <- glm(sumobserved.2 ~ population.2, family = poisson, data = summary.data)
summary.data$e2<-poissonmodel2$fitted.values
summary.data$ir<-summary.data$sumobserved.1 /summary.data$e1
summary.data$id<-summary.data$id.area

#Map data
shape<-ms_filter_islands(map(), min_area = 12391399903) #Removing small islands
mapdata <- fortify(shape)
sfpolygon <- sfheaders::sf_polygon(
  obj = mapdata
  , x = "long"
  , y = "lat"
  , polygon_id = "id"
)
carto1 <- merge(sfpolygon,summary.data, by="id")
summary.data$ir<-summary.data$sumobserved.2/summary.data$e2
carto2 <- merge(sfpolygon,summary.data, by="id")
carto1$disease = "Disease 1"
carto2$disease = "Disease 2"
carto.total <- rbind(carto1, carto2)
carto.total <- clgeo_Clean(carto.total)

paleta <- brewer.pal(6,"RdYlGn")[6:1]
values <- c(0.0,0.3,0.6,0.9,1,1.3,Inf)

tm_shape(sfpolygon)+tm_polygons()+tm_shape(carto.total)+
  tm_polygons(col="ir",
              palette=paleta,title="Incidence ratio", legend.show=T, border.col="transparent",
              legend.reverse=T, style="fixed", breaks=values, midpoint = NA, interval.closure="left",showNA=TRUE,colorNA="grey") +
  tm_layout(main.title= "", main.title.position =0.17, panel.label.size=1,
            legend.outside=T,legend.outside.position="right",
            legend.outside.size=0.25)+tmap_options(check.and.fix = TRUE)+
  tm_facets(by="disease",showNA=FALSE)
})

output$plot1 <- renderPlot({
summary.data <- data.select() %>%
                group_by(id.time) %>%
                summarise(
                count = n(),
                sumobserved.1 = sum(observed.1,na.rm=TRUE),
                sumobserved.2 = sum(observed.2,na.rm=TRUE),
                )
plot1<-ggplot(summary.data, aes(x=id.time, y=sumobserved.1))+ geom_line(size=1,linetype = "dashed") +geom_point()+geom_text(aes(label = round(sumobserved.1, 1)),vjust=-0.5)+
       ylim(min(summary.data$sumobserved.1)-0.1*min(summary.data$sumobserved.1), max(summary.data$sumobserved.1)+0.1*max(summary.data$sumobserved.1))+ 
       labs(size=14, x="\nYear", y = "Number of cases\n")+scale_x_continuous(breaks=seq(min(data.select()$id.time),max(data.select()$id.time),1))+ggtitle("Disease 1")+
       theme(plot.title = element_text(hjust = 0.5))+theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))+theme_bw()

plot2<-ggplot(summary.data, aes(x=id.time, y=sumobserved.2)) +ylim(min(summary.data$sumobserved.2)-0.1*min(summary.data$sumobserved.2), max(summary.data$sumobserved.2)+0.1*max(summary.data$sumobserved.2))+
       geom_line(size=1,linetype = "dashed")+labs(size=14, x="\nYear", y = "Number of cases\n") +geom_point()+geom_text(aes(label = round(sumobserved.2, 1)),vjust=-0.5)+
       scale_x_continuous(breaks=seq(min(data.select()$id.time),max(data.select()$id.time),1))+ggtitle("Disease 2")+theme(plot.title = element_text(hjust = 0.5))+theme(axis.text=element_text(size=14),
       axis.title=element_text(size=14,face="bold")) +theme_bw()

grid.arrange(plot1,plot2,nrow=2,ncol=1)

})

## Model data
model.data <- reactive({
select.data<-data.frame(data.select())
poissonmodel1 <- glm(observed.1 ~ population.1, family = poisson, data = select.data)
select.data$e1 <- poissonmodel1$fitted.values
poissonmodel2 <- glm(observed.2 ~ population.2, family = poisson, data = select.data)
select.data$e2 <- poissonmodel2$fitted.values
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
shape<-ms_filter_islands(map(), min_area = 12391399903) #Removing small islands
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
mapdata <- fortify(joint.inla()$shape)
n<-length(unique(mapdata$id))
da1<-data.frame(id=1:n, rr=matrix(exp(joint.inla()$fit$summary.random$sp.idx1[,"mean"]),nrow=n,ncol=1))
da1$prob<-Prob1

da2<-data.frame(id=1:n, rr=matrix(exp(joint.inla()$fit$summary.random$sp.idx2[,"mean"]),nrow=n,ncol=1))
da2$prob<-Prob2

da3<-data.frame(id=1:n, rr=matrix(exp(joint.inla()$fit$summary.random$sp.dum[,"mean"]),nrow=n,ncol=1))
da3$prob<-Prob3

sfpolygon <- sfheaders::sf_polygon(
  obj = mapdata
  , x = "long"
  , y = "lat"
  , polygon_id = "id"
)

carto1 <- merge(sfpolygon,da1, by="id")
carto2 <- merge(sfpolygon,da2, by="id")
carto3 <- merge(sfpolygon,da3, by="id")
carto1$disease = "Disease 1"
carto2$disease = "Disease 2"
carto3$disease = "Shared"
carto.total <- rbind(carto1, carto2, carto3)
carto.total <- cleangeo::clgeo_Clean(carto.total)
carto.total
})

output$map2 <- renderPlot({
paleta <- brewer.pal(6,"RdYlGn")[6:1]
values <- c(0.0,0.3,0.6,0.9,1,1.3,Inf)
tm_shape(spatial.data())+
  tm_polygons(col="rr",
              palette=paleta,title="Relative risk", legend.show=T, border.col="transparent",
              legend.reverse=T, style="fixed", breaks=values, midpoint = NA, interval.closure="left") +
  tm_layout(main.title= "",panel.label.size=1,
            legend.outside=T,legend.outside.position="right",
            legend.outside.size=0.25)+tmap_options(check.and.fix = TRUE)+
  tm_facets(by="disease")
})

output$plot2 <- renderPlot({
rr<-exp(joint.inla()$fit$summary.random$tm.idx1[,"mean"])
temp1<-data.frame(year=min(data.select()$id.time):max(data.select()$id.time),rr)
temp1$disease="Disease 1"

rr<-exp(joint.inla()$fit$summary.random$tm.idx2[,"mean"])
temp2<-data.frame(year=min(data.select()$id.time):max(data.select()$id.time),rr)
temp2$disease="Disease 2"

rr<-exp(joint.inla()$fit$summary.random$tm.dum[,"mean"])
temp3<-data.frame(year=min(data.select()$id.time):max(data.select()$id.time),rr)
temp3$disease="Shared"

temp<-rbind(temp1,temp2,temp3)

ggplot(temp, aes(x=year, y=rr, color =disease, shape=disease)) + scale_shape_manual(values=c(15,16,15))+geom_line(size=1.5,linetype = "dotdash")+
labs(color="Category",x="\nYear", y = "Relative risk\n") +scale_color_manual(values=c("yellow", "green","orange"))+
scale_x_continuous(breaks=seq(min(temp$year),max(temp$year),1))+guides(shape=FALSE)+theme_bw()
})

#Spatio-temporal risk
carto<-reactive ({
model.data<-data.frame(model.data())
model.data$rr1 <- joint.inla()$fit$summary.fitted.values[1:((length(model.data$disease))/2),"mean"] #Change 1:150 BTB & 151: 300 EPTB
model.data$rr2 <- joint.inla()$fit$summary.fitted.values[(((length(model.data$disease))/2)+1):length(model.data$disease),"mean"] #Change 1:150 BTB & 151: 300 EPTB
model.data$id<-model.data$id.area
dat<-data.frame(model.data$rr1,model.data$rr2,model.data$id,model.data$id.time)
dat$id<-dat$model.data.id
dat$rr1<-dat$model.data.rr1
dat$rr2<-dat$model.data.rr2
dat$id.time<-dat$model.data.id.time
mapdata <- fortify(joint.inla()$shape)
sfpolygon <- sfheaders::sf_polygon(
  obj = mapdata
  , x = "long"
  , y = "lat"
  , polygon_id = "id"
)
carto <- merge(sfpolygon,dat,by="id", all.x=TRUE)

return(
    list(
      sfpolygon = sfpolygon,
      carto = carto
    )
  )

})
output$map3<- renderPlot({
paleta <- brewer.pal(10,"RdYlGn")[10:1]
values <- c(-Inf,0.05,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,Inf)
tm_shape(carto()$sfpolygon)+tm_polygons()+tm_shape(carto()$carto)+
  tm_polygons(col="rr1",
              palette=paleta,title="Relative risk", legend.show=T, border.col="transparent",
              legend.reverse=T, style="fixed", breaks=values, interval.closure="left",showNA=TRUE,colorNA="grey") +
  tm_layout(main.title= "Disease 1", main.title.position ="left", panel.label.size=1,
            legend.outside=T, legend.outside.position="right",legend.frame=F,
            legend.outside.size=0.2,
            panel.labels=as.character(round(seq(min(model.data()$id.time), max(model.data()$id.time),length.out=length(unique(model.data()$id.time)))))) +tmap_options(check.and.fix = TRUE)+
  tm_facets(by="id.time",free.scales=TRUE,showNA=FALSE)
})

output$map4<- renderPlot({
paleta <- brewer.pal(10,"RdYlGn")[10:1]
values <- c(-Inf,0.05,0.1,0.2,0.4,0.6,0.8,1,1.2,1.4,Inf)

tm_shape(carto()$sfpolygon)+tm_polygons()+tm_shape(carto()$carto)+
  tm_polygons(col="rr2",
              palette=paleta,title="Relative risk", legend.show=T, border.col="transparent",
              legend.reverse=T, style="fixed", breaks=values, interval.closure="left",showNA=TRUE,colorNA="grey") +
  tm_layout(main.title= "Disease 2", main.title.position ="left", panel.label.size=1,
            legend.outside=T, legend.outside.position="right",legend.frame=F,
            legend.outside.size=0.2,
            panel.labels=as.character(round(seq(min(model.data()$id.time), max(model.data()$id.time),length.out=length(unique(model.data()$id.time)))))) +tmap_options(check.and.fix = TRUE)+
  tm_facets(by="id.time",free.scales=TRUE,showNA=FALSE)
})

#Correlation
output$corr <- renderPrint({
samples <- inla.posterior.sample(1000, joint.inla()$fit)
betas <- sapply(1:1000, function(x) (samples[[x]]$hyperpar)[10:15])
betas <- as.data.frame(t(betas))
rcorr(as.matrix(betas))
})


}

shinyApp(ui = ui, server = server)