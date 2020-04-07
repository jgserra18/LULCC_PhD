library(raster)
library(rgdal)
library(dplyr)
library(lulcc)
library(doParallel)
library(foreach)

setwd('C:\\Users\\serrm\\Desktop\\CLC\\')
rasterOptions(timer = T)

rcl <- read.csv('reclass.csv')[, -4]

n3 <- readOGR('Output/NUTS3.shp')
n3 <- spTransform(n3, proj4string(clc12))
n3 <- subset(n3, NUTS3=='DOURO')

nvz <- readOGR('Output/D311_ZonasProtegidas_ZVulneraveisPolNitratos.shp')
t <- subset(nvz, nome=='Tejo')
t <- spTransform(t, proj4string(clc12))

clc12 <- raster('Output/CLC_PT2012.tif')
clc12 <- mask(crop(clc12, t), t)
clc12 <- reclassify(clc12, rcl=rcl)


clc18 <- raster('Output/CLC_PT2018.tif')
clc18 <- mask(crop(clc18, t), t)
clc18 <- reclassify(clc18, rcl=rcl)

#ef
oc <- raster('Output/Lf_OC.tif')
oc <- mask(crop(oc, t), t)


roads <- raster('./Output/r_ev_roads_osm.tif')
roads <- mask(crop(roads, t), t)

d <- c(250, 500, 1000, 2000)

r_list <- lapply(d, FUN = function(x) {
                 buffer(roads, width=x)})

r_list$fun <- sum

rod <- do.call(mosaic, r_list)
rc_rod <- reclassify(rod, rcl=c(0, 1, 0.2, 1, 2, 0.5, 2, 3, 0.75, 3, 4, 1))

sl <- raster('Output/Rf_Slope.tif')
sl <- mask(crop(sl, t), t)

soil <- raster('Output/Lf_SoilType.tif')
soil <- mask(crop(soil, t), t)

wt <- raster('Output/water_table.tif')
wt <- mask(crop(wt, t), t)

ef_st <- list(oc, sl, soil, wt, rc_rod)
names(ef_st) <- c('ef1', 'ef2', 'ef3', 'ef4', 'ef5')

#ctr <- crosstab(clc12, clc18)
#trans_m <- asxmatrix(round(ct/rowSums(ct), 5))


lu <- ObsLulcRasterStack(x=stack(clc12, clc18),
                         categories=seq(1,4),
                         labels = c('urban', 'cropland', 'grassland', 'forest'),
                         t=c(0, 6))
crossTabulate(lu, times=c(0, 6))

ef <- ExpVarRasterList(x=ef_st, pattern='ef',)
part <- partition(x=lu[[1]], size=0.2, spatial=T)
train.data <- getPredictiveModelInputData(obs=lu, ef=ef, cells=part[['test']])
forms <- list(urban ~ ef2 + ef5,
              cropland ~ ef1+ef3+ef2+ef5,
              grassland ~ ef2,
              forest ~ ef1+ef2+ef3)
glm.models <- glmModels(formula=forms, family=binomial, data=train.data, obs=lu)              

test.data <- getPredictiveModelInputData(obs=lu, ef=ef, cells=part[["test"]])      

glm.pred <- PredictionList(models=glm.models, newdata=test.data)
glm.perf <- PerformanceList(pred=glm.pred, measure="rch")

#ROC analysis
plot(list(glm=glm.perf))

part <- rasterToPoints(x=lu[[1]], fun=function(x) x != 2, spatial=TRUE)
test.data <- getPredictiveModelInputData(obs=lu, ef=ef, cells=part, t=6)

glm.pred <- PredictionList(models=glm.models[[2]], newdata=test.data)
glm.perf <- PerformanceList(pred=glm.pred, measure="rch")
plot(list(glm=glm.perf))


dmd <- approxExtrapDemand(obs=lu, tout=0:6)

msk <- reclassify(clc12, rcl=c(0, 1, 0, 1, +Inf, 1))

matplot(dmd, type="l", ylab="Demand (no. of cells)", xlab="Time point",lty=1, col=c("Green","Red","Blue"))
legend("topleft", legend=lu@labels, col=c("Green","Red","Blue"), lty=1)

w <- matrix(data=1, nrow=3, ncol=3)
w3 <- matrix(data=.3, nrow=3, ncol=3)
w2 <- matrix(data=c(1,1,1, 1, 1, 1, 1,1,1), nrow=3, ncol=3)
nb <- NeighbRasterStack(x=lu[[1]], weights=w, categories=lu@categories)

#nice figure of merit
clues.rules <- matrix(data=c(1, 1, 1, 1, 
                             0, 1, .8, 1, 
                             0, 1, 1, 1, 
                             0, .5, .5, 1), nrow=4, ncol=4, byrow=TRUE)      

clues.rules <- matrix(data=c(1, 0, 0, 0, 
                             0, 1, .8, .2, 
                             0, 1, 1, .1, 
                             0, .5, .5, 1), nrow=4, ncol=4, byrow=TRUE)  

clues.parms <- list(jitter.f=0.0002,scale.f=0.000001,max.iter=1500,max.diff=2,ave.diff=2)
clues.model <- CluesModel(obs=lu, ef=ef,
                          models=glm.models,
                          neigh = nb,
                          time=0:6,
                          demand=dmd, 
                          elas=c(0.6, 0.5, .8, .8),
                          #elas=c(.4,.3,.3,1),
                          rules=clues.rules,
                          params=clues.parms)

clues.model <- allocate(clues.model)
plot(clues.model)
plot(clues.model@output$layer.2)
plot(clc18)

clues.tabs <- ThreeMapComparison(x=clues.model,factors=2^(1:8),timestep=6)
plot(clues.tabs)
plot(clues.tabs, category=2, factors=2^(1:8)[c(1,3,5,7)])


ordered.model <- OrderedModel(obs=lu,ef=ef,models=glm.models,time=0:6,demand=dmd,order=c(2,1,3,4))
ordered.model <- allocate(ordered.model, stochastic=TRUE)
ordered.tabs <- ThreeMapComparison(x=ordered.model,factors=2^(1:8),timestep=6)
plot(ordered.model@output$layer.2)



clues.agr <- AgreementBudget(x=clues.tabs)
plot(clues.agr)
clues.fom <- FigureOfMerit(x=clues.tabs)
p1 <- plot(clues.fom)
p1
