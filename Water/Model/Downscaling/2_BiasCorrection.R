#devtools::install_github(c('SantanderMetGroup/drought4R@v0.1.0',
#                        'SantanderMetGroup/convertR@v01.2'), force = T)
#devtools::install_github('SantanderMetGroup/drought4R@v0.2.0', force = T)

source('./Water/Model/Downscaling/2.1_DownscaleR_modifier.R') # DownscaleR modified functions 


library(convertR)
library(drought4R)
library(magrittr)
library(downscaleR)

models = UDG.datasets()

lonLim = c(-9, -6)
latLim = c(35, 43)

grep('EU',models$CORDEX, value=T)
C4R.vocabulary()
#[1] "CORDEX-EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CLMcom-CCLM4-8-17_v1"   
#[2] "CORDEX-EUR-11_CNRM-CERFACS-CNRM-CM5_rcp45_r1i1p1_CLMcom-CCLM4-8-17_v1"        
#[3] "CORDEX-EUR-11_CNRM-CERFACS-CNRM-CM5_rcp85_r1i1p1_CLMcom-CCLM4-8-17_v1"  

tasmin.hist <- loadGridData(dataset = "CORDEX-EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CLMcom-CCLM4-8-17_v1",
                            var = "pr",
                            lonLim = lon,
                            latLim = lat,
                            years = set_timestep,
                           # time = "DD",
                            aggr.m = "sum") %>%  
  udConvertGrid(new.units = "mm") %>%
  interpGrid(getGrid(tmin_temp)) %>%
  gridArithmetics(ref_mask, operator="+")
plot_grid(tasmax.hist)
#tasmin.hist = gridArithmetics(tasmin.hist, ref_mask, operator="+") # ref_mask must have the same months/years of the input grid

tasmin.hist.corr <- mod_biasCorrection(x = tasmin.hist,
                                   y = tmin_hist,
                                   method = "scaling",
                                   scaling.type = "additive") %>% redim(drop = TRUE)


tasmax.hist <- loadGridData(dataset = "CORDEX-EUR-11_CNRM-CERFACS-CNRM-CM5_rcp45_r1i1p1_CLMcom-CCLM4-8-17_v1" ,
                            var = "tasmax",
                            lonLim = lon,
                            latLim = lat,
                            years = 2040:2050,
                            #time = "DD",
                            aggr.m = "mean") %>%
  udConvertGrid(new.units = "degC") %>% 
  interpGrid(getGrid(tmin_temp)) %>%
  gridArithmetics(ref_mask, operator="+")

tasmax.hist.corr <- mod_biasCorrection(tasmax.hist,
                                   y = tmax_hist,
                                   method = "scaling",
                                   scaling.type = "additive") %>% redim(drop = TRUE)

pr.hist <- loadGridData(dataset = "CORDEX-EUR44_EC-EARTH_r1i1p1_historical_RACMO22E_v1",
                        var = "pr",
                        lonLim = lonLim,
                        latLim = latLim,
                        years = 1971:2000,
                        time = "DD",
                        aggr.d = "sum",
                        aggr.m = "sum") %>% udConvertGrid(new.units = "degC") %>% interpGrid(new.coordinates = ref.grid)
pr.hist.corr <- biasCorrection(pr.hist,
                               y = pr.cru,
                               method = "scaling",
                               scaling.type = "multiplicative") %>% redim(drop = TRUE)

# future scenarios -----------

rcp85 <- "CORDEX-EUR44_EC-EARTH_r1i1p1_rcp85_RACMO22E_v1" 
tasmin.85 <- loadGridData(dataset = rcp85,
                          var = "tasmin",
                          lonLim = lonLim,
                          latLim = latLim,
                          years = 2010:2100,
                          time = "DD",
                          aggr.d = "mean",
                          aggr.m = "mean") %>% udConvertGrid(new.units = "degC") %>% interpGrid(new.coordinates = ref.grid)

tasmin.85.corr <- biasCorrection(y = tasmin.cru,
                                 x = tasmin.hist,
                                 newdata = tasmin.85,
                                 method = "scaling",
                                 scaling.type = "additive") %>% redim(drop = TRUE)

tasmax.85 <- loadGridData(dataset = rcp85,
                          var = "tasmax", 
                          lonLim = lonLim,
                          latLim = latLim,
                          years = 2010:2100,
                          time = "DD",
                          aggr.d = "mean",
                          aggr.m = "mean") %>% udConvertGrid(new.units = "degC") %>% interpGrid(new.coordinates = ref.grid)
tasmax.85.corr <- biasCorrection(y = tasmax.cru,
                                 x = tasmax.hist,
                                 newdata = tasmax.85,
                                 method = "scaling",
                                 scaling.type = "additive") %>% redim(drop = TRUE)

pr.85 <- loadGridData(dataset = rcp85,
                      var = "pr",
                      dictionary = dic, 
                      lonLim = lonLim,
                      latLim = latLim,
                      years = 2010:2100,
                      time = "DD",
                      aggr.d = "sum",
                      aggr.m = "sum") %>% interpGrid(new.coordinates = ref.grid)
pr.85.corr <- biasCorrection(y = pr.cru, x = pr.hist, newdata = pr.85, method = "scaling",
                             scaling.type = "multiplicative") %>% redim(drop = TRUE)

# join historical+future timelines ---------
tx <- bindGrid(tasmax.hist.corr, tasmax.85.corr, dimension = "time") %>% redim(drop = TRUE)
tn <- bindGrid(tasmin.hist.corr, tasmin.85.corr, dimension = "time") %>% redim(drop = TRUE)
pr <- bindGrid(pr.hist.corr, pr.85.corr, dimension = "time") %>% redim(drop = TRUE)


# compute PET hargreaves ------------
library(drought4R)
pet.har.85 <- petGrid(tasmin = tn, tasmax = tx, pr = pr, method = "hargreaves")
