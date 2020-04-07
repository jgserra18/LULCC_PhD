source('./Main/Global_functions.R')


## ----------------------- LIBRARIES --------------------- ##
## --------------------------------------------------------##

library(raster)
library(sf)
library(dplyr)


## ----------------------- DISTANCE TO ROADS --------------------- ##
## ----------------------------------------------------------------##

remove_muni_ids <- function() {
  # used in order to facilitate crash errors in the MRB roads computation
  
  muni <- get_activity_data(module = 'LULCC', mainfolder = 'Activity_data', folder = 'Admin', pattern = 'Municipality')
  ids <- muni$Admin_id
  
  roads_path <- get_folderpath(module = 'LULCC', main_folder = 'Output', folder = 'Exploratory_variables', subfolder = 'Roads')
  muni_ids <- gsub(pattern = 'Muni_', replacement = '', x = list.files(roads_path))
  muni_ids <- gsub('.tif', '', muni_ids)
  
  ids <- dplyr::setdiff(ids, muni_ids)
  
  return(ids)
  rm(list=c('muni', 'roads_path', 'muni_ids'))
}


MRB_roads <- function() {
  # creates a multiple ring buffer for roads for mainland Portugal
    
  roads <- get_activity_data(module = 'LULCC', mainfolder = 'Activity_data', folder = 'Physical_constraints', pattern = 'Roads_OSM.shp')
  muni <- get_activity_data(module = 'LULCC', mainfolder = 'Activity_data', folder = 'Admin', pattern = 'Municipality')
  
  id <- remove_muni_ids()

  for (i in id) {
    print(paste0('Working with municipality ID ', i))

    # subset municipalities and crop roads within 
    sb_muni <- subset(muni, Admin_id==i)
    sb_roads <- st_crop(roads, extent(sb_muni))

    # set MRB distance and create a list to store the MRB for a municipality
    dist <- c(100, 500, 1000, 2500, 5000, 8000, 10000)
    store_mrb <- vector('list', length(dist))
    
    ctr <- 0
    
    # create a MRB of roads for each municipality
    for (j in dist) {
      
      ctr <- ctr + 1
      
      mrb_roads <- st_buffer(sb_roads, dist=j)
      
      if (ctr==1) {
        store_mrb[[1]] <- mrb_roads
      }
      else {
        store_mrb[[ctr]] <- st_difference(mrb_roads, st_geometry(store_mrb[[ctr-1]]))
      }
      store_mrb[[ctr]]$idx <- j
    }
    
    #  join buffers
    join_mrb <- do.call(rbind, store_mrb)
    
    # rasterize the MRB for this municipality and store it in muni_store
    r <- raster(ext=extent(muni), crs=crs(muni), res=1000)
    r_mrb <- rasterize(x = join_mrb, y = r, field='idx', fun='first', background=-999)
    
    # export the rasterized MRB of a municipality
    export_file(module = 'LULCC', file = r_mrb, folder = 'Exploratory_variables', filename = paste0('Muni_', i), subfolder = 'Roads')
    rm(list=c('r', 'r_mrb', 'join_mrb'))
  }
}

d <- MRB_roads()







