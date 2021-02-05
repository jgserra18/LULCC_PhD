source('./Main/Global_functions.R')
source('./Main/General_GIS.R')





# IMPORTANT !!!!!!!!!!!!!!!!!!!!!!
find_spatial_admin <- function(file) {
  # this function receives a specific file as param
  # analyses the number of rows and identifies the administrative boundaries accordingly
  
  # NOTE: I HAVE YET TO IMPLEMENT NUTS2, NUTS3 AND AGRARIAN REGIONS
  
  ifelse(nrow(file)>350,
         admin <- 'Parish',
         admin <- 'Municipality')
  return(admin)
}


get_statistical_data <- function(year, pattern, subfolder) {
  # gets statistical data from Statistics Portugal
  # these are used as exploratory variables mainly for crop- and grassland
  # output: index 1 is the statistical dataset whereas index 2 is the spatial administrative boundary (e.g., Parish)
  
  
  stat_data <- get_activity_data(module = 'LULCC', 'Statistical_data', pattern, subfolder)
  names(stat_data) <- gsub('X', '', names(stat_data))
  # assess whether this is parish or not
  parish <- find_spatial_admin(stat_data)

  if (parish=='Parish') {
    stat_data <- stat_data[, c('Admin_name', 'Full_id', 'Admin_id', as.character(year))]
    
    stat_data$Admin_id <- as.character(stat_data$Admin_id)
    stat_data <- stat_data[nchar(stat_data$Full_id)==11, ]
    
    for (i in 1:nrow(stat_data)) {
      
      if (nchar(stat_data[i, 3])==5) {
        stat_data[i, 3] <- paste0('0', stat_data[i, 3])
      }
    }
  }
  else if (parish=='Municipality') {
    stat_data <- stat_data[, c('Admin_name', 'Admin_id', as.character(year))]
    
    stat_data[, 2] <- as.character(stat_data$Admin_id)
    stat_data <- stat_data[nchar(stat_data$Admin_id)==9, ]
    stat_data[, 2] <- substr(stat_data[, 2], nchar(stat_data$Admin_id)-3, nchar(stat_data$Admin_id))
  }
    return(list(stat_data, parish))
}



spatially_explicit_statistical_data <- function(year) {
  # converts to a spatial dataset the statistical data from Statistics Portugal
  # the reference spatial resolution is 100x100 m 
  # data is automatically exported to the output subfolders
  
  statistical_files <- gsub(pattern = '.csv',
                            replacement = '', 
                            x = list.files(get_folderpath(module = 'LULCC',main_folder = 'Activity_data', folder = 'Statistical_data')))
  
  for (i in statistical_files) {
    print(paste0('Working with ', i))
    
    stat_data <- get_statistical_data(year, i)
    r_stat <- stat_data[[1]]
    admin <- stat_data[[2]]
    
    r_stat <- fasterize_admin_statistical(module = 'LULCC', admin_res = as.character(admin), file = r_stat, name_id = 'Admin_id', 
                                          name_field = as.character(year), spatial_res = 100)
    r_stat <- resample_to_CLC(module = 'LULCC', r_stat, TRUE)
    export_file(module = 'LULCC', file = r_stat, folder = 'Exploratory_variables', filename = paste0(i, '_', year), subfolder = 'Statistical_data', subfolderX2 = 'Native')
  }
}

