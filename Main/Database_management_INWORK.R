require('RSQLite')


crop_areas = list.files(path = './Nutrients/Activity_data/Correct_data_Municipality/Areas/', recursive = T, full.names = T)
crop_names = list.files(path = './Nutrients/Activity_data/Correct_data_Municipality/Areas/', recursive = T, pattern = '.csv')
crop_names = gsub('/','_',crop_names)

set_local_db = function(dbpath) {
  
  myDB = dbpath
  myConn <- dbConnect(drv = SQLite(), dbname= myDB)
}


export_db = function(filename, file, dbpath='./Crop_areas.db') {
  
  myConn = set_local_db(dbpath)
  dbWriteTable(myConn, name =filename , value = file)
}

require('raster')
r = raster('./LULCC/Output/LULC/NUTS2/11/500/X11_glm_LULC_1990.tif')

lapply(1:length(crop_areas), function(x) export_db(crop_names[x], crop_areas[x]))
export_db('test',r)
dbListObjects(myConn)
dbReadTable(myConn, 'Potato_Rainfed_potato.csv')
