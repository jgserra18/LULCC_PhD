source('./LULCC/Model/PreProcessing/Organize_ObsLulccRasterStack.R')


find_CLC_id_name = function(clc_id) {
  
  df = CLC_df()
  clc_id_name = df[which(df[,1]==clc_id), 2]
  return(clc_id_name)
}


disaggregate_LU_erosion_template = function() {
  # unit: t ha-1 yr-1
  
  ero = get_activity_data(module = 'Nutrients', folder = 'Environmental_params', subfolder = 'Erosion', pattern = 'Erosion')
 
  clc_06 = get_activity_data(module = 'LULCC', folder = 'CLC', subfolder = 'Native', pattern = '2006')
  rcl_clc = reclassify_CLC()
  clc_06 = reclassify(clc_06, rcl=rcl_clc[-4])
  
  clc_ids = unique(clc_06[[1]])
  
  for (id in clc_ids) {
    
    clc_lu = (clc_06==id) * ero
    clc_lu[clc_lu == 0] = NA
    
    lu_name = find_CLC_id_name(id)
    
    
    export_file(module = 'Nutrients', 
                file = clc_lu, 
                filename = lu_name, 
                folder = 'Runoff', 
                subfolder = 'Soil_erosion')
  }
  rm(list=c('ero','clc_06'))
}



compute_avg_LU_erosion_AR = function() {
  
  AR = get_activity_data(module = 'LULCC', folder = 'Admin', pattern = 'Agrarian_region')
  AR_df = data.frame(Admin_id=seq(1,7))
  
  df = CLC_df()
  
  for (i in 1:nrow(df)) {
    
    lu_name = df[i, 2]
    lu_erosion = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Runoff', subfolder = 'Soil_erosion', pattern = lu_name)
    
    print(lu_name)
    # loop AR
    for (j in 1:7) {
      
      sb_AR = subset(AR, Admin_id == j)
      
      lu_erosion_AR = mask(crop(lu_erosion, extent(sb_AR)), sb_AR)
      
      if (cellStats(lu_erosion_AR, 'sum')==0) {
        AR_df[j, lu_name] = 0
      }
      else {
        AR_df[j, lu_name] = exactextractr::exact_extract(lu_erosion_AR, sb_AR, 'mean')
      }
    }
  }
  export_file(module = 'Nutrients', folder = 'Activity_data', subfolder = 'General_params', subfolderX2 = 'LULCC', subfolderX3 = 'Erosion', file = AR_df, filename = 'AR_LU_erosion')
}


create_AR_LU_erosion_rate = function(lu_name, spatial_res = 500) {
  
  AR_LU_rates = get_activity_data(module = 'Nutrients', subfolder = 'General_params', subfolderX2 = 'LULCC', subfolderX3 = 'Erosion', pattern = 'AR_LU_erosion')
  AR_LU_rates = AR_LU_rates[, c('Admin_id', lu_name)]
  
  AR_LU_rates = fasterize_admin_statistical(module = 'LULCC', admin_res = 'Agrarian_region', file = AR_LU_rates, name_id = 'Admin_id', name_field = lu_name, spatial_res = spatial_res)
  return(AR_LU_rates)
}


LU_CNratio_AR_raster = function(clc_id) {
  
  df = data.frame(CLC_df(), CN = c(NA, NA, 14, 
                                   12, 12, 12,
                                   12, 12, 12,
                                   14, 12, 12,
                                   14,14,14, NA))
  cn_ratio = df[which(df[,1]==clc_id), 'CN']
  return(cn_ratio)
}


compute_LU_N_erosion = function(clc_id, annual_LULC) {
  # unit: kg N ha-1 yr-1
  
  lu_name = find_CLC_id_name(clc_id)
  
  lu_CN = LU_CNratio_AR_raster(clc_id) # 
  r_lu_rate = create_AR_LU_erosion_rate(lu_name, spatial_res = res(annual_LULC)) # erosion lu rate at the AT
  
  annual_LULC = annual_LULC == clc_id
  lu_N_rate = (r_lu_rate /  lu_CN ) * annual_LULC * 100
  
  return(lu_N_rate)
  rm(list=c('lu_name','lu_CN','r_lu_rate'))
}

d = raster('./LULCC/Output/LULC/PT/500/Mainland_glm_1990.tif')
dd = compute_LU_N_erosion(244, d)
dd[dd==0] = NA

