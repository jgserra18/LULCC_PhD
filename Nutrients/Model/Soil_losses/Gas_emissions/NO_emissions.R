source('./Main/Global_functions.R')
source('./Nutrients/Model/Soil_losses/Gas_emissions/N2O_emissions_IPCC.R')


# using EMEP 2019 ------------------------
# uses the calculations for N2O but adapted to account for NOX emission factor

# set calculation method for the gas --- 
set_gas_Nloss = "NOx"


compute_total_direct_NOX_emissions = function(write = FALSE,
                                              manure_surplus_fills = FALSE, 
                                              manure_method = 'Method 1', 
                                              nutrient = 'N') {
  
  # computes the NOX emissions from all direct N sources (grazing, fertilisers)
  if (manure_surplus_fills == TRUE) { folder_div = 'With_ManSurplus'} else { folder_div = 'Without_ManSurplus'}
  yrs = paste0('X',seq(1987,2017))
  
  grazing = compute_grazing_N2O_emissions(write = write)
  biosolid = compute_other_Ninputs_N2O_emissions('Biosolid', write, manure_surplus_fills, manure_method)
  man = compute_other_Ninputs_N2O_emissions('Manure', write, manure_surplus_fills, manure_method)
  fert = compute_synthetic_fertiliser_N2O_emissions(write = write, manure_surplus_fills = manure_surplus_fills, manure_method = manure_method)
  
  tot_NOx = fert
  tot_NOx[, yrs] = sapply(yrs, function(x) fert[,x] + man[, x] +  biosolid[,x] + grazing[,x])
  
  
  if (write == TRUE) {
    export_file(module = 'Nutrients', 
                file = tot_NOx, 
                filename = 'Total', 
                folder = 'Gas_N_emissions', 
                subfolder = set_gas_Nloss, 
                subfolderX2 = 'Total',
                subfolderX3 = manure_method, 
                subfolderX4 = folder_div,
                subfolderX5 = 'Total')
  }
  
  return(tot_NOx)
  rm(list=c('grazing','biosolid','man','fert'))
}

