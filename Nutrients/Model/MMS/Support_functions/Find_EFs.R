source('./Main/Global_functions.R')


## SUPPORT FUNCTIONS TO HELP FIND THE MMS GASEOUS EMISSION FACTORS FOR A GIVEN ANIMAL

get_N_EFs <- function(N_gas, pathway) {
  
  
  if (N_gas == 'NH3') {
    
    EF <- get_activity_data(module = 'Nutrients', subfolder = 'Nutrient_params', subfolderX2 = 'N', subfolderX3 = 'MMS', subfolderX4 = 'EFs', pattern = 'NH3')
  }
  else {
    
    search_name <- paste0(N_gas, '_', pathway)
    EF <- get_activity_data(module = 'Nutrients', subfolder = 'Nutrient_params', subfolderX2 = 'N', subfolderX3 = 'MMS', subfolderX4 = 'EFs', pattern = search_name) 
  }
  return(EF)
}


find_animal_params_manure_type <- function(EF_df, param, pathway, manure_type) {
  
  # subset based on manure type
  EF <- subset(EF_df, Manure_type == manure_type)
  
  # find param row
  EF <- subset(EF, Animal == param)
  
  # get EF for the pathway
  EF <- EF[, pathway]
  
  return(EF)
}



select_animal_N_EFs <- function(N_gas, pathway, param, manure_type) {
  
  EF <- get_N_EFs(N_gas = N_gas, pathway = pathway)
  EF <- find_animal_params_manure_type(EF_df = EF, param = param, pathway = pathway, manure_type = manure_type)
  return(EF)
}


