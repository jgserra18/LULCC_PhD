source('./Main/Global_functions.R')




linearly_intrapolate_share_MMS <- function(general_param='Distribution', param = 'Grazing') {
  # linearly interpolates general params related to animal distribution (housing, grazing) or manure storage (solid, slurry)
  
  file_df <- get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Animals', subfolderX2 = general_param, pattern = param)
  
  new_file <- file_df[, -c(1,2)]
  names(new_file) <- gsub('X','',names(new_file))
  
  store <- file_df[, c(1,2)]
  
  calc_cols <- paste0('X', seq(1987,2017))
  store[, calc_cols] <- sapply(calc_cols, function(x) store[,] <- NA)
  
  populate_cols <- c('X1990','X2014')
  store[, populate_cols] <- sapply(populate_cols, function(x) store[,x] <- file_df[,x])
  
  for (i in 1:nrow(new_file)) {
    
    calc_df <- data.frame(y = c(new_file[i,1], new_file[i,2]), x = c(1990,2014))
    lm_model <- lm(y~x, calc_df)
    lm_prediction <- round(
      predict(lm_model, newdata =  data.frame(x =  c(seq(1987,1989), seq(1991,2013), seq(2015,2017)))), 2)
    
    names(lm_prediction) <- paste0('X', c(seq(1987,1989), seq(1991,2013), seq(2015,2017)))
    lm_prediction <- ifelse(lm_prediction>1, 1, round(lm_prediction,2))
    lm_prediction <- ifelse(lm_prediction<0, 0, round(lm_prediction, 2))
    store[i, names(lm_prediction)] <- lm_prediction
  }
  
  return(store)
  rm(list = c('calc_df','lm_model','lm_prediction'))
}



## CORRECT GROSS MANURE ALLOCATION TO DIFFERENT PATHWAYS (GRAZING, YARD, HOUSING) ----------------

# when interpolating for the entire timeperiod, the sum of the difference fractions must be 1
# if not, the difference is allocated to housing

correct_share_MMS_pathway <- function(pathway, param) {
  # correct the years where the sum < 1
  # allocates what remains to housing
  
  graz <- linearly_intrapolate_share_MMS(general_param = 'Distribution', param = 'Grazing')
  yard <- linearly_intrapolate_share_MMS(general_param = 'Distribution', param = 'Yards')
  house <- linearly_intrapolate_share_MMS(general_param = 'Distribution', param = 'Housing')
  
  total <- house
  
  yrs <- paste0('X', seq(1987,2017))
  total[, yrs] <- graz[, yrs] + yard[, yrs] + house[, yrs]
  
  # ids where != 1
  dif_ids <- which(total<1, arr.ind = T)
  
  for (i in 1:nrow(dif_ids)) {
    
    r <- dif_ids[i,1]
    c <- dif_ids[i,2]
    
    house[r,c] <- house[r,c] + (1 - total[r,c])
  }
  
  if (pathway == 'Grazing') {
    
    graz <- subset(graz, Animals == param)
    return(graz)
    rm(list=c('yard','house','total'))
  }
  else if (pathway == 'Yards') {
    
    yard <- subset(yard, Animals == param)
    return(yard)
    rm(list=c('graz','house','total'))
  }
  else if (pathway == 'Housing') {
    
    house <- subset(house, Animals == param)
    return(house)
    rm(list=c('graz','yard','total'))
  }
}


