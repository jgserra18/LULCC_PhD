source('./Main/Global_functions.R')
source('./Main/Data_operations.R')



library('ggplot2')

man_p <- read.csv('./Nutrients/Output/BNF/N/Total/Total_sum.csv')

compute_values_by_mainland_UAA <- function(calc_df) {
  
  uaa <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Reference_areas', subfolder = 'UAA', pattern = 'UAA')
  uaa <- compute_mainland_total(uaa)
  
  calc_df <- compute_mainland_total(calc_df)
  
  yrs <- paste0('X',seq(1987,2017))
  calc_df[1, yrs] <- sapply(yrs, function(x) round(calc_df[1,x] / uaa[1,x], 2))
  
  return(calc_df)
  rm(list=c('uaa','yrs'))
}


plot_mainland_values <- function(calc_df) {
  
  calc_df <- compute_values_by_mainland_UAA(calc_df)
  
  calc_df <- reshape2::melt(calc_df)
  calc_df$yrs <- seq(1987,2017)
  
  limits_y <- c(0, max(calc_df$value))
  
  ggplot(calc_df, aes(x=yrs, y= value)) + 
    geom_line(color='red') + 
    theme_test() + 
    scale_y_continuous(limits = limits_y)
}

plot_mainland_values(man_p)
man_p
