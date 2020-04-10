require('tmap')
require('ggplot2')
require('ggthemr')


plot_params_over_time <- function(module, param, main_var, var_name) {
  # param is either (Area, Yield)
  # main_var is the main category of a var (e.g., Cereals, Bovine)
  # var_name is the crop_name 
  
  if (param=='Areas') {
    title <- 'Areas (hectare)' 
  } else if (param=='Yield') {
    title <- expression(Yield~(kg~FM~ha^{-1}))
  }
  
  var_path <- get_mainfolder_sub(module = main_folder = 'Activity_data', pattern = 'Regional_crop')
  var_path <- file.path(var_path,param,main_var,paste0(var_name, '.csv'))
  r_file <- read.csv(var_path)
  names(r_file) <- gsub('X','', names(r_file))
  
  r_file <- reshape2::melt(r_file,'id')
  r_file$id <- as.factor(r_file$id)

  p <- ggplot(r_file, aes(x = variable, y = value, group=factor(id), color=id)) + 
    geom_line(lwd=1) + 
    theme_test() + 
    ylab(title) + 
    xlab('Years')
  
  return(p)
  rm(list=c('var_path', 'r_file','title'))
}



lulcc_plot <- function(stack_file) {
  # plots raster stacks 
  
  x <- stack_file
  
  p <- tm_shape(stack_file) + 
    tm_raster(breaks = x@categories, labels = x@labels, title = 'LU classes', 
              palette = 'Accent', n=15, stretch.palette = FALSE, legend.is.portrait = T) + 
    tm_facets(nrow=2, ncol=3, drop.units = TRUE, free.coords = TRUE) + 
    tm_layout(legend.outside = T, panel.labels = paste0('t = ', x@t))
  return(p)
}



ordered_model_plot <- function(ordered_model) {
  
  x <- clues_model@output
  lab <- clues_model@labels
  brk <- clues_model@categories
  t <- clues_model@time
  
  plot <- tm_shape(x) + 
    tm_raster(breaks = brk, labels = lab, title = 'LU classes', 
              palette = 'Accent', n=15, stretch.palette = FALSE, legend.is.portrait = T) + 
    tm_facets(drop.units = TRUE, free.coords = TRUE) + 
    tm_layout(legend.outside = T, panel.labels = seq(1990,2018))
  return(plot)
}
p <- ordered_model_plot(clues_model)
tmap_save(tm = p, './Historical_TVZ_LULC.pdf',dpi=600)


dmd
plot_LULCC_demand <- function(demand_df) {
  # plots land use demand
  
  #ggthemr_reset()
  colnames(demand_df) <- as.character(get_CLC_info('label'))
  rs_dmd <- reshape2::melt(demand_df)
  rs_dmd$Var1 <- rs_dmd$Var1-1
  
  ggplot(rs_dmd, aes(x=Var1, y=value, color=Var2, label=Var2)) + 
    geom_line(lwd=1, size = 1.5, alpha = 0.8) + scale_x_continuous(name = 'Time (years between 1990 and 2018)', breaks = c(0,5,10,15,20,25,28), limits=c(0,30)) + 
    ylab('LULCC cells at 500x500 m (No.)') + 
    theme_bw() + 
    theme(legend.position="bottom", legend.title = element_blank())
}
plot_LULCC_demand(dmd)
ggsave('./TNZ_LULC_demand.pdf', dpi=600)


