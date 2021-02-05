source('./LULCC/Model/LULC/LULCC_modelling.R')


require('ggplot2')
require('ggpubr')

plot_NUTS2_LULC_predictions()
plot_NUTS2_LULC_predictions = function() {
  nuts2 = c(15,16,11,17,18)
  sapply(nuts2, function(x) {
    
    param = set_LULCC_params('NUTS2',x, spatial_res = '500')
    glm_model  = compute_LULCC_models(param, 'glm')
    d = compute_LULCC_prediction(param, glm_model)
    
    correct_lu_cats = d[[2]]@labels
    correct_auc = d[[2]]@auc
    store_plots = list()
    
    for (i in seq_along(correct_auc)) {
      
      print(i)
      df = data.frame(
        x = d[[2]]@performance[[i]]@x.values,
        y = d[[2]]@performance[[i]]@y.values)
      names(df)=c('x','y')
      df$param = correct_lu_cats[i]
      
      store_plots[[i]] = ggplot(df,aes(x,y)) + 
        geom_line(colour='red1', size =1) + 
        geom_abline(slope = 1, intercept = 0, colour='black', size = 1) + 
        facet_wrap(.~param) + 
        geom_text(aes(x=0.8, y=0.1),
                  label=paste0('AUC = ', round(correct_auc[i], 2)), 
                  family='serif', size =3) +
        scale_x_continuous(breaks=c(0,.25,.5,.75,1)) + 
        theme_bw()  + 
        theme(
          text = element_text(family='serif', size=12),
          axis.text.x = element_text(size=12),
          axis.title = element_text(size=12),
          strip.text = element_text(size=12),
          axis.text = element_text(size = 12),
          panel.grid.major = element_line(colour = 'black', size=0.1, linetype='dotted'),
          axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=1, linetype='solid'))
    }
    
    all_plot = do.call(ggpubr::ggarrange, store_plots)
    all_plot = annotate_figure(all_plot, top = text_grob(paste0('NUTS2 ID ', x), face='bold',size=15))
    path = paste0('./LULCC/Output/Plots/LULC/')
    dir.create(path, recursive = T)
    ggsave(plot = all_plot, filename = paste0(path,'AUC_N2id_',x,'.tiff'), compress='lzw', height = 7, width = 9.2)
  })
}

