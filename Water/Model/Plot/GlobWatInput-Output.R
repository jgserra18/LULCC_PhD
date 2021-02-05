source('./Main/Global_functions.R')

perc = stack('./Water/Output/GlobWat/Annual/DeepPerc.grd')
rf = stack('./Water/Output/GlobWat/Annual/Runoff.grd')
prec = stack('./Water/Activity_data/Climatic_params/Annual/Prec.grd')

spres = res(rf)[1] * res(rf)[2] / 10000 # ha

df = data.frame(y=seq(1995,2017),
                rf = cellStats(rf,'sum') * 10 * spres / 1e9 , # km3
                perc = cellStats(perc,'sum')  * 10 * spres / 1e9,
                prec = cellStats(prec,'sum') * 10 * spres / 1e9) # km3
df = reshape2::melt(df, 'y')

require('ggplot2')

ggplot(df, aes(x=y, y=value, color=variable)) + 
  geom_line(size=2) + 
  scale_y_continuous(limits=c(0,130), breaks=c(0,10,25,50,100,125), expand=c(0,0)) + 
  scale_x_continuous(expand=c(0,0)) + 
  theme_classic() + 
  xlab('') + ylab(expression(GlobWat~IO~~(km^{3}~yr^{-1}))) + 
  theme(
    text = element_text(family='serif', size=14),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15),
    plot.margin = margin(t = 0.1, r = 2, b = 0.1, l = 0.3, "cm"),
    axis.text = element_text(size = 12),
    axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=1, linetype='solid'),
    axis.ticks.length=unit(.25, "cm"))



