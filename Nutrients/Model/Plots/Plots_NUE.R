source('./Nutrients/Model/Nutrient_balance/5_Nutrient_use_efficiency.R')


# calls script from NUE
# plots scatterplot NUE_standard vs NUE_irrigation w/ NUE variation 
# plots the fraction of Nirrig vs total N inputs


# load and prepare the dataset ------------------------------------

irrig_nue = compute_irrigation_NUE()
dif_nue = irrig_nue$dif_NUE
irr_nue = irrig_nue$irrig_NUE
std_nue = irrig_nue$standard_NUE
new_tot_inp = irrig_nue$new_tot_input

std_nue = std_nue[, c('Muni_ID','ID','Muni',irrig_yrs)]




# scatterplot NUE --- 

scatter_df = reshape2::melt(std_nue, c('Muni_ID','ID','Muni'))
scatter_df = scatter_df[, c('Muni_ID','variable','value')]
names(scatter_df)[3] = 'Standard_NUE'

irr_nue = reshape2::melt(irr_nue, c('Muni_ID','ID','Muni'))[, c('Muni_ID','value')]
dif_nue = reshape2::melt(dif_nue, c('Muni_ID','ID','Muni'))[, c('Muni_ID','value')]
names(irr_nue)[2] = 'Irrigation_NUE'
names(dif_nue)[2] = 'NUE_variation'
scatter_df = cbind(scatter_df, irr_nue$Irrigation_NUE, dif_nue$NUE_variation)
names(scatter_df)[c(3,4,5)] = c('Standard_NUE','Irrigation_NUE','NUE_variation')

scatter_df$variable = gsub('X','',scatter_df$variable)
scatter_df$NUE_variation = as.numeric(scatter_df$NUE_variation)
scatter_df$brsk = cut(as.numeric(scatter_df$NUE_variation)*-1, c(0,1,5,10,25,50, +Inf), labels=c('<1','1-5','5-10','10-25','25-50','>50'))
scatter_df = na.omit(scatter_df)
#scatter_df$brsk = cut(as.numeric(scatter_df$NUE_variation)*-1, c(0,1,5,10,20,30,40,+Inf), labels=c('<1','1-5','5-10','10-20','20-30','30-40','>40'))
scatter_df$brsk = cut(as.numeric(scatter_df$NUE_variation)*-1, c(0,10,20,30,40,+Inf), labels=c('<10','10-20','20-30','30-40','>40'))
scatter_df$yrs = cut(as.numeric(scatter_df$variable),c(1995,2000,2005,2010,2015,2017), labels=c('1995-2000','2000-2005','2005-2010','2010-2015','2015-2017'))
quantile(scatter_df$NUE_variation)
mean(scatter_df$NUE_variation)
sd(scatter_df$NUE_variation)

require('ggExtra')

p1 = ggplot(scatter_df, aes(x=as.numeric(Irrigation_NUE),
                            y=as.numeric(Standard_NUE),
                            colour = brsk)) + 
  geom_point(size=2.5, alpha=0.85) + 
  scale_x_continuous(limits=c(0,505), breaks=c(0,50,100,200,300,400,500))+#,expand=c(0,0)) +
  scale_y_continuous(limits=c(0,525), breaks=c(0,50,100,200,300,400,500))+#, expand=c(0,0)) + 
  labs(colour='NUE reduction') +
  #scale_colour_brewer(palette = 'Blues', na.value=NA) + 
  scale_colour_viridis_d() + 
  geom_abline(slope=1, intercept=0, colour='black') + 
  xlab(expression(NUE[Irrig-Municipality]~'(%)')) + 
  ylab(expression(NUE[Standard-Municipality]~'(%)')) +
  guides(colour=guide_legend(nrow=1, 
                             title.position = 'top',
                             title.hjust = 0.5,
                             title =expression(NUE[Reduction]~'(%)'))) + 
  theme_classic() + 
  theme(legend.position = 'bottom', 
        legend.direction = 'horizontal',
        text = element_text(family='serif', size=14),
        axis.title = element_text(size=15.5),
        axis.text = element_text(size = 14),
        panel.grid.major = element_line(colour = 'black', size=0.1, linetype='dotted'),
        axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=1, linetype='solid'))
p1 = ggMarginal(p1, type='density', groupFill = TRUE, colour='black', alpha=0.8)

# FRAC_Nirrig --------



frac_nflow = reshape2::melt(new_tot_inp, c('Muni_ID','ID','Muni'))
Nirrig = read.csv('./Nutrients/Output/Irrigation/Irrigation_N/Total/Average_municipality/Avg_municipality_UAA.csv')
Nirrig = reshape2::melt(Nirrig, c('Muni_ID','ID','Muni'))
names(frac_nflow)[5] = 'Tot_Ninput'
names(Nirrig)[5] = 'Nirrig'
frac_nflow = cbind(frac_nflow, Nirrig$Nirrig)

colnames(frac_nflow)[c(5,6)] = c('Tot_Ninp','Nirrig')
frac_nflow$frac_nflow = round(frac_nflow$Nirrig/frac_nflow$Tot_Ninp*100, 2)
frac_nflow$variable = gsub('X','',frac_nflow$variable)

mean(frac_nflow$frac_nflow, na.rm=T)
sd(frac_nflow$frac_nflow, na.rm=T)
View(frac_nflow)
p2 = ggplot(frac_nflow, aes(x=variable, y=frac_nflow, fill=as.numeric(variable))) + 
  geom_boxplot(fill='darkorchid4',colour='azure4', width=0.3, size=1.1, outlier.size=2) + 
  #scale_colour_viridis_c(option = 'A') + 
  coord_flip() + 
  labs(colour='') +
  ylab(expression(f[Nirrig]~'(%)')) + xlab('Years') + 
  scale_y_continuous(limits=c(0,45), breaks=c(0,5,10,20,30,40)) + 
  theme_classic() + 
  theme(legend.position = 'none', 
        text = element_text(family='serif', size=14),
        axis.title = element_text(size=15.5),
        axis.text = element_text(size = 14),
        panel.grid.major = element_line(colour = 'black', size=0.1, linetype='dotted'),
        axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=1, linetype='solid')) 
p2


# NUE national vs municipalities -------------------

# call from Nirrig_Flows.R
require('dplyr')
require('ggplot2')

all_flows= compute_all_N_flows_mainland()
all_flows[which(all_flows$param=='N harvested'), 3] = 'N_harvested'
all_flows[which(all_flows$param=='Gross manure'), 3] = 'Gross_manure'
all_flows[which(all_flows$param=='Atmospheric deposition'), 3] = 'Atmospheric_deposition'
all_flows[which(all_flows$param=='Inorganic fertiliser'), 3] = 'Inorganic_fertiliser'

new_all = split(all_flows[,-3], all_flows$param, drop=F)
new_all <- do.call(cbind, new_all)
names(new_all) = gsub('.N_flow','',names(new_all))
names(new_all)[1] = 'yrs'

nue = new_all %>%
  group_by(yrs) %>%
  summarize(NUE_STD = round(N_harvested/(Atmospheric_deposition+BNF+Gross_manure + Inorganic_fertiliser)*100,2),
            NUE_irrig = round(N_harvested/(Atmospheric_deposition+Irrigation+BNF+Gross_manure + Inorganic_fertiliser)*100,2),
            NUE_dif = round(NUE_irrig-NUE_STD, 2))
cor.test(nue$yrs, nue$NUE_STD, method = 'spearman')
mean(nue$NUE_dif)
sd(nue$NUE_dif)

Wnue$brks=cut(nue$NUE_dif*-1, breaks=c(0,1,2,3,4), labels=c('< 1','1 - 2','2 - 3','3 - 4'))
View(nue)
p_national = ggplot(nue, aes(x=NUE_irrig, y=NUE_STD, colour=brks)) +
  geom_point(size=2.5, alpha=0.85) + 
  scale_x_continuous(limits=c(0,101), breaks=seq(0,100,10))+#,expand=c(0,0)) +
  scale_y_continuous(limits=c(0,101), breaks=seq(0,100,10))+#, expand=c(0,0)) + 
  labs(colour='NUE reduction') +
  xlab(expression(NUE[Irrig-National]~'(%)')) + 
  ylab(expression(NUE[Standard-National]~'(%)')) +
  #scale_colour_brewer(palette = 'Blues', na.value=NA) + 
  scale_colour_viridis_d() + 
  geom_abline(slope=1, intercept=0, colour='black') + 
  xlab(expression(NUE[Irrig-National]~'(%)')) + 
  ylab(expression(NUE[Standard-National]~'(%)')) +
  guides(colour=guide_legend(nrow=1, 
                             title.position = 'top',
                             title.hjust = 0.5,
                             title =expression(NUE[Reduction]~'(%)'))) + 
  theme_classic() + 
  theme(legend.position = 'bottom', 
        legend.direction = 'horizontal',
        text = element_text(family='serif', size=14),
        axis.title = element_text(size=14.5),
        axis.text = element_text(size = 14),
        panel.grid.major = element_line(colour = 'black', size=0.1, linetype='dotted'),
        axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=1, linetype='solid'))

p_national =ggMarginal(p_national, type='density', groupFill = TRUE, colour='black', alpha=0.8, )

# this plot
require('ggpubr')
all = ggarrange(p2, ncol=2, labels=c('A',''), font.label = list(family='serif'),
                ggarrange(p_national, p1,nrow=2, ncol=1, labels=c('B','C'), font.label = list(family='serif')))
all
ggsave(plot = all, filename = './Nutrients/Output/Plots/Irrigation/NUE/NUE_irrig_plot_v2.jpeg',dpi=600,
       height = 7.8, width = 8.9)
ggsave(plot = all, filename = './Nutrients/Output/Plots/Irrigation/NUE/NUE_irrig_plot_v2.tiff',dpi=600,
       height = 7.8, width = 8.9, compress='lzw')


frac_nflow = all_flows %>%
  filter(param != 'N_harvested') %>%
  group_by(yrs) %>%
  mutate (tot_Nin = sum(N_flow),
          frac_flow = N_flow/tot_Nin*100) %>%
  filter(param == 'Irrigation')
mean(frac_nflow$frac_flow)
sd(frac_nflow$frac_flow)
