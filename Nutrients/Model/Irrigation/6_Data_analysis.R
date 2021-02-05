source('./Main/Global_functions.R')
source('./Main/Data_operations.R')


# IWW portugal ----------------------------------------------------
# from Plots_IWR.R
mainland_iww =  compute_mainland_IWR()

TIA = read.csv('./Nutrients/Activity_data/Correct_data_Municipality/Irrigation/Correct_irrigated_areas_method/Total/Total/Total.csv')
TIA = compute_mainland_total(TIA)[-1]
TIA = data.frame(yrs = seq(1995,2017),
                 tia = as.numeric(TIA[yrs]))
# temporal trend
cor.test(x=mainland_iww$yrs, y=mainland_iww$IWR, method='spearman')
cor.test(x=TIA$yrs, y=TIA$tia, method='spearman')

# change 1995-2017
1 - TIA[which(TIA$yrs=='2017'), 2] / TIA[which(TIA$yrs=='1995'), 2]
1 - mainland_iww[which(mainland_iww$yrs=='2017'), 2] / mainland_iww[which(mainland_iww$yrs=='1995'), 2]

# proportion main params in mainland iww
mainland_iww_mainParams = aggregate_mainland_IWR_categories()
calc_df = mainland_iww_mainParams %>%
  arrange(main_param) %>%
  group_by(yrs) %>%
  mutate(frac = IWR/sum(IWR)*100)  # compute annual proportions for each main param

calc_df = calc_df %>% 
  group_by(main_param) %>%
  summarize(avg= round(mean(frac), 2),
            sd= round(sd(frac), 2)) # annual avg


# avg IWW per municipality
avg_iww = compute_avg_IWR_rates_muni()
mean(avg_iww$Avg_IWR_rates) * 10
sd(avg_iww$Avg_IWR_rates) * 10


# Nirrig ---------------------------------------------------------
# Nirrig mainland
Nirrig_abs = read.csv('./Nutrients/Output/Irrigation/Irrigation_N/Total/Average_municipality/Avg_municipality_ABS.csv')
Nirrig_abs = compute_mainland_total(Nirrig_abs)[-1]
Nirrig_abs = data.frame(yrs = seq(1995,2017),
                 total = as.numeric(Nirrig_abs[yrs]))
cor.test(x=Nirrig_abs$yrs, y=Nirrig_abs$total, method = 'spearman')

# gw and sw fractions in nrrig
Nirrig_gw = read.csv('./Nutrients/Output/Irrigation/Irrigation_N/GW/Average_municipality/Avg_municipality.csv')
Nirrig_gw = compute_mainland_total(Nirrig_gw)[-1]
Nirrig_gw = data.frame(yrs = seq(1995,2017),
                        gw = as.numeric(Nirrig_gw[yrs]))

Nirrig_sw = read.csv('./Nutrients/Output/Irrigation/Irrigation_N/SW/Average_municipality/Avg_municipality.csv')
Nirrig_sw = compute_mainland_total(Nirrig_sw)[-1]
Nirrig_sw = data.frame(yrs = seq(1995,2017),
                       sw = as.numeric(Nirrig_sw[yrs]))

frac_src_nirrig = Nirrig_abs
frac_src_nirrig$tot_gw = Nirrig_gw$gw
frac_src_nirrig$tot_sw = Nirrig_sw$sw

frac_src_nirrig$frac_gw =  frac_src_nirrig$tot_gw/frac_src_nirrig$total
frac_src_nirrig$frac_sw =  frac_src_nirrig$tot_sw/frac_src_nirrig$total

mean(frac_src_nirrig$frac_gw)
sd(frac_src_nirrig$frac_gw)
mean(frac_src_nirrig$frac_sw)
sd(frac_src_nirrig$frac_sw)

# heuristic SA 
iwwGw_nirrig = read.csv('./Nutrients/Output/Irrigation/Irrigation_N/Total/Average_municipality/Heuristic_SA/Mod1.25_IWR_gw_municipality.csv')
iwwGw_nirrig = compute_mainland_total(iwwGw_nirrig)[-1]
iwwGw_nirrig = data.frame(yrs = seq(1995,2017),
                       iww = as.numeric(iwwGw_nirrig[yrs]))

iwwSw_nirrig = read.csv('./Nutrients/Output/Irrigation/Irrigation_N/Total/Average_municipality/Heuristic_SA/Mod1.25_IWR_sw_municipality.csv')
iwwSw_nirrig = compute_mainland_total(iwwSw_nirrig)[-1]
iwwSw_nirrig = data.frame(yrs = seq(1995,2017),
                          iww = as.numeric(iwwSw_nirrig[yrs]))

gw_nirrig = read.csv('./Nutrients/Output/Irrigation/Irrigation_N/Total/Average_municipality/Heuristic_SA/Mod1.25_no3_gw_municipality.csv')
gw_nirrig = compute_mainland_total(gw_nirrig)[-1]
gw_nirrig = data.frame(yrs = seq(1995,2017),
                        gw = as.numeric(gw_nirrig[yrs]))

sw_nirrig = read.csv('./Nutrients/Output/Irrigation/Irrigation_N/Total/Average_municipality/Heuristic_SA/Mod1.25_no3_sw_municipality.csv')
sw_nirrig = compute_mainland_total(sw_nirrig)[-1]
sw_nirrig = data.frame(yrs = seq(1995,2017),
                       sw = as.numeric(sw_nirrig[yrs]))

HSA_nirrig = Nirrig_abs
HSA_nirrig$iwwGw = iwwGw_nirrig$iww
HSA_nirrig$iwwSw = iwwSw_nirrig$iww
HSA_nirrig$gw = gw_nirrig$gw
HSA_nirrig$sw = sw_nirrig$sw

# calculate sensitivity in Nirrig of changing 25% parameters
HSA_nirrig$SA_iwwGW = HSA_nirrig$iwwGw/HSA_nirrig$total
HSA_nirrig$SA_iwwSW = HSA_nirrig$iwwSw/HSA_nirrig$total
HSA_nirrig$SA_gw = HSA_nirrig$gw/HSA_nirrig$total
HSA_nirrig$SA_sw = HSA_nirrig$sw/HSA_nirrig$total

col_name = 'SA_iwwGW'
give_stat = function(col_name) {
  
  print(paste0('Mean of ', col_name, ' is ', round(mean(HSA_nirrig[, col_name])*100-100, 2)))
  print(paste0('Sd of ', col_name, ' is ', round(sd(HSA_nirrig[, col_name])*100, 2)))
}
sapply(c('SA_iwwGW','SA_iwwSW','SA_gw','SA_sw'), give_stat)

# all parameters difference
all_nirrig = read.csv('./Nutrients/Output/Irrigation/Irrigation_N/Total/Average_municipality/Heuristic_SA/Mod1.25_All_municipality.csv')
all_nirrig = compute_mainland_total(all_nirrig)[-1]
all_nirrig = data.frame(yrs = seq(1995,2017),
                          all_irrig = as.numeric(all_nirrig[yrs]))

HSA_nirrig$all_nirrig = all_nirrig$all_irrig/HSA_nirrig$total
sd(HSA_nirrig$all_nirrig)


# N irrig vs TIA and UAA
Nirrig_abs = read.csv('./Nutrients/Output/Irrigation/Irrigation_N/Total/Average_municipality/Avg_municipality_ABS.csv')
Nirrig_abs = compute_mainland_total(Nirrig_abs)[-1]
Nirrig_abs = data.frame(yrs = seq(1995,2017),
                        total = as.numeric(Nirrig_abs[yrs]))

UAA = read.csv('./Nutrients/Output/Reference_areas/UAA/UAA.csv')
UAA = compute_mainland_total(UAA)[-1]
UAA = data.frame(yrs = seq(1995,2017),
                 uaa = as.numeric(UAA[yrs]))

TIA = read.csv('./Nutrients/Activity_data/Correct_data_Municipality/Irrigation/Correct_irrigated_areas_method/Total/Total/Total.csv')
TIA = compute_mainland_total(TIA)[-1]
TIA = data.frame(yrs = seq(1995,2017),
                 uaa = as.numeric(TIA[yrs]))

Nirrig_abs$perUAA = round(Nirrig_abs$total/UAA$uaa, 1)
Nirrig_abs$perTIA = round(Nirrig_abs$total/TIA$uaa, 1)


# local N irrig (per UAA)
Nirrig = read.csv('./Nutrients/Output/Irrigation/Irrigation_N/Total/Average_municipality/Avg_municipality_UAA.csv')
Nirrig = reshape2::melt(Nirrig, c('Muni_ID','ID','Muni'))
d = sapply(yrs, function(x) max(Nirrig[,x]))
View(d)


# NUE ------------
#plots_NUE.R

# call from Nirrig_Flows.R
all_flows= compute_all_N_flows_mainland()
View(all_flows)
tot_Nin = all_flows %>% 
  filter(param != 'N harvested')

# get frac N input for each input
frac_Nin = tot_Nin %>%
  group_by(yrs) %>%
  arrange(param) %>%
  mutate(sum = sum(N_flow),
         prop = round(N_flow/sum*100, 2)) 
# get frac n nput for irrigation
frac_Nirrig = frac_Nin %>%
  group_by(param) %>%
  summarise(mean=mean(prop),
            sd = sd(prop))

frac_NirrigFert = all_flows %>%
  filter(param == 'Irrigation' | param == 'Inorganic fertiliser')

df = data.frame(yrs=seq(1995,2017),
                frac = frac_NirrigFert[which(frac_NirrigFert$param=='Irrigation'),'N_flow']/frac_NirrigFert[which(frac_NirrigFert$param=='Inorganic fertiliser'),'N_flow']
)
cor.test(df$yrs,df$frac, method = 'spearman')

# nue trend national --
all_flows[which(all_flows$param=='N harvested'), 'N_flow'] = all_flows[which(all_flows$param=='N harvested'), 'N_flow'] * -1
View(all_flows)
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

# nue local
dif_nue = reshape2::melt(dif_nue, c('Muni_ID','ID','Muni'))
mean(dif_nue$value, na.rm=T)
sd(dif_nue$value, na.rm=T)



# IWW comparison with other sources -----------------
source('./Main/Data_operations.R')

tia =read.csv('./Nutrients/Activity_data/Correct_data_Municipality/Irrigation/Correct_irrigated_areas_method/Total/Total/Total.csv')
tia = general_sumIF_NUTS2(admin = 'NUTS2', tia)
tia = tia[, c('nuts2_ID','X2005','X2007','X2010','X2013')]
tia[6, c('X2005','X2007','X2010','X2013')] = colSums(tia[, c('X2005','X2007','X2010','X2013')])
tia[6, 1] = 'PT'
tia = reshape2::melt(tia, c('nuts2_ID'))
tia = tia %>% arrange(nuts2_ID)

tia_euro = read.csv('./Nutrients/Activity_data/General_params/Irrigation/Comparison_source/Eurostat_TIA.csv')
tia_euro = reshape2::melt(tia_euro, c('nuts2_ID'))
tia_euro = tia_euro %>% arrange(nuts2_ID)
names(tia_euro)[3] = 'eurostat'
tia_euro$modelled = tia$value

ggplot(tia_euro, aes(x=eurostat/1e3, y=modelled/1e3)) + 
  geom_point(aes(color=nuts2_ID),size=3) + 
  geom_abline(slope = 1, intercept = 0, colour='black') + 
  geom_smooth(method='lm',colour='red1', se=F) + 
  scale_y_continuous(trans='pseudo_log', limits=c(0,500), expand=c(0,0)) + 
  scale_x_continuous(trans='pseudo_log', limits=c(0,500), expand=c(0,0)) + 
  xlab('Eurostat (1000 ha)') + ylab('Modelled (1000 ha)') + 
  theme(
    text = element_text(family='serif', size=14),
    axis.title = element_text(size=15.5, face = 'bold'),
    axis.text = element_text(size = 13),
    panel.grid.major  = element_line(colour = 'black', size=0.1, linetype='dotted'),
    axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=1, linetype='solid'))

summary(lm(modelled~eurostat, tia_euro))$r.squared



iww =read.csv('./Nutrients/Output/Irrigation/Irrigation_requirements/GlobWat/Total/Total/Total.csv')
iww = compute_temporal_sumIF_admin_df(admin = 'NUTS2', iww)
iww = iww[, c('Admin_id','X2010')]
iww[6, c('X2010')] = sum(iww$X2010)
iww[6, 1] = 'PT'
iww$eurostat = c(797722890, 161272460, 868334330, 179464350, 1405506770, 3437365770)
iww$dif = 1-iww$X2010/iww$eurostat
View(iww)
ggplot(iww, aes(x=eurostat,y=X2010)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_smooth(method='lm')
