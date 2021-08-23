# impact of pc expansion on mortality in brazil
# sanjay_basu@hms.harvard.edu
library(tidyverse)
library(readr)
setwd("~/Box/Research/Research projects/Primary care Brazil")
df <- read_csv("SanjayDataset_SIM_ATLAS_ESF.csv")

dfcit = df %>%
  filter(year==2018) %>%
  group_by(CountyName) %>%
  summarise(pop=mean(Population_Ref,na.rm = T),
            coverage=mean(PHC_Coverage,na.rm = T)/100) 


dfrates = df %>%
  filter(year==2010) 

dfrates$dzgroup = 1*(dfrates$ICD_10codes=="A00 - A09" | 
                       dfrates$ICD_10codes=="A20 - A28" |
                       dfrates$ICD_10codes=="A30 - A49" |
                       dfrates$ICD_10codes=="A65 - A69" |
                       dfrates$ICD_10codes=="A70 - A74" |
                       dfrates$ICD_10codes=="A75 - A79" |
                       dfrates$ICD_10codes=="A80 - A89" |
                       dfrates$ICD_10codes=="A90 - A99" |
                       dfrates$ICD_10codes=="B00 - B09" |
                       dfrates$ICD_10codes=="B15 - B19" |
                       dfrates$ICD_10codes=="B25 - B34" |
                       dfrates$ICD_10codes=="B35 - B49" |
                       dfrates$ICD_10codes=="B35 - B49" |
                       dfrates$ICD_10codes=="B50 - B64" |
                       dfrates$ICD_10codes=="B65 - B83" |
                       dfrates$ICD_10codes=="B99 - B99" |
                       dfrates$ICD_10codes=="G00 - G09" |
                       dfrates$ICD_10codes=="N70 - N77" |
                       dfrates$ICD_10codes=="P35 - P39") +
  2*(dfrates$ICD_10codes=="B20 - B24") +
  3*(dfrates$ICD_10codes=="A15 - A29" |
       dfrates$ICD_10codes=="A20 - A28" |
       dfrates$ICD_10codes=="B50 - B64" |
       dfrates$ICD_10codes=="B65 - B83") +
  4*(dfrates$ICD_10codes=="H65 - H75" |
       dfrates$ICD_10codes=="J00 - J06" | 
       dfrates$ICD_10codes=="J09 - J18" |
       dfrates$ICD_10codes=="J20 - J22" |
       dfrates$ICD_10codes=="J30 - J39" |
       dfrates$ICD_10codes=="J40 - J47" |
       dfrates$ICD_10codes=="J60 - J70" |
       dfrates$ICD_10codes=="J80 - J84" |
       dfrates$ICD_10codes=="J85 - J86" |
       dfrates$ICD_10codes=="J90 - J94" |
       dfrates$ICD_10codes=="J95 - J99")  +
  5*(dfrates$ICD_10codes=="D50 - D53"|
       dfrates$ICD_10codes=="D60 - D64"|
       dfrates$ICD_10codes=="E00 - E07" |
       dfrates$ICD_10codes=="E40 - E46" |
       dfrates$ICD_10codes=="E50 - E64") +
  6*(dfrates$ICD_10codes=="C00 - C97" |
       dfrates$ICD_10codes=="D00 - D09" |
       dfrates$ICD_10codes=="D10 - D36" |
       dfrates$ICD_10codes=="D37 - D48") +
  7*(dfrates$ICD_10codes=="G10 - G13" |
       dfrates$ICD_10codes=="G20 - G26" |
       dfrates$ICD_10codes=="G30 - G32" |
       dfrates$ICD_10codes=="G35 - G37" |
       dfrates$ICD_10codes=="G40 - G47" |
       dfrates$ICD_10codes=="G60 - G64" |
       dfrates$ICD_10codes=="G70 - G73" |
       dfrates$ICD_10codes=="G90 - G99" |
       dfrates$ICD_10codes=="M30 - M36") +
  8*(dfrates$ICD_10codes=="D55 - D59" |
       dfrates$ICD_10codes=="D60 - D64" |
       dfrates$ICD_10codes=="D70 - D77" |
       dfrates$ICD_10codes=="D80 - D89" |
       dfrates$ICD_10codes=="E00 - E07" |
       dfrates$ICD_10codes=="E10 - E14" |
       dfrates$ICD_10codes=="E15 - E16" | 
       dfrates$ICD_10codes=="E20 - E35" |
       dfrates$ICD_10codes=="E65 - E68" |
       dfrates$ICD_10codes=="E70 - E90") +
  9*(dfrates$ICD_10codes=="F10 - F19" |
       dfrates$ICD_10codes=="F30 - F39" |
       dfrates$ICD_10codes=="F50 - F59" |
       dfrates$ICD_10codes=="F60 - F69" |
       dfrates$ICD_10codes=="F70 - F79" |
       dfrates$ICD_10codes=="F80 - F89" |
       dfrates$ICD_10codes=="F90 - F98" ) +
  10*(dfrates$ICD_10codes=="I60 - I69")+
  11*(dfrates$ICD_10codes=="I20 - I25" |
        dfrates$ICD_10codes=="I26 - I28" |
        dfrates$ICD_10codes=="I30 - I52") +
  12*(dfrates$ICD_10codes=="I00 - I02" |
        dfrates$ICD_10codes=="I05 - I09" |
        dfrates$ICD_10codes=="I10 - I15" |
        dfrates$ICD_10codes=="I95 - I99") +
  13*(dfrates$ICD_10codes=="K35 - K38" |
        dfrates$ICD_10codes=="K40 - K46" |
        dfrates$ICD_10codes=="K50 - K52" |
        dfrates$ICD_10codes=="K55 - K63" |
        dfrates$ICD_10codes=="K65 - K67" |
        dfrates$ICD_10codes=="K70 - K77" |
        dfrates$ICD_10codes=="K90 - K93") +
  14*(dfrates$ICD_10codes=="N00 - N08" |
        dfrates$ICD_10codes=="N10 - N16" |
        dfrates$ICD_10codes=="N17 - N19" |
        dfrates$ICD_10codes=="N20 - N23" |
        dfrates$ICD_10codes=="N25 - N29" |
        dfrates$ICD_10codes=="N30 - N39" |
        dfrates$ICD_10codes=="N40 - N51" |
        dfrates$ICD_10codes=="N60 - N64" |
        dfrates$ICD_10codes=="N70 - N77" |
        dfrates$ICD_10codes=="N80 - N98") +
  15*(dfrates$ICD_10codes=="V01 - V99" |
        dfrates$ICD_10codes=="X00 - X59" | 
        dfrates$ICD_10codes=="Y40 - Y84" |
        dfrates$ICD_10codes=="Y85 - Y89") +
  16*(dfrates$ICD_10codes=="X60 - X84" |
        dfrates$ICD_10codes=="X85 - X99 Y00-Y09" |
        dfrates$ICD_10codes=="Y35 - Y36") +
  17*(dfrates$ICD_10codes=="O00 - O08" |
        dfrates$ICD_10codes=="O10 - O16" |
        dfrates$ICD_10codes=="O20 - O29" |
        dfrates$ICD_10codes=="O30 - O48" |
        dfrates$ICD_10codes=="O60 - O75" |
        dfrates$ICD_10codes=="O85 - O92" |
        dfrates$ICD_10codes=="O94 - O99")

  

dfrates_adult = dfrates %>%
  group_by(CountyName,dzgroup) %>%
  summarise(mort_tot_crude=sum(Crude_Rate,na.rm = T),
            mort_tot_agest=sum(Age_St_Rate,na.rm = T)) 

dfrates_child = dfrates %>%
  group_by(CountyName) %>%
  summarise(inf_mort = mean(InfantMortality,na.rm = T),
            u5_mort = mean(Under5Mortality,na.rm = T))


dfcit_adult = full_join(dfcit,dfrates_adult)
dfcit_child = full_join(dfcit,dfrates_child)

# 1 Infectious and parasitic diseases (excluding HIV/aids, Tuberculosis, malaria, and neglected tropical diseases)	A00-B99, G00-G04, G14, N70-N73, P37.3, P37.4
# (Except B20-B24, A15-A19, B90, B50-B54, P37.3, P37.4, A30, A31.1, A66, A71, A82,  A90-A91, A92.0, B47, B55-B57, B65, B66.3-B66.4, B66.0-B66.1, B67, B68, B69, B72, B73 B74.0-B74.2, B76.0-B76.1, B77, B79) 
# 2 HIV/aids	B20-B24
# 3 Tuberculosis, malaria, and neglected tropical diseases	A15-A19, B90, B50-B54, P37.3, P37.4, A30, A31.1, A66, A71, A82,  A90-A91, A92.0, B47, B55-B57, B65, B66.3-B66.4, B66.0-B66.1, B67, B68, B69, B72, B73 B74.0-B74.2, B76.0-B76.1, B77, B79
# 4 Respiratory infections and diseases	H65-H66, J00- J98, P23, U04
# 5 Nutritional deficiencies	D50-D53, D64.9, E00-E02, E40-E46, E50-E64
# 6 Neoplasms	C00-D48
# 7 Diseases of the nervous system	F00-F03.9, G10-G13.8, G20-G21.0, G21.2-G24, G24-G25.0, G25.2-G25.3, G25.5, G25.8-G26.0, G30-G31.1, G31.8-G31.9, G35-G37.9, G40-G41.9, G61-G61.9, G70-G72, G72.2-G73.7, G90-G90.9, G95-G95.9, M33-M33.9
# 8 Endocrine disorders	D55-D89 (minus D64.9), E03-E07, E10-E14 (minus  E10.2-E10.29, E11.2-E11.29, E12.2, E13.2-E13.29, E14.2), E15-E34, E65-E88
# 9 Mental and substance use disorders	F04-F99, G72.1, Q86.0, X41-X42, X44, X45
# 10  Stroke	I60-I69.9
# 11  Heart disease	I20-I59.9
# 12  Other cardiovascular diseases	I00-I09.9 I10-15.9 I70-I99.9 
# 13  Digestive diseases	K20-K92.9
# 14  Genitourinary diseases	E10.2-E10.29,E11.2-E11.29,E12.2,E13.2-E13.29,E14.2, N00-N64, N75-N76, N80-N98
# 15  Unintentional injuries	V01-X40, X43, X46-59, Y40-Y86, Y88, Y89
# 16  Intentional injuries	X60-Y09, Y35-Y36, Y870, Y871
# 17  Maternal causes (females only)	O00-O99
hr_id = 0.66
hr_id_sd = (0.66-0.54)/1.96
hr_hiv = 0.85
hr_hiv_sd = (.85-.72)/1.96
hr_tb = 0.59
hr_tb_sd = (0.59-0.42)/1.96
hr_resp = 0.64
hr_resp_sd = (0.64-0.58)/1.96
hr_nut = .56
hr_nut_sd = (.56-.34)/1.96
hr_neo = .75
hr_neo_sd = (.75-.69)/1.96
hr_neuro = .56
hr_neuro_sd = (.56-.39)/1.96
hr_endo = .58
hr_endo_sd = (.58-.50)/1.96
hr_ment = .47
hr_ment_sd = (.47-.32)/1.96
hr_str = .57
hr_str_sd = (.57-.50)/1.96
hr_chd = .49
hr_chd_sd = (.49-.45)/1.96
hr_ohd = .57
hr_ohd_sd = (.57-.49)/1.96
hr_gi = .60
hr_gi_sd = (.60-.50)/1.96
hr_uninj = .49
hr_uninj_sd = (.49-.40)/1.96
hr_intinj = .38
hr_intinj_sd = (.38-.30)/1.96
hr_mat = .97
hr_mat_sd = (.97-.60)/1.96
hr_tot = 0.564
hr_tot_sd = (0.564-0.544)/1.96


hr = c(hr_tot,hr_id,hr_hiv,hr_tb,hr_resp,hr_nut,hr_neo,hr_neuro,hr_endo,hr_ment,hr_str,hr_chd,hr_ohd,hr_gi,hr_uninj,hr_intinj,hr_mat,0)
hr_sd = c(hr_tot_sd,hr_id_sd,hr_hiv_sd,hr_tb_sd,hr_resp_sd,hr_nut_sd,hr_neo_sd,hr_neuro_sd,hr_endo_sd,hr_ment_sd,hr_str_sd,hr_chd_sd,hr_ohd_sd,hr_gi_sd,hr_uninj_sd,hr_intinj_sd,hr_mat_sd,0)
# hr = hr-2*hr_sd
# rate * hr* coveredprop + rate  * (1-coveredpop) = observed
# hence, hr = (observed - (rate*(1-coveredpop)))/(coveredpop*rate)
# 5% decrease for each 10% increase in FHS, per table 1 of https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0182336
# hr = (95 - (100*(1-.5)))/(.5*100) = 0.9
# 3% decrease with each additional year, so upper bound is:
# hr = (97 - (100*(1-.5)))/(.5*100) = 0.94
hr_inf = 0.9
hr_inf_sd = (0.94-0.9)/1.96
hr_u5 = 0.9
hr_u5_sd = (0.94-0.9)/1.96
# hr_inf = hr_inf-2*hr_inf_sd
# hr_u5 = hr_u5-2*hr_u5_s
# backcalculate base rates if no fhs
# baserate * hr* coveredprop + baserate  * (1-coveredpop) = observedrate
# hence, baserate = observedrate / (hr*coveredprop + 1 - coveredprop)

dfcit_adult$base_mort_tot_crude = dfcit_adult$mort_tot_crude / (rep(hr,15)*dfcit_adult$coverage + 1 - dfcit_adult$coverage)
dfcit_adult$base_mort_tot_agest = dfcit_adult$mort_tot_agest / (rep(hr,15)*dfcit_adult$coverage + 1 - dfcit_adult$coverage)

dfcit_child$base_inf_mort = dfcit_child$inf_mort / (hr_inf*dfcit_child$coverage + 1 - dfcit_child$coverage)
dfcit_child$base_u5_mort = dfcit_child$u5_mort / (hr_u5*dfcit_child$coverage + 1 - dfcit_child$coverage)


# newrate = baserate * (hr* newcoveredprop + 1 - newcoveredpop)

delta_cov = (-2:4)/10
result_mort_tot_crude = matrix(0,ncol=length(delta_cov),nrow=dim(dfcit_adult)[1])
result_mort_tot_agest = matrix(0,ncol=length(delta_cov),nrow=dim(dfcit_adult)[1])
result_inf_mort = matrix(0,ncol=length(delta_cov),nrow=dim(dfcit_child)[1])
result_u5_mort = matrix(0,ncol=length(delta_cov),nrow=dim(dfcit_child)[1])
for (i in 1:length(delta_cov)) {
  result_mort_tot_crude[,i] = dfcit_adult$mort_tot_crude * (rep(hr,15) * (dfcit_adult$coverage+delta_cov[i]) +  1 - (dfcit_adult$coverage+delta_cov[i]))
  result_mort_tot_agest[,i] = dfcit_adult$mort_tot_agest * (rep(hr,15) * (dfcit_adult$coverage+delta_cov[i]) +  1 - (dfcit_adult$coverage+delta_cov[i]))
  result_inf_mort[,i] = dfcit_child$inf_mort * (rep(hr_inf,15) * (dfcit_child$coverage+delta_cov[i]) +  1 - (dfcit_child$coverage+delta_cov[i]))
  result_u5_mort[,i] = dfcit_child$u5_mort * (rep(hr_u5,15) * (dfcit_child$coverage+delta_cov[i]) +  1 - (dfcit_child$coverage+delta_cov[i]))
}
  

r_mort_tot_crude = cbind(dfcit_adult$CountyName,result_mort_tot_crude) 
r_mort_tot_agest = cbind(dfcit_adult$CountyName,result_mort_tot_agest) 
r_inf_mort = cbind(dfcit_child$CountyName,result_inf_mort) 
r_u5_mort = cbind(dfcit_child$CountyName,result_u5_mort) 

colnames(r_mort_tot_crude) = c("CountyName","deltacov1","deltacov2","deltacov3","deltacov4","deltacov5","deltacov6","deltacov7")
colnames(r_mort_tot_agest) = c("CountyName","deltacov1","deltacov2","deltacov3","deltacov4","deltacov5","deltacov6","deltacov7")
colnames(r_inf_mort) = c("CountyName","deltacov1","deltacov2","deltacov3","deltacov4","deltacov5","deltacov6","deltacov7")
colnames(r_u5_mort) = c("CountyName","deltacov1","deltacov2","deltacov3","deltacov4","deltacov5","deltacov6","deltacov7")

r_mort_tot_crude = as_tibble(r_mort_tot_crude)
r_mort_tot_agest = as_tibble(r_mort_tot_agest)
r_inf_mort = as_tibble(r_inf_mort)
r_u5_mort = as_tibble(r_u5_mort)

char_columns <- sapply(r_mort_tot_crude, is.character)             # Identify character columns
char_columns[1]=F
r_mort_tot_crude[ , char_columns] <- as.data.frame(   # Recode characters as numeric
        apply(r_mort_tot_crude[ , char_columns], 2, as.numeric))
        sapply(r_mort_tot_crude, class)                       # Print classes of all colums
r_mort_tot_agest[ , char_columns] <- as.data.frame(   # Recode characters as numeric
        apply(r_mort_tot_agest[ , char_columns], 2, as.numeric))
        sapply(r_mort_tot_agest, class)                       # Print classes of all colums
              
        
ra_mort_tot_crude = r_mort_tot_crude %>%
  group_by(CountyName) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)
ra_mort_tot_crude[,2:8] = ra_mort_tot_crude[,2:8]/100

ra_mort_tot_agest = r_mort_tot_agest %>%
  group_by(CountyName) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)
ra_mort_tot_agest[,2:8] = ra_mort_tot_agest[,2:8]/100

tab1 = dfcit_adult %>%
  select(CountyName, mort_tot_crude) %>%
  group_by(CountyName) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)


tab1b = dfcit_adult %>%
  select(CountyName, mort_tot_agest) %>%
  group_by(CountyName) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

tab1c = dfcit_child %>%
  select(CountyName, inf_mort) %>%
  group_by(CountyName) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

tab1d = dfcit_child %>%
  select(CountyName, u5_mort) %>%
  group_by(CountyName) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)




