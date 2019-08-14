library(tidyr)
library(tidyverse)
library(reshape2)
library(stringr)
library(dplyr)
library(ggplot2)
library(lubridate)

tempdata <- read.csv("./Gowanus Sample Temp Data.csv")

usmort_all = 803.6
#US mortality per 100,000 (accidents/unintentional injuries; all groups)
usmort_acc = 41.0
#US mortality per 100,000 (all causes less unintentional injuries; all groups) 
usmort_alllessacc = usmort_all - usmort_acc
#Daily US mortality per 100,000 (all causes less unintentional injuries; all groups)
usmort_alllessacc_daily = usmort_alllessacc/365
#Daily US mortality per 100,000 applied to gowanus population size (all causes less unintentional injuries; all groups)
pop = 26500 #134669


hampmort_alllessacc_daily = usmort_alllessacc_daily * (pop / 100000)

#SOURCE: EPA (2006$) https://www.epa.gov/environmental-economics/mortality-risk-valuation
vsl_epa2006 = 7.6 
#SOURCE: CPI 01-01-2006 to 02-01-2019 https://fred.stlouisfed.org/series/CPIAUCSL
#Calculation in excel spreadsheet "CPIAUCSL.xls"
vsl_epa2019 = vsl_epa2006 * 1.270010035

#SOURCE: Anderson (2011), Table 3; "Heat Waves in the United States: Mortality Risk during Heat Waves and Effect Modification by Heat Wave Characteristics in 43 U.S. Communities"
#Change in pct risk of mortality per additional day of heatwave (%)
mort_per_add_day = .38
#Change in pct risk of mortality per 1 degree fahrenheit/celsius increase in average temperature (%)
mort_per_F = 2.49
mort_per_C = mort_per_F*(9/5)

#Change in pct risk of mortality per one-day increase in timing in season (1 May = 1) (%)
mort_timing = -.063

#Grams of pollution per m^2 of canopy cover
#SOURCE: Table 1 values from Nowak et al., 2014 (Pollutant year 2000)
poll_removal_low = 6.6
poll_removal_high = 12

#Dollar value per hectar of canopy cover
#SOURCE: Table 1 values from Nowak et al., 2007 (Pollutant year 2000)
usd_per_hec_low = 378
usd_per_hec_high = 663

canopy_cover = 2384/10000


start_date = 05-01
end_date = 09-30


#Assigns date number (1 May = 0, 2 May = 1, etc. per Anderson (2011))

gowanus$startdate <- as.Date(paste(format(as.Date(gowanus$DATE2, format="%Y/%m/%d"),"%Y"), 05, 01 , sep = "-"))
gowanus$date_num <- gowanus$DATE2 - gowanus$startdate



#Adds reduced UHI effect column to dataset
gowanus$isa_to_grass_avg <- isa_to_grass_avg
gowanus$isa_to_trees_avg <- isa_to_trees_avg

#Calculates/creates column of 95 percentile value for TAVG_C and creates flag column
#for TAVG_C values >= 95th percentile (REMOVED THIS SECTION OF CODE: or values greater than 32 degrees Celsius)
gowanus$pct95_C <- quantile(gowanus$TAVG_C, .95)

gowanus <- gowanus%>%
  filter(months(DATE2) %in% c("May", "June", "July", "August", "September"))%>%
  filter(year(DATE2) == 2018)



gowanus$tavg_flag <- ifelse(gowanus$TAVG_C >= quantile(gowanus$TAVG_C, .95), 1, 0) #| gowanus$TAVG_C >= 32, 1, 0)

#Counts heatwave days where a heat wave is defined as >=2 consecutive days where 
#TAVG >= 95th percentile of daily TAVG
gowanus$heatwave_days <- ifelse(gowanus$tavg_flag == 1, sequence(rle(as.numeric(gowanus$tavg_flag))$lengths)-1, 0)

sum(gowanus$tavg_flag) 
#Calculates daily RR using Anderson's methodology
gowanus$DegAbovePct95 <- gowanus$TAVG_C - gowanus$pct95_C 
gowanus$rr_temp <- ifelse(gowanus$tavg_flag == 1, (gowanus$DegAbovePct95 * mort_per_C) / 100, 0)
gowanus$rr_add_day = (gowanus$heatwave_days * mort_per_add_day) / 100
gowanus$rr_timing = (gowanus$date_num * mort_timing) / 100
gowanus$rr_tot = 1 + gowanus$rr_temp + gowanus$rr_add_day + gowanus$rr_timing 
#rr_timing = (date-start_date)*mort_timing

gowanus <- gowanus%>%filter(rr_tot > 1)

gowanus$mort_uhi <- ifelse(gowanus$rr_tot * hampmort_alllessacc_daily - hampmort_alllessacc_daily < 0, 0, gowanus$rr_tot * hampmort_alllessacc_daily - hampmort_alllessacc_daily)

#check = sum(gowanus$mort_uhi)

#Calculates aggregate VSL given original UHI effect
uhi_damages0 = ((sum(gowanus$mort_uhi) * vsl_epa2019)*1000000)
uhi_damages = format(round(uhi_damages0, 2), nsmall = 2)

#Calculates/creates column of 95 percentile value for reduced TAVG_C given change in ISA and creates flag column
#for red_TAVG_C values >= 95th percentile (REMOVED THIS SECTION OF CODE: or values greater than 32 degrees Celsius

gowanus$i2g_TAVG_C <- gowanus$TAVG_C + gowanus$isa_to_grass_avg
gowanus$i2t_TAVG_C <- gowanus$TAVG_C + gowanus$isa_to_trees_avg
#gowanus$red_TMAX_C <- gowanus$TMAX_C - gowanus$red_uhi_C
#gowanus$red_TMIN_C <- gowanus$TMIN_C - gowanus$red_uhi_C

gowanus$i2g_red_tavg_flag <- ifelse(gowanus$i2g_TAVG_C >= gowanus$pct95_C, 1, 0)# | gowanus$red_TMAX_C >= 32,1,0)
gowanus$i2t_red_tavg_flag <- ifelse(gowanus$i2t_TAVG_C >= gowanus$pct95_C, 1, 0)# | gowanus$red_TMAX_C >= 32,1,0)
#gowanus$red_tmax_flag <- ifelse(gowanus$red_TMAX_C >= quantile(gowanus$TMAX_C, .95), 1, 0)# | gowanus$red_TMAX_C >= 32,1,0)
#gowanus$red_tmin_flag <- ifelse(gowanus$red_TMIN_C >= quantile(gowanus$TMAX_C, .95), 1, 0)# | gowanus$red_TMIN_C >= 32,1,0) 

#______________________________________________  
#Created flag column for days where temperature was above 95pct threshold and would be moved below it due to 
#change in ISA
gowanus$i2g_safe_flag <- ifelse(gowanus$i2g_red_tavg_flag != gowanus$tavg_flag,1,0) #| gowanus$red_tmin_flag != gowanus$tmin_flag,1,0)
gowanus$i2t_safe_flag <- ifelse(gowanus$i2t_red_tavg_flag != gowanus$tavg_flag,1,0) #| gowanus$red_tmin_flag != gowanus$tmin_flag,1,0)

#Check <- gowanus%>%filter(gowanus$i2g_red_flag != gowanus$i2g_safe_flag)
# TempLoweredgowanus <- gowanus%>%
#   filter(gowanus$red_safe_flag ==1)
#______________________________________________

#Counts heatwave days for reduced temperature values where a heat wave is defined as >=2 consecutive days where 
#TAVG >= 95th percentile of daily TAVG
gowanus$i2g_red_DegAbovePct95 = gowanus$i2g_TAVG_C - gowanus$pct95_C
gowanus$i2t_red_DegAbovePct95 = gowanus$i2t_TAVG_C - gowanus$pct95_C


gowanus$i2g_red_heatwave_days <- ifelse(gowanus$i2g_red_tavg_flag == 1, sequence(rle(as.numeric(gowanus$i2g_red_tavg_flag))$lengths)-1, 0)
gowanus$i2t_red_heatwave_days <- ifelse(gowanus$i2t_red_tavg_flag == 1, sequence(rle(as.numeric(gowanus$i2t_red_tavg_flag))$lengths)-1, 0)

#Calculates daily RR of reduced temperature values using Anderson's methodology 
#(NOTE: Timing component, regional component not incorporated)
gowanus$i2g_rr1_temp <- ifelse(gowanus$i2g_red_tavg_flag == 1, (gowanus$i2g_red_DegAbovePct95 * mort_per_C) / 100, 0)
gowanus$i2g_rr1_add_day = (gowanus$i2g_red_heatwave_days * mort_per_add_day) / 100
gowanus$i2g_rr1_tot = 1 + gowanus$i2g_rr1_temp + gowanus$i2g_rr1_add_day + gowanus$rr_timing

gowanus$i2t_rr1_temp <- ifelse(gowanus$i2t_red_tavg_flag == 1, (gowanus$i2t_red_DegAbovePct95 * mort_per_C) / 100, 0)
gowanus$i2t_rr1_add_day = (gowanus$i2t_red_heatwave_days * mort_per_add_day) / 100
gowanus$i2t_rr1_tot = 1 + gowanus$i2t_rr1_temp + gowanus$i2t_rr1_add_day + gowanus$rr_timing
#rr_timing = (date-start_date)*mort_timing
#gowanus$red_mort_uhi <- gowanus$rr1_tot * hampmort_alllessacc_daily - hampmort_alllessacc_daily

#Calculates aggregate VSL given reduced temperature
#red_uhi_damages = sum(gowanus$red_mort_uhi) * vsl_epa2019

#Calculates change in relative risk in areas where grass is replaced with trees
gowanus$change_rr_tot <- gowanus$rr_tot - (1 + (gowanus$i2g_rr1_temp + gowanus$i2g_rr1_add_day)+(gowanus$i2t_rr1_temp + gowanus$i2t_rr1_add_day) + gowanus$rr_timing)

avg_change_rr_tot = format(round(as.numeric(mean(gowanus$change_rr_tot, na.rm = FALSE)*100), 2), nsmall = 2)
