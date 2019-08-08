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
