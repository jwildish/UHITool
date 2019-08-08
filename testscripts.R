# Inputs 

pop = 26500 #134669
#Baseline mortality rate (deaths per 100,000) United States
#SOURCE: https://www.cdc.gov/nchs/data/dvs/MortFinal2007_Worktable250R.pdf
#US mortality per 100,000 (all causes; all groups)
usmort_all = 803.6
#US mortality per 100,000 (accidents/unintentional injuries; all groups)
usmort_acc = 41.0
#US mortality per 100,000 (all causes less unintentional injuries; all groups) 
usmort_alllessacc = usmort_all - usmort_acc
#Daily US mortality per 100,000 (all causes less unintentional injuries; all groups)
usmort_alllessacc_daily = usmort_alllessacc/365
#Daily US mortality per 100,000 applied to gowanus population size (all causes less unintentional injuries; all groups)
hampmort_alllessacc_daily = usmort_alllessacc_daily * (pop / 100000)