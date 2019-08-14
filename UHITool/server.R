#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    #Inputs
        #If inputs are based on user entered data they must be defined as "reactive"
        #My preference is to use a different name for the reactive variable and the input, to avoid confusion
mypop <- reactive({input$pop})

#Consolidated mortality calc. Source https://www.cdc.gov/nchs/data/dvs/MortFinal2007_Worktable250R.pdf
dailysitemortality <- reactive({ ((803.6-41)/365)* (as.numeric(mypop())/100000)})

#Value of Statistical Life ($2019)
valueofstatisticallife <- 9.652076266

#SOURCE: Anderson (2011), Table 3; "Heat Waves in the United States: Mortality Risk during Heat Waves and Effect Modification by Heat Wave Characteristics in 43 U.S. Communities"
#Change in pct risk of mortality per additional day of heatwave (%)
mort_per_add_day = .38
#Change in pct risk of mortality per 1 degree fahrenheit/celsius increase in average temperature (%)
mort_per_F = 2.49
mort_per_C = mort_per_F*(9/5)

#Change in pct risk of mortality per one-day increase in timing in season (1 May = 1) (%)
mort_timing = -.063

#Establishing temp change parameters
#SOURCE: Mitigating New York City's Heat Island..., Table 3-1 (p. 84 of PDF)

isa_to_grass_avg = -2.5*(5/9)
isa_to_grass_max = -5.8*(5/9)
isa_to_trees_avg = -3.5*(5/9)
isa_to_trees_max = -8.7*(5/9)

#Percent change (of project area) in SA from ISA to Grass
#pct_isa_to_grass = format(round(410446/2495090,2), nsmall = 2)
newgrass <- reactive({input$ScenarioGrass})
oldgrass <- reactive({input$baselineGrass})
newtrees <- reactive({input$ScenarioTrees})
oldtrees <- reactive({input$baselineTrees})

pct_isa_to_grass = reactive({as.numeric(newgrass()) - as.numeric(oldgrass())})
pct_isa_to_trees = reactive({as.numeric(newtrees()) - as.numeric(oldtrees())})

#Import temperature data
tempdata <- read.csv("./Gowanus Sample Temp Data.csv")
gowanus_all <- tempdata
gowanus_all$TAVG <- as.integer(gowanus_all$TAVG)
gowanus_all$TMAX <- as.integer(gowanus_all$TMAX)
gowanus_all$TMIN <- as.integer(gowanus_all$TMIN)
gowanus_all$TOBS <- as.integer(gowanus_all$TOBS)
gowanus_all$DATE2 <- mdy(gowanus_all$DATE)
gowanus <- gowanus_all
gowanus$startdate <- as.Date(paste(format(as.Date(gowanus$DATE2, format="%Y/%m/%d"),"%Y"), 05, 01 , sep = "-"))
gowanus$date_num <- gowanus$DATE2 - gowanus$startdate
gowanus[ , paste0(names(gowanus[c("TAVG","TMAX","TMIN")]), "_C")] <- 
    lapply(gowanus[c("TAVG","TMAX","TMIN")], function(x) (x-32) * 5/9)

gowanus$isa_to_grass_avg <- isa_to_grass_avg
gowanus$isa_to_trees_avg <- isa_to_trees_avg
gowanus <- subset(gowanus, !is.na(TAVG_C))
gowanus$pct95_C <- quantile(gowanus$TAVG_C, .95)

gowanus <- gowanus%>%
    filter(months(DATE2) %in% c("May", "June", "July", "August", "September"))%>%
    filter(year(DATE2) == 2018)
gowanus$tavg_flag <- ifelse(gowanus$TAVG_C >= quantile(gowanus$TAVG_C, .95), 1, 0)

gowanus$heatwave_days <- ifelse(gowanus$tavg_flag == 1, sequence(rle(as.numeric(gowanus$tavg_flag))$lengths)-1, 0)

#Calculates daily RR using Anderson's methodology
gowanus$DegAbovePct95 <- gowanus$TAVG_C - gowanus$pct95_C 
gowanus$rr_temp <- ifelse(gowanus$tavg_flag == 1, (gowanus$DegAbovePct95 * mort_per_C) / 100, 0)
gowanus$rr_add_day = (gowanus$heatwave_days * mort_per_add_day) / 100
gowanus$rr_timing = (gowanus$date_num * mort_timing) / 100
gowanus$rr_tot = 1 + gowanus$rr_temp + gowanus$rr_add_day + gowanus$rr_timing 
#rr_timing = (date-start_date)*mort_timing
#rr_timing = (date-start_date)*mort_timing
gowanus <- gowanus%>%filter(rr_tot > 1)

#Ask Ian about this line
gowanus$mort_uhi <- ifelse(gowanus$rr_tot < 0, 0, gowanus$rr_tot)

#check = sum(gowanus$mort_uhi)

#Calculates aggregate VSL given original UHI effect
uhi_damages0 = ((sum(gowanus$mort_uhi) * valueofstatisticallife*1000000))
uhi_damages1 = format(round(uhi_damages0, 2), nsmall = 2)
uhi_damages <- reactive({as.numeric(uhi_damages1) * as.numeric(dailysitemortality())})


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



#This output just tests the reactive variables are working
output$test <- renderText(print(uhi_damages()))

output$Results<- renderText({(paste0("The Urban Heat Island effect is currently causing an estimated $", 
                                        print(uhi_damages()), 
                                        " in mortality-related damages in the gowanus area per year. Planting an additional ",
                                     print(pct_isa_to_trees()),
                                        " square meters of tree canopy will decrease proximal air temperatures by ",
                                        format(round(-1*isa_to_trees_avg,2),nsmall = 2), " to ",
                                        format(round(-1*isa_to_trees_max,2),nsmall = 2),"ºC; and will reduce the relative risk of mortality by an estimated ",
                                        avg_change_rr_tot, "% in ",
                                       # format(round(pct_isa_to_trees*100,2), nsmall = 2),
                                        "% of the project space." ))


    

    })

})
