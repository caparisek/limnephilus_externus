## ---------------------------
## Purpose of script: Get info on YSI measurements for lake-stream and June-July. 
## Author: Christine A. Parisek, caparisek@ucdavis.edu
## ---------------------------

library(tidyverse)
library(fBasics)

ysi_full<-read_csv("data/LE_manuscript_only/YSI_lakesbasin_summer2017.csv")

ysi<-ysi_full %>% 
  dplyr::filter(!site_name=="Silver outlet to Cub") #odd site; just exploring for ysi info.

colnames(ysi)

# June & July ----------------------------------------------------


#JUNE+JULY - RANGES 
f<-ysi %>%
  group_by(flow_type) %>%
  summarise(
    Temp_Max = max(temperature_oC, na.rm = T),
    Temp_Min = min(temperature_oC, na.rm = T),
    Temp_Mean=mean(temperature_oC, na.rm = T),
    Temp_SD = sd(temperature_oC, na.rm = T),
    BR_Max = max(barometer_mmHg, na.rm = T),
    BR_Min = min(barometer_mmHg, na.rm = T),
    BR_Mean=mean(barometer_mmHg, na.rm = T),
    BR_SD = sd(barometer_mmHg, na.rm = T),
    DO_Max = max(DO_percent, na.rm = T),
    DO_Min = min(DO_percent, na.rm = T),
    DO_Mean=mean(DO_percent, na.rm = T),
    DO_SD=sd(DO_percent, na.rm = T),
    C_Max = max(conductivity_uScm, na.rm = T),
    C_Min = min(conductivity_uScm, na.rm = T),
    C_Mean=mean(conductivity_uScm, na.rm = T),
    C_SD=sd(conductivity_uScm, na.rm = T),
    pH_Max = max(pH, na.rm = T),
    pH_Min = min(pH, na.rm = T),
    pH_Mean=mean(pH, na.rm = T),
    pH_SD=sd(pH, na.rm = T))

ysi %>% ungroup()

#write_csv(f,"data output/YSI_summary_JuneJuly_MinMaxMean_2021Apr02.csv")


# JUNE+JULY - LAKE VS STREAM TTEST
t.test(data=ysi, var.equal=TRUE, temperature_oC ~ flow_type)
t.test(data=ysi, var.equal=TRUE, barometer_mmHg ~ flow_type)
t.test(data=ysi, var.equal=TRUE, DO_percent ~ flow_type) ##  significant 
t.test(data=ysi, var.equal=TRUE, DO_mgL ~ flow_type) 
t.test(data=ysi, var.equal=TRUE, SPC_uScm_specificconductivity ~ flow_type)
t.test(data=ysi, var.equal=TRUE, conductivity_uScm ~ flow_type)
t.test(data=ysi, var.equal=TRUE, pH ~ flow_type)



# JUNE OR JULY --------------------------------------------------------------------

june<-ysi %>% 
  filter(date=="June2017")

july<-ysi %>% 
  filter(date=="July2017")

# JUNE ONLY 
t.test(data=june, var.equal=TRUE, temperature_oC ~ flow_type)
t.test(data=june, var.equal=TRUE, barometer_mmHg ~ flow_type)
t.test(data=june, var.equal=TRUE, DO_percent ~ flow_type)  
t.test(data=june, var.equal=TRUE, DO_mgL ~ flow_type) 
t.test(data=june, var.equal=TRUE, SPC_uScm_specificconductivity ~ flow_type)
t.test(data=june, var.equal=TRUE, conductivity_uScm ~ flow_type)
t.test(data=june, var.equal=TRUE, pH ~ flow_type) #significant; higher in streams

june %>% 
  group_by(flow_type) %>%
  summarize(Mean = mean(pH, na.rm=TRUE))
june %>% ungroup()

## JULY ONLY
t.test(data=july, var.equal=TRUE, temperature_oC ~ flow_type)
t.test(data=july, var.equal=TRUE, barometer_mmHg ~ flow_type)
t.test(data=july, var.equal=TRUE, DO_percent ~ flow_type)  ## significant; higher in lakes
t.test(data=july, var.equal=TRUE, DO_mgL ~ flow_type) ## significant; higher in lakes
t.test(data=july, var.equal=TRUE, SPC_uScm_specificconductivity ~ flow_type)
t.test(data=july, var.equal=TRUE, conductivity_uScm ~ flow_type)
t.test(data=july, var.equal=TRUE, pH ~ flow_type) #significant; higher in lakes

july %>% 
  group_by(flow_type) %>%
  summarize(Mean_DOpercent = mean(DO_percent, na.rm=TRUE))%>% 
  ungroup()

july %>% 
  group_by(flow_type) %>%
  summarize(Mean_DOmgL = mean(DO_mgL, na.rm=TRUE)) %>% 
  ungroup()

july %>% 
  group_by(flow_type) %>%
  summarize(Mean_pH = mean(pH, na.rm=TRUE)) %>% 
  ungroup()


# PLOT

ysiJuly<-ysi %>% 
  filter(date=="July2017") 
  
ysiJuly %>% 
  ggplot(aes(x = flow_type, y = pH))+
  geom_boxplot()
  
  
  
  
 











