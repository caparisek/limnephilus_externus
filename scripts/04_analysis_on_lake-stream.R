## ---------------------------
## Purpose of script: Compare Limnephilus externus collected from lakes and streams. 
## Author: Christine A. Parisek, caparisek@ucdavis.edu
## ---------------------------

library(tidyverse)
library(stats)
library(fBasics)

#Read in data
DF <- read_csv("data/LE_manuscript_only/morphology_measurements_spring2018_LE.csv")

#Explore
DF %>% 
  group_by(flow_type) %>% 
  dplyr::filter(!site_code=="6LHA") %>%
  dplyr::filter(!site_code=="8LLO") %>% 
  summarise(n = n()) 


#Keep only LakesBasin; remove Tamarack context site or NA morph context site. 
LakesBasin<-DF %>% 
  dplyr::filter(region=="lakesbasin") %>% 
  dplyr::filter(!site_code=="6LHA")   %>% 
  dplyr::filter(!site_code=="8LLO") 



# Head Capsule Width (HCW) stats ------------------------------------------
fBasics::basicStats(LakesBasin$head_capsule_width_mm)
# Minimum                                1.400000
# Maximum                                1.820000
# Mean                                   1.612794
# Median                                 1.595000
# (1.40-1.82mm, mean 1.61mm, median 1.595)





# basic mean/sd-----------------------------------------------------------
colnames(LakesBasin)

LakesBasin %>%
  group_by(flow_type) %>%   
  drop_na(body_length_mm) %>% 
  dplyr::summarize(meanbio = mean(body_length_mm),    
                   sd = sd(body_length_mm))
LakesBasin %>%
  group_by(flow_type) %>% 
  drop_na(body_width_mm_2ndABsegment) %>%
  dplyr::summarize(meanbio = mean(body_width_mm_2ndABsegment),    
                   sd = sd(body_width_mm_2ndABsegment)) 
LakesBasin %>%
  group_by(flow_type) %>% 
  drop_na(pronotum_length_mm) %>%
  dplyr::summarize(meanbio = mean(pronotum_length_mm),    
                   sd = sd(pronotum_length_mm))
LakesBasin %>%
  group_by(flow_type) %>% 
  drop_na(pronotum_width_mm) %>%
  dplyr::summarize(meanbio = mean(pronotum_width_mm),    
                   sd = sd(pronotum_width_mm)/sqrt(n())) 
LakesBasin %>%
  group_by(flow_type) %>% 
  drop_na(head_capsule_width_mm) %>%
  dplyr::summarize(meanbio = mean(head_capsule_width_mm),    
                   sd = sd(head_capsule_width_mm)) 
LakesBasin %>%
  group_by(flow_type) %>% 
  drop_na(case_length_mm) %>%
  dplyr::summarize(meanbio = mean(case_length_mm),    
                   sd = sd(case_length_mm))
LakesBasin %>%
  group_by(flow_type) %>% 
  drop_na(case_max_width_mm_AtWidestPoint) %>%
  dplyr::summarize(meanbio = mean(case_max_width_mm_AtWidestPoint),    
                   sd = sd(case_max_width_mm_AtWidestPoint))

LakesBasin %>%
  group_by(flow_type) %>% 
  drop_na(mites_abdominal_counted) %>%
  dplyr::summarize(meanbio = mean(mites_abdominal_counted),    
                   sd = sd(mites_abdominal_counted))





# lake-stream t-tests ------------------------------------------------------
var.test(data=LakesBasin, body_length_mm ~ flow_type) # var.equal=FALSE 
var.test(data=LakesBasin, body_width_mm_2ndABsegment ~ flow_type) 
var.test(data=LakesBasin, pronotum_length_mm ~ flow_type) 
var.test(data=LakesBasin, pronotum_width_mm ~ flow_type) 
var.test(data=LakesBasin, head_capsule_width_mm ~ flow_type) 
var.test(data=LakesBasin, case_length_mm ~ flow_type) 
var.test(data=LakesBasin, case_max_width_mm_AtWidestPoint ~ flow_type) 

t.test(data=LakesBasin, var.equal=FALSE, body_length_mm ~ flow_type) #t = -0.14409, df = 15, p-value = 0.8873
t.test(data=LakesBasin, var.equal=TRUE, body_width_mm_2ndABsegment ~ flow_type) #t = -1.812, df = 32, p-value = 0.07938 .
t.test(data=LakesBasin, var.equal=TRUE, pronotum_length_mm ~ flow_type) #t = -0.28116, df = 32, p-value = 0.7804
t.test(data=LakesBasin, var.equal=TRUE, pronotum_width_mm ~ flow_type) #t = -0.9991, df = 32, p-value = 0.3252
t.test(data=LakesBasin, var.equal=TRUE, head_capsule_width_mm ~ flow_type) #t = -1.1788, df = 32, p-value = 0.2472
t.test(data=LakesBasin, var.equal=TRUE, case_length_mm ~ flow_type) #t = -4.4198, df = 29, p-value = 0.0001267 ****************
t.test(data=LakesBasin, var.equal=TRUE, case_max_width_mm_AtWidestPoint ~ flow_type) #t = 0.82871, df = 29, p-value = 0.414




# qualitative - %s and fisher --------------------------------------------

colnames(LakesBasin)

#GILL SPOTTING (ABDOMINAL CONDITION)
LakesBasin %>%
  drop_na(gill_condition_spotting) %>% 
  group_by(flow_type,gill_condition_spotting) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100)) 

fisher.test(LakesBasin$flow_type, 
            LakesBasin$gill_condition_spotting) #significant 

#GILL LENGTH
LakesBasin %>%
  drop_na(gill_length) %>% 
  group_by(flow_type,gill_length) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100)) 

fisher.test(LakesBasin$flow_type, 
            LakesBasin$gill_length)

#GILL THICKNESS
LakesBasin %>%
  drop_na(gill_thickness) %>% 
  group_by(flow_type,gill_thickness) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100))

fisher.test(LakesBasin$flow_type, 
            LakesBasin$gill_thickness) #significant 

#HEAD PIGMENTATION
LakesBasin %>%
  drop_na(head_pigment) %>% 
  group_by(flow_type,head_pigment) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100)) 

fisher.test(LakesBasin$flow_type, 
            LakesBasin$head_pigment)


# ABDOMINAL MITES - PRESENCE OR ABSENCE 
LakesBasin %>%
  drop_na(mites_abdominal_presenceabsence_Yes1_No0) %>% 
  group_by(flow_type,mites_abdominal_presenceabsence_Yes1_No0) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100)) 

fisher.test(LakesBasin$flow_type, 
            LakesBasin$mites_abdominal_presenceabsence_Yes1_No0) #significant 


colnames(LakesBasin)
#CASE WIDTH (SHAPE)
LakesBasin %>%
  drop_na(case_shape_width) %>% 
  group_by(flow_type,case_shape_width) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100)) 

fisher.test(LakesBasin$flow_type, 
            LakesBasin$case_shape_width)


#CASE BULDGE LOCATION (not included in MS; overlaps w above a bit)
# i.e., was the widest point on the case width at the front/back or middle of the case.
LakesBasin %>%
  drop_na(case_buldge_location) %>% 
  group_by(flow_type,case_buldge_location) %>% 
  dplyr::filter(!case_buldge_location=="0") %>% # silly to have "none" here when it is in the above binary question. 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100)) 

fisher.test(LakesBasin$flow_type, 
            LakesBasin$case_buldge_location)


#CASE SILTINESS
# originally 0 (no silt), 1 (some silt), 2 (very silty).
# silt is fine organic or sediment material that is stuck to the case; not easily shaken off. 
LakesBasin$case_siltiness_alt <- factor(LakesBasin$case_siltiness, 
                                      levels = c("0", 
                                                 "1",
                                                 "2"),
                                      labels = c("0",
                                                 "1",
                                                 "1")) #rename  any silt = "1". make binary.
LakesBasin %>%
  drop_na(case_siltiness_alt) %>% 
  group_by(flow_type,case_siltiness_alt) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100)) 

fisher.test(LakesBasin$flow_type, 
            LakesBasin$case_siltiness_alt)



#CASE FLUFFINESS (MATERIAL TYPE)
LakesBasin %>%
  drop_na(case_material_fluffiness) %>% 
  group_by(flow_type,case_material_fluffiness) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100)) 

fisher.test(LakesBasin$flow_type, 
            LakesBasin$case_material_fluffiness)


#CASE STURDINESS OF STRUCTURE 
LakesBasin %>%
  drop_na(case_sturdinessofstructure) %>% 
  group_by(flow_type,case_sturdinessofstructure) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100))  # all lakes basin cases were relatively strong (3) and sturdy (4). only tamarack was 1-2 (fragile/breaking)

fisher.test(LakesBasin$flow_type, 
            LakesBasin$case_sturdinessofstructure)

#CASE BARK LENGTH 
LakesBasin %>%
  drop_na(case_bark_length) %>% 
  group_by(flow_type,case_bark_length) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100))

fisher.test(LakesBasin$flow_type, 
            LakesBasin$case_bark_length)

#CASE LATERAL EXTENTIONS
LakesBasin %>%
  drop_na(case_extentions) %>% 
  group_by(flow_type,case_extentions) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100)) 

fisher.test(LakesBasin$flow_type, 
            LakesBasin$case_extentions)


#CASE NEATNESS (ASSEMBLY UNIFORMITY)
LakesBasin %>%
  drop_na(case_uniformity_neatness) %>% 
  group_by(flow_type,case_uniformity_neatness) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100)) 

fisher.test(LakesBasin$flow_type, 
            LakesBasin$case_uniformity_neatness)

#CASE HITCHHIKER - PRESENCE OR ABSENCE 
LakesBasin %>%
  drop_na(case_hitchhikers) %>% 
  group_by(flow_type,case_hitchhikers) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100)) 

fisher.test(LakesBasin$flow_type, 
            LakesBasin$case_hitchhikers)





