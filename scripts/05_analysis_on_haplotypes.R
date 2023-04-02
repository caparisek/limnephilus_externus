## ---------------------------
## Purpose of script: Compare Limnephilus externus individuals across the 3 primary haplotypes that emerged from this analysis. 
## Author: Christine A. Parisek, caparisek@ucdavis.edu
## ---------------------------

library(tidyverse)
library(agricolae) # Tukey
library(fBasics)   # summary stats
library(patchwork) # multipanel plots
# library(LaCroixColoR) #color not used in final plot.


# Read in data 
df<-read_csv("data/LE_manuscript_only/morphology_measurements_spring2018_LE.csv")

colnames(df)

#Explore
df %>% 
  group_by(haplotype) %>% #group by haplotype assignment H1, H2, or H3
  summarise(n = n())
df %>% ungroup()

10+13+6

#Tidy data to have only relevant sites and no NAs
df_filtered<-df %>% 
  dplyr::filter(!haplotype=="NA") %>% #remove NA from this column
  dplyr::filter(!site_code=="6LHA", !site_code=="8LLO") #remove sites w NA quant. info


#Create new columns for haplotype for plotting aesthetics; Have H1-H3 in that order. 
df_filtered$haplotype_num <- factor(df_filtered$haplotype, 
                                levels = c("one", 
                                           "two",
                                           "three"),
                                labels = c("1",
                                           "2",
                                           "3")) #new column (one = 1, etc.)
df_filtered$haplotype_text <- factor(df_filtered$haplotype, 
                                      levels = c("one", 
                                                 "two",
                                                 "three"),
                                      labels = c("H1",
                                                 "H2",
                                                 "H3")) #new column (one = H1, etc.)

CADDIS <- df_filtered
colnames(CADDIS)



# basic mean/se -----------------------------------------------------------
CADDIS %>%
  group_by(haplotype) %>% 
  dplyr::summarize(meanbio = mean(body_length_mm),    
                   se = sd(body_length_mm)/sqrt(n())) 
CADDIS %>% ungroup()

means<-CADDIS %>% 
  ungroup() %>%
  select(haplotype,
         body_length_mm,                      
         body_width_mm_2ndABsegment,          
         pronotum_length_mm,         
         pronotum_width_mm,                   
         head_capsule_width_mm,            
         case_length_mm,     
         case_max_width_mm_AtWidestPoint) %>% 
  group_by(haplotype) %>% 
  summarise_each(funs(mean, sd))

CADDIS %>% ungroup()


# microinvert % s ----------------------------------------------------------------------

colnames(CADDIS)
CADDIS %>%
  drop_na(case_shape_width) %>% 
  group_by(flow_type,case_hitchhikers) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100))  # lake-stream case hitchhiker %
CADDIS %>% ungroup()

CADDIS %>%
  drop_na(case_shape_width) %>% 
  group_by(haplotype,case_shape_width) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100)) # lake stream case hitchhiker % (haplotype specific)
CADDIS %>% ungroup()


# qualitative %, fisher test on HAPLOTYPES ------------------------------------

colnames(CADDIS)

#GILL HEALTH
CADDIS %>%
  drop_na(gill_condition_spotting) %>% 
  group_by(haplotype,gill_condition_spotting) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100)) 
CADDIS %>% ungroup()
fisher.test(CADDIS$haplotype, 
            CADDIS$gill_condition_spotting) 

#GILL LENGTH
CADDIS %>%
  drop_na(gill_length) %>% 
  group_by(haplotype,gill_length) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100)) 
CADDIS %>% ungroup()
fisher.test(CADDIS$haplotype, 
            CADDIS$gill_length)

#GILL THICKNESS
CADDIS %>%
  drop_na(gill_thickness) %>% 
  group_by(haplotype,gill_thickness) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100))
CADDIS %>% ungroup()
fisher.test(CADDIS$haplotype, 
            CADDIS$gill_thickness) 

#HEAD PIGMENTATION
CADDIS %>%
  drop_na(head_pigment) %>% 
  group_by(haplotype,head_pigment) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100)) 
CADDIS %>% ungroup()
fisher.test(CADDIS$haplotype, 
            CADDIS$head_pigment) #almost significant...!

# ABDOMINAL MITES - PRESENCE OR ABSENCE 
CADDIS %>%
  drop_na(mites_abdominal_presenceabsence_Yes1_No0) %>% 
  group_by(haplotype,mites_abdominal_presenceabsence_Yes1_No0) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100)) 
CADDIS %>% ungroup()
fisher.test(CADDIS$haplotype, 
            CADDIS$mites_abdominal_presenceabsence_Yes1_No0) #significant 

#CASE WIDTH (SHAPE)
CADDIS %>%
  drop_na(case_shape_width) %>% 
  group_by(haplotype,case_shape_width) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100)) 
CADDIS %>% ungroup()
fisher.test(CADDIS$haplotype, 
            CADDIS$case_shape_width)


#CASE BULDGE LOCATION (not included in MS; overlaps w above a bit)
# i.e., was the widest point on the case width at the front/back or middle of the case.
unique(CADDIS$case_buldge_location)
CADDIS %>%
  drop_na(case_buldge_location) %>% 
  group_by(haplotype,case_buldge_location) %>% 
  dplyr::filter(!case_buldge_location=="0") %>% # removing "none" here bc its in above binary Q 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100)) 
CADDIS %>% ungroup()
bulg<-CADDIS %>% 
  dplyr::filter(!case_buldge_location=="0")
fisher.test(bulg$haplotype, 
            bulg$case_buldge_location)

#CASE SILTINESS
# originally 0 (no silt), 1 (some silt), 2 (very silty).
# silt is fine organic or sediment material that is stuck to the case; not easily shaken off. 
CADDIS$case_siltiness_alt <- factor(CADDIS$case_siltiness, 
                                        levels = c("0", 
                                                   "1",
                                                   "2"),
                                        labels = c("0",
                                                   "1",
                                                   "1")) #rename  any silt = "1". make binary.
CADDIS %>%
  drop_na(case_siltiness_alt) %>% 
  group_by(haplotype,case_siltiness_alt) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100))
CADDIS %>% ungroup()
fisher.test(CADDIS$haplotype, 
            CADDIS$case_siltiness_alt)


#CASE FLUFFINESS (MATERIAL TYPE)
CADDIS %>%
  drop_na(case_material_fluffiness) %>% 
  group_by(haplotype,case_material_fluffiness) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100)) 
CADDIS %>% ungroup()
fisher.test(CADDIS$haplotype, 
            CADDIS$case_material_fluffiness)

#CASE STURDINESS OF STRUCTURE 
CADDIS %>%
  drop_na(case_sturdinessofstructure) %>% 
  group_by(haplotype,case_sturdinessofstructure) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100))  # all lakes basin cases were relatively strong (3) and sturdy (4). only tamarack was 1-2 (fragile/breaking)
CADDIS %>% ungroup()
fisher.test(CADDIS$haplotype, 
            CADDIS$case_sturdinessofstructure) #significant

#CASE BARK LENGTH 
CADDIS %>%
  drop_na(case_bark_length) %>% 
  group_by(haplotype,case_bark_length) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100))
CADDIS %>% ungroup()
fisher.test(CADDIS$haplotype, 
            CADDIS$case_bark_length)

#CASE EXTENTIONS
CADDIS %>%
  drop_na(case_extentions) %>% 
  group_by(haplotype,case_extentions) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100)) 
CADDIS %>% ungroup()
fisher.test(CADDIS$haplotype, 
            CADDIS$case_extentions)


#CASE NEATNESS (UNIFORMITY)
CADDIS %>%
  drop_na(case_uniformity_neatness) %>% 
  group_by(haplotype,case_uniformity_neatness) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100)) 
CADDIS %>% ungroup()
fisher.test(CADDIS$haplotype, 
            CADDIS$case_uniformity_neatness)

#CASE HITCHHIKER - PRESENCE OR ABSENCE 
CADDIS %>%
  drop_na(case_hitchhikers) %>% 
  group_by(haplotype,case_hitchhikers) %>% 
  tally() %>% 
  mutate(percent = ((n/(sum(n)))*100)) 
CADDIS %>% ungroup()
fisher.test(CADDIS$haplotype, 
            CADDIS$case_hitchhikers)



# two-way anova -----------------------------------------------------------
# YVAR ~ flow + haplotype + flow:haplotype

AOV_body_length<-aov(body_length_mm ~ flow_type + haplotype + flow_type:haplotype, data = CADDIS) # *
summary(AOV_body_length)

AOV_body_width<-aov(body_width_mm_2ndABsegment ~ flow_type + haplotype+ flow_type:haplotype, data = CADDIS)
summary(AOV_body_width)

AOV_pronotum_length<-aov(pronotum_length_mm ~ flow_type + haplotype + flow_type:haplotype, data = CADDIS) # **
summary(AOV_pronotum_length)

AOV_pronotum_width<-aov(pronotum_width_mm ~ flow_type + haplotype+ flow_type:haplotype, data = CADDIS)
summary(AOV_pronotum_width)

AOV_HCW<-aov(head_capsule_width_mm ~ flow_type + haplotype+ flow_type:haplotype, data = CADDIS)
summary(AOV_HCW)

AOV_case_length<-aov(case_length_mm ~ flow_type + haplotype+ flow_type:haplotype, data = CADDIS) # *.
summary(AOV_case_length)

AOV_case_shape_width<-aov(case_max_width_mm_AtWidestPoint ~ flow_type + haplotype+ flow_type:haplotype, data = CADDIS)
summary(AOV_case_shape_width)







# FIGURES - body length ---------------------------------------------------


# calculate mean and standard error 
SUMbody <- CADDIS %>%
  group_by(haplotype,flow_type,.groups = "keep") %>% 
  dplyr::summarize(meanbio = mean(body_length_mm),    
                   se = sd(body_length_mm)/sqrt(n())) 
CADDIS %>% ungroup()
PLOTbodylength <-left_join(CADDIS, SUMbody, by=c('haplotype', 'flow_type')) #join for: error bar + raw point plotting capability)


# TUKEY - 
AOV1<-aov(CADDIS$body_length_mm ~ CADDIS$flow_type + CADDIS$haplotype) # * (repeated line from above)
summary(AOV1) # regular 2way anova output
TUKEY <- TukeyHSD(x=AOV1, 'CADDIS$haplotype', conf.level=0.95) #Tukey Method #1
TUKEY 
plot(TUKEY) # Tukey #1
tukey.test2 <- HSD.test(AOV1, trt = 'CADDIS$haplotype') #Tukey Method #2
tukey.test2 # (confirms #1)


# PLOT
fBasics::basicStats(PLOTbodylength$body_length_mm)
A<-PLOTbodylength %>% 
  ggplot(aes(factor(x=haplotype_text,level = c('H1', 'H2', 'H3')), y=body_length_mm))+
  geom_point(aes(shape=flow_type), size=4, alpha=0.6)+
  geom_errorbar(aes(ymin=meanbio-se, ymax=meanbio+se, linetype=flow_type), width=.08,position = "dodge")+
  #scale_color_manual(values=(lacroix_palette("Apricot",type = "continuous", n=2)))+
  theme_bw()+
  scale_y_continuous(breaks=seq(10,25,3))+
  annotate(geom="text", x=1, y=10.5, label="A",  color="black", size=4)+
  annotate(geom="text", x=2, y=10.5, label="B",  color="black", size=4)+
  annotate(geom="text", x=3, y=10.5, label="AB", color="black", size=4)+
  xlab("")+
  ylab("Body length (mm)") + 
  labs(color='')+
  theme(panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),
        panel.border = element_blank(), axis.line = element_line(),
        #legend.text =  element_text(size=16)
        legend.position = "none", 
        axis.title.y = element_text(size=12),
        axis.title.x = element_text(size=9),
        axis.text.x =  element_text(size=10, color="black"),
        axis.text.y =  element_text(size=8, color="black"))
 



# FIGURES - pronotum length -----------------------------------------------

# calculate mean and standard error 
SUMpronotum <- CADDIS %>%
  group_by(haplotype,flow_type,.groups = "keep") %>% 
  dplyr::summarize(meanbio = mean(pronotum_length_mm),    
                   se = sd(pronotum_length_mm)/sqrt(n())) 
CADDIS %>% ungroup()
PLOTpronotum <-left_join(CADDIS, SUMpronotum, by=c('haplotype', 'flow_type')) #join for: error bar + raw point plotting capability)

# TUKEY - 
AOV3<-aov(CADDIS$pronotum_length_mm ~ CADDIS$flow_type + CADDIS$haplotype) # * (repeated line from above)
summary(AOV3) # regular 2way anova output
TUKEY <- TukeyHSD(x=AOV3, 'CADDIS$haplotype', conf.level=0.95) #Tukey Method #1
TUKEY 
plot(TUKEY) # Tukey #1
tukey.test2 <- HSD.test(AOV3, trt = 'CADDIS$haplotype') #Tukey Method #2
tukey.test2 # (confirms #1)


# PLOT 
fBasics::basicStats(PLOTpronotum$pronotum_length_mm)
B<-PLOTpronotum %>% 
  ggplot(aes(factor(x=haplotype_text,level = c('H1', 'H2', 'H3')), y=pronotum_length_mm))+
  geom_point(aes(shape=flow_type), size=4, alpha=0.6)+
  geom_errorbar(aes(ymin=meanbio-se, ymax=meanbio+se, linetype=flow_type), width=.08,position = "dodge")+
  #scale_color_manual(values=(lacroix_palette("Apricot",type = "continuous", n=2)))+
  theme_bw()+
  scale_y_continuous(breaks=seq(0,2,0.1))+
  annotate(geom="text", x=1, y=0.95, label="A",  color="black", size=4)+
  annotate(geom="text", x=2, y=0.95, label="B",  color="black", size=4)+
  annotate(geom="text", x=3, y=0.95, label="B",  color="black", size=4)+
  xlab("")+
  ylab("Pronotum length (mm)") + 
  labs(color='')+
  theme(panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),
        panel.border = element_blank(), axis.line = element_line(),
        #legend.text =  element_text(size=16)
        legend.position = "none", 
        axis.title.y = element_text(size=12),
        axis.title.x = element_text(size=9),
        axis.text.x =  element_text(size=10, color="black"),
        axis.text.y =  element_text(size=8, color="black"))




# FIGURES - case length ---------------------------------------------------

# calculate mean and standard error 
SUMcase <- CADDIS %>%
  group_by(haplotype,flow_type,.groups = "keep") %>% 
  dplyr::filter(!is.na(case_length_mm)) %>% 
  dplyr::summarize(meanbio = mean(case_length_mm),    
                   se = sd(case_length_mm)/sqrt(n())) 
CADDIS %>% ungroup()
new<-CADDIS %>% 
  dplyr::filter(!is.na(case_length_mm)) #remove the 3 NA datapoints for case_length

PLOTcase <-left_join(new, SUMcase, by=c('haplotype', 'flow_type')) #join for: error bar + raw point plotting capability)

# TUKEY - 
AOV6<-aov(CADDIS$case_length_mm ~ CADDIS$flow_type + CADDIS$haplotype) # * (repeated line from above)
summary(AOV6) # regular 2way anova output
TUKEY <- TukeyHSD(x=AOV6, 'CADDIS$haplotype', conf.level=0.95) #Tukey Method #1
TUKEY 
plot(TUKEY) # Tukey #1
tukey.test2 <- HSD.test(AOV6, trt = 'CADDIS$haplotype') #Tukey Method #2
tukey.test2 # (confirms #1)


# PLOT
fBasics::basicStats(PLOTcase$case_length_mm)
C<-PLOTcase %>% 
  ggplot(aes(factor(x=haplotype_text,level = c('H1', 'H2', 'H3')), y=case_length_mm))+
  geom_point(aes(shape=flow_type), size=4, alpha=0.6)+
  geom_errorbar(aes(ymin=meanbio-se, ymax=meanbio+se, linetype=flow_type), width=.08,position = "dodge")+
  #scale_color_manual(values=(lacroix_palette("Apricot",type = "continuous", n=2)))+
  theme_bw()+
  scale_y_continuous(breaks=seq(10,25,3))+
  annotate(geom="text", x=1, y=10.5, label="A",  color="black", size=4)+
  annotate(geom="text", x=2, y=10.5, label="A",  color="black", size=4)+
  annotate(geom="text", x=3, y=10.5, label="A",  color="black", size=4)+
  xlab("")+
  ylab("Case length (mm)") + 
  labs(color='')+
  theme(panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),
        panel.border = element_blank(), axis.line = element_line(),
        axis.title.y = element_text(size=12),
        axis.title.x = element_text(size=9),
        axis.text.x =  element_text(size=10, color="black"),
        axis.text.y =  element_text(size=8, color="black"))

B+A+C

ggsave("figures/Clades2_BW_with_legend.tiff", height=10, width=16, units="cm", dpi=600)



