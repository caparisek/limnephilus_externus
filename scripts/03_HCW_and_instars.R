## ---------------------------
## Purpose of script: Examine percentage of Limnephilus externus instars across sites. 
## Author: Christine A. Parisek, caparisek@ucdavis.edu
## ---------------------------



library(tidyverse)
library(fBasics) 


# Read in data ------------------------------------------------------------
instar<-read_csv("data/LE_manuscript_only/instar_counts_Lexternus.csv")



# Instar stats ------------------------------------------------------------
colnames(instar)
unique(instar$site)

instar %>% 
  group_by(flow_type,INSTAR) %>% 
  tally(COUNT) %>% 
  mutate(percent = ((n/(sum(n)))*100)) 
instar %>% ungroup()

unique(instar$INSTAR) #categorical
fisher.test(instar$flow_type, 
            instar$INSTAR) #if curious, it's ~same result with or without Tamarack sites.





# FIG - stacked bar chart -------------------------------------------------

unique(instar$INSTAR)

instar2<-instar %>% 
  mutate(INSTAR_rename= case_when(INSTAR== "instar_lessthan_3" ~ "<3rd Instar", 
                                  INSTAR== "instar_4"          ~ "4th Instar", 
                                  INSTAR== "instar_5"          ~ "5th Instar"))

#order the instars into a way you want in the legend
instar2$INSTAR_rename <- factor(instar2$INSTAR_rename, levels = c("<3rd Instar", "4th Instar", "5th Instar"))

instar2$site2  <- factor(instar2$site,levels = c("Big Bear Lake", 
                                                 "Upper Salmon Lake", 
                                                 "Goose Lake", 
                                                 "Tamarack Lake", 
                                                 "Big Bear outlet stream", 
                                                 "Lower Salmon outlet stream", 
                                                 "Upper Salmon inlet stream",
                                                 "Tamarack outlet stream"))

instar2 %>% 
  ggplot(aes(fill=INSTAR_rename, y=COUNT, x = site2))+ 
  geom_bar(position="fill", stat="identity") +
  scale_x_discrete(  expand = expansion(add = c(0, 0))) +
  scale_y_continuous(expand = expansion(add = c(0, 0)) ,labels = scales::percent) +
  scale_fill_brewer(palette = "Blues", labels = c((expression(3^{rd}~"instar")), 
                                                  (expression(4^{th}~"instar")), 
                                                  (expression(5^{th}~"instar"))))+
  theme_bw()+
  labs( y = "Percentage of instar (%)")+
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_text(size=12.8), 
        axis.text.x =element_text(size=12,angle=45,hjust=1,vjust=1),
        axis.text.y =element_text(size=12),
        panel.border = element_blank(), axis.line = element_line(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title = element_blank())


ggsave("figures/Stacked_Bar.tiff", height=5, width=5, units="in", dpi=600)






