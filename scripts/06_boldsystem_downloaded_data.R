## ---------------------------
## Purpose of script: Explore and tidy metadata downloaded from BOLD for sequences used in analysis. Label BOLD BINs into user-friendly A-B-C format and create new column that will be a combination of meta-data to use in the phylogentic tree's branch label, making it easier to understand which specimens come from which location on the tree. 
## Author: Christine A. Parisek, caparisek@ucdavis.edu
## ---------------------------


library(tidyverse)


# Read in Data ------------------------------------------------------------
bold <- readr::read_tsv("data/bold_data.txt",
                 col_names = TRUE,
                 na = c("", "NA"))

write_csv(bold, "data output/bold_data_original_untidied.csv") #export as CSV


# Explore & Tidy Metadata -----------------------------------------------------------------
unique(bold$processid) #252 uniques, but 255 total rows...

unique     <- bold[!duplicated(bold$processid),] #puts uniques into a df 
duplicated <- bold[duplicated(bold$processid),]  #puts non-uniques into a df
Odd <- bold %>% filter(processid=="EBTCH542-11") #all 4 dup entries are identical, so it doesn't matter which was pulled into the unique file. Good. 

colnames(unique)
unique(unique$province_state) #Explore the unique Province or States
unique(unique$country)        #Explore the unique Countries

unique(unique$bin_uri)    #the imported L. externus sequences fall into 3 BINS
sum(is.na(unique$bin_uri)) #note: there are inidv. with missing BIN assignments

missingbinn<-unique %>% 
  filter(is.na(bin_uri))  #explore indiv. with NA for BIN

allbinn<-unique %>% 
  filter(!is.na(bin_uri)) #remove indiv. with NA for BIN

#Create user-friendly label for the 3 BIN assignments from BOLD.
allbinn$BIN <- factor(allbinn$bin_uri, 
                     levels = c("BOLD:ADI6279", "BOLD:AAA2803","BOLD:ADI6278"),
                     labels = c("Bin_A","Bin_B","Bin_C"))

#Create new column with the following identifying info to be used in phylogenetic tree branch label.
Geneious  <- allbinn %>% 
  mutate(concated_column = paste(country, province_state, region, BIN, processid, institution_storing,sep = '_'))

write_csv(Geneious, "data output/bold_data_tidied.csv") #export as CSV


