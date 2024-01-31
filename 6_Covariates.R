#This R file calculates the covariate columns - i.e. occupation & comorbidities

library(tidyverse)
library(data.table)
library(BSDA)
library(plyr)

#COMORBIDITIES----------------------------------------------------------------------------
#Use ICD codes from the Charlson comorbidity index to count number of instances of ICD codes for a particular comorbidity, prior to that episode date
#Do it in sections, because otherwise it crashes
m.data[, id := .I]

gc()
m.data_1 <- m.data[row.names(m.data) %in% 1:228824, ]
m.data_2 <- m.data[row.names(m.data) %in% 228825:457648, ]
m.data_3 <- m.data[row.names(m.data) %in% 457649:686472, ]
m.data_4 <- m.data[row.names(m.data) %in% 686473:915296, ]
m.data_5 <- m.data[row.names(m.data) %in% 915297:1144120, ]
m.data_6 <- m.data[row.names(m.data) %in% 1144121:1372944, ]
m.data_7 <- m.data[row.names(m.data) %in% 1372945:1601768, ]
m.data_8 <- m.data[row.names(m.data) %in% 1601769:1830592, ]
m.data_9 <- m.data[row.names(m.data) %in% 1830593:2059416, ]
m.data_10 <- m.data[row.names(m.data) %in% 2059417:2288238, ]
rm(m.data)
gc()

#CHF-------------
CHF_CCI_ICD_import <- read_csv("/Users/BenG/Library/CloudStorage/OneDrive-NHS/Documents/1 Med School/Clinical/2 Themes/iM@C/Elective/Biobank Project/R/Raw data/CCI_ICD/CHF_CCI_ICD.csv", col_names=TRUE)
CHF_CCI_ICD <- CHF_CCI_ICD_import$icd

#1
gc()
tmp <- melt(m.data_1, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_CHF_count = sum(f.41270 %in% CHF_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_1[tmp, icd_CHF_count := i.icd_CHF_count, on = .(id, episode_date)]

#2
gc()
tmp <- melt(m.data_2, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_CHF_count = sum(f.41270 %in% CHF_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_2[tmp, icd_CHF_count := i.icd_CHF_count, on = .(id, episode_date)]

#3
gc()
tmp <- melt(m.data_3, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_CHF_count = sum(f.41270 %in% CHF_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_3[tmp, icd_CHF_count := i.icd_CHF_count, on = .(id, episode_date)]

#4
gc()
tmp <- melt(m.data_4, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_CHF_count = sum(f.41270 %in% CHF_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_4[tmp, icd_CHF_count := i.icd_CHF_count, on = .(id, episode_date)]

#5
gc()
tmp <- melt(m.data_5, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_CHF_count = sum(f.41270 %in% CHF_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_5[tmp, icd_CHF_count := i.icd_CHF_count, on = .(id, episode_date)]

#6
gc()
tmp <- melt(m.data_6, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_CHF_count = sum(f.41270 %in% CHF_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_6[tmp, icd_CHF_count := i.icd_CHF_count, on = .(id, episode_date)]

#7
gc()
tmp <- melt(m.data_7, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_CHF_count = sum(f.41270 %in% CHF_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_7[tmp, icd_CHF_count := i.icd_CHF_count, on = .(id, episode_date)]

#8
gc()
tmp <- melt(m.data_8, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_CHF_count = sum(f.41270 %in% CHF_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_8[tmp, icd_CHF_count := i.icd_CHF_count, on = .(id, episode_date)]

#9
gc()
tmp <- melt(m.data_9, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_CHF_count = sum(f.41270 %in% CHF_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_9[tmp, icd_CHF_count := i.icd_CHF_count, on = .(id, episode_date)]

#10
gc()
tmp <- melt(m.data_10, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_CHF_count = sum(f.41270 %in% CHF_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_10[tmp, icd_CHF_count := i.icd_CHF_count, on = .(id, episode_date)]

#Merge
gc()
list = list(m.data_1, m.data_2, m.data_3, m.data_4, m.data_5, m.data_6, m.data_7, m.data_8, m.data_9, m.data_10)
rm(list=ls()[! ls() %in% c("list")])
gc()
m.data_merged <- rbindlist(list)
rm(list)
gc()
m.data_CHF <- select(m.data_merged, icd_CHF_count, DCM)
#m.data <- bind_cols(m.data, m.data_CHF)
rm(list=ls()[! ls() %in% c("m.data_CHF")])

#Summarise (and check it's worked)
table_CHF=with(m.data_CHF,table(DCM,icd_CHF_count))
table_CHF
chisq.test(table_CHF)

m.data_CHF <- m.data_CHF %>% 
  mutate(icd_CHF_binary = ifelse(icd_CHF_count >= 1, 1, 0))

table_CHF_binary=with(m.data_CHF,table(DCM,icd_CHF_binary))
table_CHF_binary
chisq.test(table_CHF_binary)
fisher.test(table_CHF_binary)
gc()
save.image("~/Library/CloudStorage/OneDrive-NHS/Documents/1 Med School/Clinical/2 Themes/iM@C/Elective/Biobank Project/R/RData Files/m.data_CHF.RData")

#CVA-------------
CVA_CCI_ICD_import <- read_csv("/Users/BenG/Library/CloudStorage/OneDrive-NHS/Documents/1 Med School/Clinical/2 Themes/iM@C/Elective/Biobank Project/R/Raw data/CCI_ICD/CVA_CCI_ICD.csv", col_names=TRUE)
CVA_CCI_ICD <- CVA_CCI_ICD_import$icd

#1
gc()
tmp <- melt(m.data_1, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_CVA_count = sum(f.41270 %in% CVA_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_1[tmp, icd_CVA_count := i.icd_CVA_count, on = .(id, episode_date)]

#2
gc()
tmp <- melt(m.data_2, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_CVA_count = sum(f.41270 %in% CVA_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_2[tmp, icd_CVA_count := i.icd_CVA_count, on = .(id, episode_date)]

#3
gc()
tmp <- melt(m.data_3, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_CVA_count = sum(f.41270 %in% CVA_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_3[tmp, icd_CVA_count := i.icd_CVA_count, on = .(id, episode_date)]

#4
gc()
tmp <- melt(m.data_4, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_CVA_count = sum(f.41270 %in% CVA_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_4[tmp, icd_CVA_count := i.icd_CVA_count, on = .(id, episode_date)]

#5
gc()
tmp <- melt(m.data_5, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_CVA_count = sum(f.41270 %in% CVA_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_5[tmp, icd_CVA_count := i.icd_CVA_count, on = .(id, episode_date)]

#6
gc()
tmp <- melt(m.data_6, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_CVA_count = sum(f.41270 %in% CVA_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_6[tmp, icd_CVA_count := i.icd_CVA_count, on = .(id, episode_date)]

#7
gc()
tmp <- melt(m.data_7, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_CVA_count = sum(f.41270 %in% CVA_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_7[tmp, icd_CVA_count := i.icd_CVA_count, on = .(id, episode_date)]

#8
gc()
tmp <- melt(m.data_8, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_CVA_count = sum(f.41270 %in% CVA_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_8[tmp, icd_CVA_count := i.icd_CVA_count, on = .(id, episode_date)]

#9
gc()
tmp <- melt(m.data_9, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_CVA_count = sum(f.41270 %in% CVA_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_9[tmp, icd_CVA_count := i.icd_CVA_count, on = .(id, episode_date)]

#10
gc()
tmp <- melt(m.data_10, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_CVA_count = sum(f.41270 %in% CVA_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_10[tmp, icd_CVA_count := i.icd_CVA_count, on = .(id, episode_date)]

#Merge
gc()
list = list(m.data_1, m.data_2, m.data_3, m.data_4, m.data_5, m.data_6, m.data_7, m.data_8, m.data_9, m.data_10)
rm(list=ls()[! ls() %in% c("list")])
gc()
m.data_merged <- rbindlist(list)
rm(list)
gc()
m.data_CVA <- select(m.data_merged, icd_CVA_count, DCM)
#m.data <- bind_cols(m.data, m.data_CVA)
rm(list=ls()[! ls() %in% c("m.data_CVA")])

#Summarise (and check it's worked)
table_CVA=with(m.data_CVA,table(DCM,icd_CVA_count))
table_CVA
chisq.test(table_CVA)

m.data_CVA <- m.data_CVA %>% 
  mutate(icd_CVA_binary = ifelse(icd_CVA_count >= 1, 1, 0))

table_CVA_binary=with(m.data_CVA,table(DCM,icd_CVA_binary))
table_CVA_binary
chisq.test(table_CVA_binary)
fisher.test(table_CVA_binary)
gc()
save.image("~/Library/CloudStorage/OneDrive-NHS/Documents/1 Med School/Clinical/2 Themes/iM@C/Elective/Biobank Project/R/RData Files/m.data_CVA.RData")

#DM-------------
DM_CCI_ICD_import <- read_csv("/Users/BenG/Library/CloudStorage/OneDrive-NHS/Documents/1 Med School/Clinical/2 Themes/iM@C/Elective/Biobank Project/R/Raw data/CCI_ICD/DM_CCI_ICD.csv", col_names=TRUE)
DM_CCI_ICD <- DM_CCI_ICD_import$icd

#1
gc()
tmp <- melt(m.data_1, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_DM_count = sum(f.41270 %in% DM_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_1[tmp, icd_DM_count := i.icd_DM_count, on = .(id, episode_date)]

#2
gc()
tmp <- melt(m.data_2, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_DM_count = sum(f.41270 %in% DM_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_2[tmp, icd_DM_count := i.icd_DM_count, on = .(id, episode_date)]

#3
gc()
tmp <- melt(m.data_3, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_DM_count = sum(f.41270 %in% DM_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_3[tmp, icd_DM_count := i.icd_DM_count, on = .(id, episode_date)]

#4
gc()
tmp <- melt(m.data_4, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_DM_count = sum(f.41270 %in% DM_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_4[tmp, icd_DM_count := i.icd_DM_count, on = .(id, episode_date)]

#5
gc()
tmp <- melt(m.data_5, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_DM_count = sum(f.41270 %in% DM_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_5[tmp, icd_DM_count := i.icd_DM_count, on = .(id, episode_date)]

#6
gc()
tmp <- melt(m.data_6, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_DM_count = sum(f.41270 %in% DM_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_6[tmp, icd_DM_count := i.icd_DM_count, on = .(id, episode_date)]

#7
gc()
tmp <- melt(m.data_7, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_DM_count = sum(f.41270 %in% DM_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_7[tmp, icd_DM_count := i.icd_DM_count, on = .(id, episode_date)]

#8
gc()
tmp <- melt(m.data_8, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_DM_count = sum(f.41270 %in% DM_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_8[tmp, icd_DM_count := i.icd_DM_count, on = .(id, episode_date)]

#9
gc()
tmp <- melt(m.data_9, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_DM_count = sum(f.41270 %in% DM_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_9[tmp, icd_DM_count := i.icd_DM_count, on = .(id, episode_date)]

#10
gc()
tmp <- melt(m.data_10, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_DM_count = sum(f.41270 %in% DM_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_10[tmp, icd_DM_count := i.icd_DM_count, on = .(id, episode_date)]

#Merge
gc()
list = list(m.data_1, m.data_2, m.data_3, m.data_4, m.data_5, m.data_6, m.data_7, m.data_8, m.data_9, m.data_10)
rm(list=ls()[! ls() %in% c("list")])
gc()
m.data_merged <- rbindlist(list)
rm(list)
gc()
m.data_DM <- select(m.data_merged, icd_DM_count, DCM)
#m.data <- bind_cols(m.data, m.data_DM)
rm(list=ls()[! ls() %in% c("m.data_DM")])

#Summarise (and check it's worked)
table_DM=with(m.data_DM,table(DCM,icd_DM_count))
table_DM
chisq.test(table_DM)

m.data_DM <- m.data_DM %>% 
  mutate(icd_DM_binary = ifelse(icd_DM_count >= 1, 1, 0))

table_DM_binary=with(m.data_DM,table(DCM,icd_DM_binary))
table_DM_binary
chisq.test(table_DM_binary)
fisher.test(table_DM_binary)
gc()
save.image("~/Library/CloudStorage/OneDrive-NHS/Documents/1 Med School/Clinical/2 Themes/iM@C/Elective/Biobank Project/R/RData Files/m.data_DM.RData")

#MI-------------
MI_CCI_ICD_import <- read_csv("/Users/BenG/Library/CloudStorage/OneDrive-NHS/Documents/1 Med School/Clinical/2 Themes/iM@C/Elective/Biobank Project/R/Raw data/CCI_ICD/MI_CCI_ICD.csv", col_names=TRUE)
MI_CCI_ICD <- MI_CCI_ICD_import$icd

#1
gc()
tmp <- melt(m.data_1, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_MI_count = sum(f.41270 %in% MI_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_1[tmp, icd_MI_count := i.icd_MI_count, on = .(id, episode_date)]

#2
gc()
tmp <- melt(m.data_2, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_MI_count = sum(f.41270 %in% MI_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_2[tmp, icd_MI_count := i.icd_MI_count, on = .(id, episode_date)]

#3
gc()
tmp <- melt(m.data_3, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_MI_count = sum(f.41270 %in% MI_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_3[tmp, icd_MI_count := i.icd_MI_count, on = .(id, episode_date)]

#4
gc()
tmp <- melt(m.data_4, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_MI_count = sum(f.41270 %in% MI_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_4[tmp, icd_MI_count := i.icd_MI_count, on = .(id, episode_date)]

#5
gc()
tmp <- melt(m.data_5, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_MI_count = sum(f.41270 %in% MI_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_5[tmp, icd_MI_count := i.icd_MI_count, on = .(id, episode_date)]

#6
gc()
tmp <- melt(m.data_6, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_MI_count = sum(f.41270 %in% MI_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_6[tmp, icd_MI_count := i.icd_MI_count, on = .(id, episode_date)]

#7
gc()
tmp <- melt(m.data_7, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_MI_count = sum(f.41270 %in% MI_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_7[tmp, icd_MI_count := i.icd_MI_count, on = .(id, episode_date)]

#8
gc()
tmp <- melt(m.data_8, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_MI_count = sum(f.41270 %in% MI_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_8[tmp, icd_MI_count := i.icd_MI_count, on = .(id, episode_date)]

#9
gc()
tmp <- melt(m.data_9, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_MI_count = sum(f.41270 %in% MI_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_9[tmp, icd_MI_count := i.icd_MI_count, on = .(id, episode_date)]

#10
gc()
tmp <- melt(m.data_10, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_MI_count = sum(f.41270 %in% MI_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_10[tmp, icd_MI_count := i.icd_MI_count, on = .(id, episode_date)]

#Merge
gc()
list = list(m.data_1, m.data_2, m.data_3, m.data_4, m.data_5, m.data_6, m.data_7, m.data_8, m.data_9, m.data_10)
rm(list=ls()[! ls() %in% c("list")])
gc()
m.data_merged <- rbindlist(list)
rm(list)
gc()
m.data_MI <- select(m.data_merged, icd_MI_count, DCM)
#m.data <- bind_cols(m.data, m.data_MI)
rm(list=ls()[! ls() %in% c("m.data_MI")])

#Summarise (and check it's worked)
table_MI=with(m.data_MI,table(DCM,icd_MI_count))
table_MI
chisq.test(table_MI)

m.data_MI <- m.data_MI %>% 
  mutate(icd_MI_binary = ifelse(icd_MI_count >= 1, 1, 0))

table_MI_binary=with(m.data_MI,table(DCM,icd_MI_binary))
table_MI_binary
chisq.test(table_MI_binary)
fisher.test(table_MI_binary)
gc()
save.image("~/Library/CloudStorage/OneDrive-NHS/Documents/1 Med School/Clinical/2 Themes/iM@C/Elective/Biobank Project/R/RData Files/m.data_MI.RData")

#PVD-------------
PVD_CCI_ICD_import <- read_csv("/Users/BenG/Library/CloudStorage/OneDrive-NHS/Documents/1 Med School/Clinical/2 Themes/iM@C/Elective/Biobank Project/R/Raw data/CCI_ICD/PVD_CCI_ICD.csv", col_names=TRUE)
PVD_CCI_ICD <- PVD_CCI_ICD_import$icd

#1
gc()
tmp <- melt(m.data_1, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_PVD_count = sum(f.41270 %in% PVD_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_1[tmp, icd_PVD_count := i.icd_PVD_count, on = .(id, episode_date)]

#2
gc()
tmp <- melt(m.data_2, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_PVD_count = sum(f.41270 %in% PVD_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_2[tmp, icd_PVD_count := i.icd_PVD_count, on = .(id, episode_date)]

#3
gc()
tmp <- melt(m.data_3, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_PVD_count = sum(f.41270 %in% PVD_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_3[tmp, icd_PVD_count := i.icd_PVD_count, on = .(id, episode_date)]

#4
gc()
tmp <- melt(m.data_4, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_PVD_count = sum(f.41270 %in% PVD_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_4[tmp, icd_PVD_count := i.icd_PVD_count, on = .(id, episode_date)]

#5
gc()
tmp <- melt(m.data_5, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_PVD_count = sum(f.41270 %in% PVD_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_5[tmp, icd_PVD_count := i.icd_PVD_count, on = .(id, episode_date)]

#6
gc()
tmp <- melt(m.data_6, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_PVD_count = sum(f.41270 %in% PVD_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_6[tmp, icd_PVD_count := i.icd_PVD_count, on = .(id, episode_date)]

#7
gc()
tmp <- melt(m.data_7, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_PVD_count = sum(f.41270 %in% PVD_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_7[tmp, icd_PVD_count := i.icd_PVD_count, on = .(id, episode_date)]

#8
gc()
tmp <- melt(m.data_8, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_PVD_count = sum(f.41270 %in% PVD_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_8[tmp, icd_PVD_count := i.icd_PVD_count, on = .(id, episode_date)]

#9
gc()
tmp <- melt(m.data_9, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_PVD_count = sum(f.41270 %in% PVD_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_9[tmp, icd_PVD_count := i.icd_PVD_count, on = .(id, episode_date)]

#10
gc()
tmp <- melt(m.data_10, id.vars = c("id", "episode_date"), 
            measure.vars = patterns(f.41270 = "^f.41270", f.41280 = "^f.41280"))[
              , .(icd_PVD_count = sum(f.41270 %in% PVD_CCI_ICD & f.41280 < episode_date)), 
              by = .(id, episode_date)]

m.data_10[tmp, icd_PVD_count := i.icd_PVD_count, on = .(id, episode_date)]

#Merge
gc()
list = list(m.data_1, m.data_2, m.data_3, m.data_4, m.data_5, m.data_6, m.data_7, m.data_8, m.data_9, m.data_10)
rm(list=ls()[! ls() %in% c("list")])
gc()
m.data_merged <- rbindlist(list)
rm(list)
gc()
m.data_PVD <- select(m.data_merged, icd_PVD_count, DCM)
#m.data <- bind_cols(m.data, m.data_PVD)
rm(list=ls()[! ls() %in% c("m.data_PVD")])

#Summarise (and check it's worked)
table_PVD=with(m.data_PVD,table(DCM,icd_PVD_count))
table_PVD
chisq.test(table_PVD)

m.data_PVD <- m.data_PVD %>% 
  mutate(icd_PVD_binary = ifelse(icd_PVD_count >= 1, 1, 0))

table_PVD_binary=with(m.data_PVD,table(DCM,icd_PVD_binary))
table_PVD_binary
chisq.test(table_PVD_binary)
fisher.test(table_PVD_binary)
gc()
save.image("~/Library/CloudStorage/OneDrive-NHS/Documents/1 Med School/Clinical/2 Themes/iM@C/Elective/Biobank Project/R/RData Files/m.data_PVD.RData")

#Merge onto m.data
gc()
m.data_CHF <- select(m.data_CHF, -DCM)
m.data_CVA <- select(m.data_CVA, -DCM)
m.data_DM <- select(m.data_DM, -DCM)
m.data_MI <- select(m.data_MI, -DCM)
m.data_PVD <- select(m.data_PVD, -DCM)
gc()
m.data <- bind_cols(m.data, m.data_CHF)
m.data <- bind_cols(m.data, m.data_CVA)
m.data <- bind_cols(m.data, m.data_DM)
m.data <- bind_cols(m.data, m.data_MI)
m.data <- bind_cols(m.data, m.data_PVD)

rm(list=ls()[! ls() %in% c("m.data")])


#TABLE 1------------------------------------
count(m.data, DCM)
m.data_DCM <- filter(m.data, DCM==1)
m.data_noDCM <- filter(m.data, DCM==0)

#Sex
summary(m.data_DCM$f.31.0.0)
summary(m.data_noDCM$f.31.0.0)

table_sex=with(m.data,table(DCM,f.31.0.0))
chisq.test(table_sex)

#Age at episode
summary(m.data_DCM$approx_age_at_episode)
summary(m.data_noDCM$approx_age_at_episode)

t.test(approx_age_at_episode ~ DCM, data=m.data)

#Date of episode
summary(m.data_DCM$episode_date)
summary(m.data_noDCM$episode_date)
summary(m.data$episode_date)

table_date=with(m.data,table(DCM,episode_date))
chisq.test(table_date)

#Comorbidities
#CHF
summary(m.data_DCM$icd_CHF_binary)
summary(m.data_noDCM$icd_CHF_binary)
table_CHF_binary=with(m.data,table(DCM,icd_CHF_binary))
chisq.test(table_CHF_binary)

#CVA
summary(m.data_DCM$icd_CVA_binary)
summary(m.data_noDCM$icd_CVA_binary)
table_CVA_binary=with(m.data,table(DCM,icd_CVA_binary))
chisq.test(table_CVA_binary)

#DM
summary(m.data_DCM$icd_DM_binary)
summary(m.data_noDCM$icd_DM_binary)
table_DM_binary=with(m.data,table(DCM,icd_DM_binary))
chisq.test(table_DM_binary)

#MI
summary(m.data_DCM$icd_MI_binary)
summary(m.data_noDCM$icd_MI_binary)
table_MI_binary=with(m.data,table(DCM,icd_MI_binary))
chisq.test(table_MI_binary)

#PVD
summary(m.data_DCM$icd_PVD_binary)
summary(m.data_noDCM$icd_PVD_binary)
table_PVD_binary=with(m.data,table(DCM,icd_PVD_binary))
chisq.test(table_PVD_binary)


#Occupation
m.data$f.816.0.0 <- as.integer(m.data$f.816.0.0)
m.data$f.816.0.0[is.na(m.data$f.816.0.0)] = 0
t.test(f.816.0.0 ~ DCM, data=m.data)

m.data$f.816.0.0 <- as.factor(m.data$f.816.0.0)
m.data$f.816.0.0 <- revalue(m.data$f.816.0.0, c("-3"="Prefer not to answer", "-1"="Do not know", "0" = "Retired", "1" = "Never/rarely", "2" = "Sometimes", "3" = "Usually", "4" = "Always"))

summary(m.data$f.816.0.0)

table_occupation=with(m.data,table(DCM,f.816.0.0))
table_occupation
chisq.test(table_occupation)

#Smoking
summary(m.data_DCM$f.20161.0.0)
summary(m.data_noDCM$f.20161.0.0)

t.test(f.20161.0.0 ~ DCM, data=m.data)

#BMI
summary(m.data_DCM$f.21001.0.0)
summary(m.data_noDCM$f.21001.0.0)

table_BMI=with(m.data,table(DCM,f.21001.0.0))
chisq.test(table_BMI)

t.test(f.21001.0.0 ~ DCM, data=m.data)

#Number of operations
#All
t.test(num_operations_within_range ~ DCM, data=m.data)

#GA
t.test(GA_count ~ DCM, data=m.data)

#Anaesthetist
summary(m.data_DCM$opcs_anaesthetist_count)
summary(m.data_noDCM$opcs_anaesthetist_count)

table_anaesthetist=with(m.data,table(DCM,opcs_anaesthetist_count))
chisq.test(table_anaesthetist)

t.test(opcs_anaesthetist_count ~ DCM, data=m.data)
