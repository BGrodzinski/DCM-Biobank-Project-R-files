#Supplementary analyses
library(tidyverse)
library(cowplot)
library(gghighlight)
library(stringr)
library(pwr)
library(dplyr)
library(lubridate)
library(data.table)

#library(CGEN)
library(MatchIt)
library(marginaleffects)
library(jtools)
library(effectsize)
load("~/Library/CloudStorage/OneDrive-NHS/Documents/1 Med School/Clinical/2 Themes/iM@C/Elective/Biobank Project/R/RData Files/m.data_covariates.RData")

#18-6 MONTH WINDOW PERIOD-----------------------
#With OPCS_Anaes_filtered, comorbidities, and recategorised occupation

#Import OPCS codes
opcs_anaesthetist_filtered <- read.table("~/Library/CloudStorage/OneDrive-NHS/Documents/1 Med School/Clinical/2 Themes/iM@C/Elective/Biobank Project/R/Raw data/OPCS_anaesthetist/opcs_anaesthetist_filtered.csv", quote="\"", comment.char="")

opcs_anaesthetist <- opcs_anaesthetist_filtered$V1

#Truncate codes in f.41272 columns, because the opcs_anaesthetist list is only 2 digits, cf. 3 in the columns
for (x in 0:123) {
  col_name <- paste0("f.41272.0.", x)
  
  m.data[[col_name]] <- as.character(m.data[[col_name]])
  
  m.data <- m.data %>% 
    mutate(!!col_name := str_trunc(!!sym(col_name), 3, "right", ellipsis = ""))
}


#Unique row IDs
m.data[, id := .I]

#Count target OPCS codes
gc()

m.data <- m.data %>%
  mutate(
    date_range_start_18mo = episode_date - months(18))
gc()
tmp <- melt(m.data, id.vars=c("id", "date_range_start_18mo", "date_range_end"), 
            measure.vars=patterns(f.41272 = "^f.41272", f.41282 = "^f.41282")
)[, .(opcs_anaesthetist_count = sum(f.41272 %in% opcs_anaesthetist &
                                      between(f.41282, date_range_start_18mo, date_range_end, NAbounds = FALSE))), 
  by = .(id, date_range_start_18mo, date_range_end)]
gc()
m.data[tmp, opcs_anaesthetist_count := i.opcs_anaesthetist_count, on = .(id, date_range_start_18mo, date_range_end)]
rm(tmp)
gc()

#Interrogate the result
#Means
t.test(opcs_anaesthetist_count ~ DCM, data = m.data, var.equal = FALSE)


#Logistic regression
fit_opcs_count <- glm(DCM ~ opcs_anaesthetist_count * (f.816.0.0 + icd_CHF_count + icd_CVA_count + icd_DM_count + icd_MI_count + icd_PVD_count), data = m.data, weights = weights, family = quasibinomial())
gc()
avg_comparisons(fit_opcs_count,
                variables = "opcs_anaesthetist_count",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights",
                comparison = "lnoravg",
                transform = "exp")


#Binary target OPCS codes
m.data <- m.data %>% 
  mutate(binary_opcs_anaesthetist = ifelse(opcs_anaesthetist_count >= 1, 1, 0))

table_opcs_binary=with(m.data,table(DCM,binary_opcs_anaesthetist))
table_opcs_binary
chisq.test(table_opcs_binary)
fisher.test(table_opcs_binary)

fit_opcs_binary <- glm(DCM ~ binary_opcs_anaesthetist * (f.816.0.0 + icd_CHF_count + icd_CVA_count + icd_DM_count + icd_MI_count + icd_PVD_count), data = m.data, weights = weights, family = quasibinomial())
gc()
avg_comparisons(fit_opcs_binary,
                variables = "binary_opcs_anaesthetist",
                vcov = ~subclass,
                newdata = subset(m.data, binary_opcs_anaesthetist == 1),
                wts = "weights",
                comparison = "lnoravg",
                transform = "exp")

#0 vs 1 vs 2+ OPCS codes
m.data <- m.data %>% 
  mutate(categorical_opcs = ifelse(m.data$opcs_anaesthetist_count == 0, 0,
                                   ifelse(m.data$opcs_anaesthetist_count == 1, 1, 2)))

table_opcs_categorical=with(m.data,table(DCM,categorical_opcs))
table_opcs_categorical
chisq.test(table_opcs_categorical)


fit_opcs_categorical <- glm(DCM ~ categorical_opcs * (f.816.0.0 + icd_CHF_count + icd_CVA_count + icd_DM_count + icd_MI_count + icd_PVD_count), data = m.data, weights = weights, family = quasibinomial())
avg_comparisons(fit_opcs_categorical,
                variables = "categorical_opcs",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights",
                comparison = "lnoravg",
                transform = "exp")


#NON-SUPINE POSITIONING---------------------------------
#Define target OPCS codes
opcs_nonsupine <- c("W37","W38","W39","W42","W93","W94","W95","W96","W97","W98","W49","W50","W51","Z54", "Z68","Z81")

#Unique row IDs
m.data[, id := .I]

#Truncate codes in f.41272 columns, because the non_supine list is only 2 digits, cf. 3 in the columns
for (x in 0:123) {
  col_name <- paste0("f.41272.0.", x)
  
  m.data[[col_name]] <- as.character(m.data[[col_name]])
  
  m.data <- m.data %>% 
    mutate(!!col_name := str_trunc(!!sym(col_name), 3, "right", ellipsis = ""))
}

#Count target OPCS codes
gc()
tmp <- melt(m.data, id.vars=c("id", "date_range_start", "date_range_end"), 
            measure.vars=patterns(f.41272 = "^f.41272", f.41282 = "^f.41282")
)[, .(opcs_nonsupine_count = sum(f.41272 %in% opcs_nonsupine &
                                      between(f.41282, date_range_start, date_range_end))), 
  by = .(id, date_range_start, date_range_end)]
gc()
m.data[tmp, opcs_nonsupine_count := i.opcs_nonsupine_count, on = .(id, date_range_start, date_range_end)]
rm(tmp)
gc()

#Interrogate the result
#Simple chi square
t.test(opcs_nonsupine_count ~ DCM, data=m.data)

#Logistic regression - count
fit_nonsupine_count <- glm(DCM ~ opcs_nonsupine_count * (f.816.0.0 + icd_CHF_count + icd_CVA_count + icd_DM_count + icd_MI_count + icd_PVD_count), data = m.data, weights = weights, family = quasibinomial())
avg_comparisons(fit_nonsupine_count,
                variables = "opcs_nonsupine_count",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights",
                comparison = "lnratioavg",
                transform = "exp")



#Binary target OPCS codes
m.data <- m.data %>% 
  mutate(binary_nonsupine = ifelse(opcs_nonsupine_count >= 1, 1, 0))

table_nonsupine_binary=with(m.data,table(DCM,binary_nonsupine))
table_nonsupine_binary
chisq.test(table_nonsupine_binary)
fisher.test(table_nonsupine_binary)

fit_nonsupine_binary <- glm(DCM ~ binary_nonsupine * (f.816.0.0 + icd_CHF_count + icd_CVA_count + icd_DM_count + icd_MI_count + icd_PVD_count), data = m.data, weights = weights, family = quasibinomial())
avg_comparisons(fit_nonsupine_binary,
                variables = "binary_nonsupine",
                vcov = ~subclass,
                newdata = subset(m.data, binary_nonsupine == 1),
                wts = "weights",
                comparison = "lnratioavg",
                transform = "exp")

#Categorical
m.data <- m.data %>% 
  mutate(categorical_nonsupine = ifelse(m.data$opcs_nonsupine_count == 0, 0,
                                 ifelse(m.data$opcs_nonsupine_count == 1, 1, 2)))

table_nonsupine_categorical=with(m.data,table(DCM,categorical_nonsupine))
table_nonsupine_categorical
chisq.test(table_nonsupine_categorical)

fit_nonsupine_categorical <- glm(DCM ~ categorical_nonsupine * (f.816.0.0 + icd_CHF_count + icd_CVA_count + icd_DM_count + icd_MI_count + icd_PVD_count), data = m.data, weights = weights, family = quasibinomial())
avg_comparisons(fit_nonsupine_categorical,
                variables = "categorical_nonsupine",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights",
                comparison = "lnoravg",
                transform = "exp")


#NON-SPINAL OPERATIONS----------------------------------------
opcs_anaesthetist_filtered_nonspinal <- read.table("~/Library/CloudStorage/OneDrive-NHS/Documents/1 Med School/Clinical/2 Themes/iM@C/Elective/Biobank Project/R/Raw data/OPCS_anaesthetist/opcs_anaesthetist_filtered_nospinal.csv", quote="\"", comment.char="")
opcs_nonspinal <- opcs_anaesthetist_filtered_nonspinal$V1

#Unique row IDs
m.data[, id := .I]

#Truncate codes in f.41272 columns, because the non_supine list is only 2 digits, cf. 3 in the columns
for (x in 0:123) {
  col_name <- paste0("f.41272.0.", x)
  
  m.data[[col_name]] <- as.character(m.data[[col_name]])
  
  m.data <- m.data %>% 
    mutate(!!col_name := str_trunc(!!sym(col_name), 3, "right", ellipsis = ""))
}

#Count target OPCS codes
gc()
tmp <- melt(m.data, id.vars=c("id", "date_range_start", "date_range_end"), 
            measure.vars=patterns(f.41272 = "^f.41272", f.41282 = "^f.41282")
)[, .(opcs_nonspinal_count = sum(f.41272 %in% opcs_nonspinal &
                                   between(f.41282, date_range_start, date_range_end, NAbounds = FALSE))), 
  by = .(id, date_range_start, date_range_end)]
gc()
m.data[tmp, opcs_nonspinal_count := i.opcs_nonspinal_count, on = .(id, date_range_start, date_range_end)]
rm(tmp)
gc()

#Interrogate the result
#Simple t test
t.test(opcs_nonspinal_count ~ DCM, data=m.data)

#Logistic regression - count
fit_nonspinal_count <- glm(DCM ~ opcs_nonspinal_count * (f.816.0.0 + icd_CHF_count + icd_CVA_count + icd_DM_count + icd_MI_count + icd_PVD_count), data = m.data, weights = weights, family = quasibinomial())
avg_comparisons(fit_nonspinal_count,
                variables = "opcs_nonspinal_count",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights",
                comparison = "lnratioavg",
                transform = "exp")



#Binary target OPCS codes
m.data <- m.data %>% 
  mutate(binary_nonspinal = ifelse(opcs_nonspinal_count >= 1, 1, 0))

table_nonspinal_binary=with(m.data,table(DCM,binary_nonspinal))
table_nonspinal_binary
chisq.test(table_nonspinal_binary)
fisher.test(table_nonspinal_binary)

fit_nonspinal_binary <- glm(DCM ~ binary_nonspinal * (f.816.0.0 + icd_CHF_count + icd_CVA_count + icd_DM_count + icd_MI_count + icd_PVD_count), data = m.data, weights = weights, family = quasibinomial())
avg_comparisons(fit_nonspinal_binary,
                variables = "binary_nonspinal",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights",
                comparison = "lnratioavg",
                transform = "exp")

#Categorical
m.data <- m.data %>% 
  mutate(categorical_nonspinal = ifelse(m.data$opcs_nonspinal_count == 0, 0,
                                        ifelse(m.data$opcs_nonspinal_count == 1, 1, 2)))

table_nonspinal_categorical=with(m.data,table(DCM,categorical_nonspinal))
table_nonspinal_categorical
chisq.test(table_nonspinal_categorical)


fit_nonspinal_categorical <- glm(DCM ~ categorical_nonspinal * (f.816.0.0 + icd_CHF_count + icd_CVA_count + icd_DM_count + icd_MI_count + icd_PVD_count), data = m.data, weights = weights, family = quasibinomial())
avg_comparisons(fit_nonspinal_categorical,
                variables = "categorical_nonspinal",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights",
                comparison = "lnoravg",
                transform = "exp")


#SPINAL ONLY----------------------------------
opcs_anaesthetist_filtered_onlyspinal <- read.table("~/Library/CloudStorage/OneDrive-NHS/Documents/1 Med School/Clinical/2 Themes/iM@C/Elective/Biobank Project/R/Raw data/OPCS_anaesthetist/opcs_anaesthetist_filtered_onlyspinal.csv", quote="\"", comment.char="")
opcs_onlyspinal <- opcs_anaesthetist_filtered_onlyspinal$V1

#Unique row IDs
m.data[, id := .I]

#Truncate codes in f.41272 columns, because the only_spinal list is only 2 digits, cf. 3 in the columns
for (x in 0:123) {
  col_name <- paste0("f.41272.0.", x)
  
  m.data[[col_name]] <- as.character(m.data[[col_name]])
  
  m.data <- m.data %>% 
    mutate(!!col_name := str_trunc(!!sym(col_name), 3, "right", ellipsis = ""))
}

#Count target OPCS codes
gc()
tmp <- melt(m.data, id.vars=c("id", "date_range_start", "date_range_end"), 
            measure.vars=patterns(f.41272 = "^f.41272", f.41282 = "^f.41282")
)[, .(opcs_onlyspinal_count = sum(f.41272 %in% opcs_onlyspinal &
                                   between(f.41282, date_range_start, date_range_end))), 
  by = .(id, date_range_start, date_range_end)]
gc()
m.data[tmp, opcs_onlyspinal_count := i.opcs_onlyspinal_count, on = .(id, date_range_start, date_range_end)]
rm(tmp)
gc()

#Interrogate the result
#Simple t test
t.test(opcs_onlyspinal_count ~ DCM, data=m.data)

#Logistic regression - count
fit_onlyspinal_count <- glm(DCM ~ opcs_onlyspinal_count * (f.816.0.0 + icd_CHF_count + icd_CVA_count + icd_DM_count + icd_MI_count + icd_PVD_count), data = m.data, weights = weights, family = quasibinomial())
avg_comparisons(fit_onlyspinal_count,
                variables = "opcs_onlyspinal_count",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights",
                comparison = "lnratioavg",
                transform = "exp")



#Binary target OPCS codes
m.data <- m.data %>% 
  mutate(binary_onlyspinal = ifelse(opcs_onlyspinal_count >= 1, 1, 0))

table_onlyspinal_binary=with(m.data,table(DCM,binary_onlyspinal))
table_onlyspinal_binary
chisq.test(table_onlyspinal_binary)
fisher.test(table_onlyspinal_binary)

fit_onlyspinal_binary <- glm(DCM ~ binary_onlyspinal * (f.816.0.0 + icd_CHF_count + icd_CVA_count + icd_DM_count + icd_MI_count + icd_PVD_count), data = m.data, weights = weights, family = quasibinomial())
avg_comparisons(fit_onlyspinal_binary,
                variables = "binary_onlyspinal",
                vcov = ~subclass,
                newdata = subset(m.data, binary_onlyspinal == 1),
                wts = "weights",
                comparison = "lnratioavg",
                transform = "exp")

#Categorical
m.data <- m.data %>% 
  mutate(categorical_onlyspinal = ifelse(m.data$opcs_onlyspinal_count == 0, 0,
                                        ifelse(m.data$opcs_onlyspinal_count == 1, 1, 2)))

table_onlyspinal_categorical=with(m.data,table(DCM,categorical_onlyspinal))
table_onlyspinal_categorical
chisq.test(table_onlyspinal_categorical)


fit_onlyspinal_categorical <- glm(DCM ~ categorical_onlyspinal * (f.816.0.0 + icd_CHF_count + icd_CVA_count + icd_DM_count + icd_MI_count + icd_PVD_count), data = m.data, weights = weights, family = quasibinomial())
avg_comparisons(fit_onlyspinal_categorical,
                variables = "categorical_onlyspinal",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights",
                comparison = "lnoravg",
                transform = "exp")
