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


#18-6 MONTH WINDOW PERIOD-------------------------------------------------------
#We can create a new window period without rematching, because it wasn't matched on num_ops anyway!
#Create the new start date
m.data <- m.data %>%
  mutate(
    date_range_start_18mo = episode_date - months(18))

# Calculate num ops in the new range, using the data table method
setDT(m.data)
cols_to_convert <- names(m.data)[startsWith(names(m.data), "f.41282.0.")]
m.data[, (cols_to_convert) := lapply(.SD, as.Date), .SDcols = cols_to_convert]
m.data[, num_ops_18to6mo := rowSums(sapply(.SD, function(x) x >= date_range_start_18mo & x <= date_range_end), na.rm = TRUE), .SDcols = cols_to_convert]

summary(m.data$num_ops_18to6mo)
m.data_DCM <- filter(m.data, DCM == 1)
summary(m.data_DCM$num_ops_18to6mo)

#Calculate op_in_range
m.data <- m.data %>% 
  mutate(op_in_range_18_6 = ifelse(num_ops_18to6mo >= 1, 1, 0)) %>%
  relocate(op_in_range_18_6, .after = num_ops_18to6mo)

#Chi Square on num_ops & op_in_range
table_18_6=with(m.data,table(DCM,num_ops_18to6mo))
table_18_6
chisq.test(table_18_6)
fisher.test(table_18_6)

tab_18_6=with(m.data,table(DCM,op_in_range))
tab_18_6
chisq.test(tab_18_6)
fisher.test(tab_18_6)

#Simple logistic regression
dcm_numops <- m.data %>%
  glm(formula = DCM ~ num_ops_18to6mo, family = "binomial")

effect_plot(dcm_numops, pred = num_ops_18to6mo, plot.points = TRUE,
            jitter = c(0.1, 0.05), point.alpha = 0.1)

predictionDat <- tibble(num_ops_18to6mo = c(0:59))
predictions <- make_predictions(dcm_numops, new_data = predictionDat,
                                outcome.scale = "response")

ggplot(data=predictions, aes(num_ops_18to6mo, DCM)) +
  geom_point(aes(x=num_ops_18to6mo, y=DCM)) +
  geom_errorbar(data=predictions, aes(y=DCM, ymin = ymin, ymax = ymax), color = "red") +
  ylab("Probability of episode being DCM") +
  xlab("Number of operations in 18-6 months prior to episode")

#Logistic regression with matching
fit_18_6 <- glm(DCM ~ num_ops_18to6mo, data = m.data, weights = weights, family = quasibinomial())

avg_comparisons(fit_18_6,
                variables = "num_ops_18to6mo",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights",
                comparison = "lnoravg",
                transform = "exp")

fit_18_6_binary <- glm(DCM ~ op_in_range_18_6, data = m.data, weights = weights, family = quasibinomial())

avg_comparisons(fit_18_6_binary,
                variables = "op_in_range_18_6",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights",
                comparison = "lnoravg",
                transform = "exp")



#NUM GENERAL ANAESTHETICS------------------------------------------------------------------------------
#Filter to OPCS codes corresponding to GA in the OPCS columns. This should be less sensitive but much more specific for our hypothesised causal risk factor 

#Define target OPCS codes
opcs_GA <- c("Y801","Y802","Y803","Y804","Y805","Y806","Y807","Y808","Y809")

#Unique row IDs
m.data[, id := .I]

#Count target OPCS codes
tmp <- melt(m.data, id.vars=c("id", "date_range_start", "date_range_end"), 
            measure.vars=patterns(f.41272 = "^f.41272", f.41282 = "^f.41282")
)[, .(GA_count = sum(f.41272 %in% opcs_GA &
                         between(f.41282, date_range_start, date_range_end, NAbounds = FALSE))), 
  by = .(id, date_range_start, date_range_end)]

m.data[tmp, GA_count := GA_count, on = .(id, date_range_start, date_range_end)]

#Interrogate the result
#Simple chi square
table_GA=with(m.data,table(DCM,GA_count))
table_GA
chisq.test(table_GA)
fisher.test(table_GA)

#Logistic regression
fit_GA_count <- glm(DCM ~ GA_count, data = m.data, weights = weights, family = quasibinomial())
avg_comparisons(fit_GA_count,
                variables = "GA_count",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights",
                comparison = "lnratioavg",
                transform = "exp")

#For this logistic regression you'll need the covariate columns loaded in (see sheet 6)
fit_GA_count <- glm(DCM ~ GA_count * (f.816.0.0 + icd_CHF_count + icd_CVA_count + icd_DM_count + icd_MI_count + icd_PVD_count), data = m.data, weights = weights, family = quasibinomial())
avg_comparisons(fit_GA_count,
                variables = "GA_count",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights",
                comparison = "lnratioavg",
                transform = "exp")



#Binary target OPCS codes
m.data <- m.data %>% 
  mutate(binary_GA = ifelse(GA_count >= 1, 1, 0))

table_GA_binary=with(m.data,table(DCM,binary_GA))
table_GA_binary
chisq.test(table_GA_binary)
fisher.test(table_GA_binary)

fit_GA_binary <- glm(DCM ~ binary_GA, data = m.data, weights = weights, family = quasibinomial())
avg_comparisons(fit_GA_binary,
                variables = "binary_GA",
                vcov = ~subclass,
                newdata = subset(m.data, binary_GA == 1),
                wts = "weights",
                comparison = "lnratioavg",
                transform = "exp")

fit_GA_binary <- glm(DCM ~ binary_GA * (f.816.0.0 + icd_CHF_count + icd_CVA_count + icd_DM_count + icd_MI_count + icd_PVD_count), data = m.data, weights = weights, family = quasibinomial())
avg_comparisons(fit_GA_binary,
                variables = "binary_GA",
                vcov = ~subclass,
                newdata = subset(m.data, binary_GA == 1),
                wts = "weights",
                comparison = "lnratioavg",
                transform = "exp")

#0 vs 1 vs 2+ OPCS codes
m.data <- m.data %>% 
  mutate(categorical_GA = ifelse(m.data$GA_count == 0, 0,
                                           ifelse(m.data$GA_count == 1, 1, 2)))

table_GA_categorical=with(m.data,table(DCM,categorical_GA))
table_GA_categorical
chisq.test(table_GA_categorical)
fisher.test(table_GA_categorical)

fit_GA_categorical <- glm(DCM ~ categorical_GA, data = m.data, weights = weights, family = quasibinomial())
avg_comparisons(fit_GA_categorical,
                variables = "categorical_GA",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights",
                comparison = "lnoravg",
                transform = "exp")

fit_GA_categorical <- glm(DCM ~ categorical_GA * (f.816.0.0 + icd_CHF_count + icd_CVA_count + icd_DM_count + icd_MI_count + icd_PVD_count), data = m.data, weights = weights, family = quasibinomial())
avg_comparisons(fit_GA_categorical,
                variables = "categorical_GA",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights",
                comparison = "lnoravg",
                transform = "exp")

#Are we adequately powered?
cohens_d(op_in_range ~ DCM, data = extracted)



#NUM ANAESTHETIST-ATTENDANCE OPS------------------------------------------------
#This analysis uses previously-defined OPCS codes which are likely to have an anaesthetist in attendance (hence most causally relevant), and compares number of these in the window period prior to DCM and non-DCM episodes.

#Import OPCS codes
opcs_anaesthetist_import <- read_csv("/Users/BenG/Library/CloudStorage/OneDrive-NHS/Documents/1 Med School/Clinical/2 Themes/iM@C/Elective/Biobank Project/R/Raw data/OPCS_anaesthetist/opcs_anaesthetist_filtered.csv", col_names=FALSE)

opcs_anaesthetist <- opcs_anaesthetist_import$X1

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
tmp <- melt(m.data, id.vars=c("id", "date_range_start", "date_range_end"), 
            measure.vars=patterns(f.41272 = "^f.41272", f.41282 = "^f.41282")
)[, .(opcs_anaesthetist_count = sum(f.41272 %in% opcs_anaesthetist &
                         between(f.41282, date_range_start, date_range_end, NAbounds = FALSE))), 
  by = .(id, date_range_start, date_range_end)]
gc()
m.data[tmp, opcs_anaesthetist_count := i.opcs_anaesthetist_count, on = .(id, date_range_start, date_range_end)]
rm(tmp)
gc()

#Interrogate the result
#Simple chi square
table_anaesthetist=with(m.data,table(DCM,opcs_anaesthetist_count))
table_anaesthetist
chisq.test(table_anaesthetist)
fisher.test(table_anaesthetist)

#Logistic regression
fit_opcs_count <- glm(DCM ~ opcs_anaesthetist_count, data = m.data, weights = weights, family = quasibinomial())
avg_comparisons(fit_opcs_count,
                variables = "opcs_anaesthetist_count",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights",
                comparison = "lnoravg",
                transform = "exp")

fit_opcs_count <- glm(DCM ~ opcs_anaesthetist_count * (f.816.0.0 + icd_CHF_count + icd_CVA_count + icd_DM_count + icd_MI_count + icd_PVD_count), data = m.data, weights = weights, family = quasibinomial())
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

fit_opcs_binary <- glm(DCM ~ binary_opcs_anaesthetist, data = m.data, weights = weights, family = quasibinomial())
avg_comparisons(fit_opcs_binary,
                variables = "binary_opcs_anaesthetist",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights",
                comparison = "lnoravg",
                transform = "exp")

fit_opcs_binary <- glm(DCM ~ binary_opcs_anaesthetist * (f.816.0.0 + icd_CHF_count + icd_CVA_count + icd_DM_count + icd_MI_count + icd_PVD_count), data = m.data, weights = weights, family = quasibinomial())
avg_comparisons(fit_opcs_binary,
                variables = "binary_opcs_anaesthetist",
                vcov = ~subclass,
                newdata = "mean",
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

fit_opcs_categorical <- glm(DCM ~ categorical_opcs, data = m.data, weights = weights, family = quasibinomial())
avg_comparisons(fit_opcs_categorical,
                variables = "categorical_opcs",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights",
                comparison = "lnoravg",
                transform = "exp")

fit_opcs_categorical <- glm(DCM ~ categorical_opcs * (f.816.0.0 + icd_CHF_count + icd_CVA_count + icd_DM_count + icd_MI_count + icd_PVD_count), data = m.data, weights = weights, family = quasibinomial())
avg_comparisons(fit_opcs_categorical,
                variables = "categorical_opcs",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights",
                comparison = "lnoravg",
                transform = "exp")

