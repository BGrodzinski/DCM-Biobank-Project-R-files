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

#EPISODE = ROW TABLE---------------------------------------------------------------------------
#Merge Code and date columns for each episode in each patient
bd_merged <- bd %>%
  unite(col = "episode_code_date_0", "f.41202.0.0", "f.41262.0.0", sep = "_") %>%
  unite(col = "episode_code_date_1", "f.41202.0.1", "f.41262.0.1", sep = "_") %>%
  unite(col = "episode_code_date_2", "f.41202.0.2", "f.41262.0.2", sep = "_") %>%
  unite(col = "episode_code_date_3", "f.41202.0.3", "f.41262.0.3", sep = "_") %>%
  unite(col = "episode_code_date_4", "f.41202.0.4", "f.41262.0.4", sep = "_") %>%
  unite(col = "episode_code_date_5", "f.41202.0.5", "f.41262.0.5", sep = "_") %>%
  unite(col = "episode_code_date_6", "f.41202.0.6", "f.41262.0.6", sep = "_") %>%
  unite(col = "episode_code_date_7", "f.41202.0.7", "f.41262.0.7", sep = "_") %>%
  unite(col = "episode_code_date_8", "f.41202.0.8", "f.41262.0.8", sep = "_") %>%
  unite(col = "episode_code_date_9", "f.41202.0.9", "f.41262.0.9", sep = "_") %>%
  unite(col = "episode_code_date_10", "f.41202.0.10", "f.41262.0.10", sep = "_") %>%
  unite(col = "episode_code_date_11", "f.41202.0.11", "f.41262.0.11", sep = "_") %>%
  unite(col = "episode_code_date_12", "f.41202.0.12", "f.41262.0.12", sep = "_") %>%
  unite(col = "episode_code_date_13", "f.41202.0.13", "f.41262.0.13", sep = "_") %>%
  unite(col = "episode_code_date_14", "f.41202.0.14", "f.41262.0.14", sep = "_") %>%
  unite(col = "episode_code_date_15", "f.41202.0.15", "f.41262.0.15", sep = "_") %>%
  unite(col = "episode_code_date_16", "f.41202.0.16", "f.41262.0.16", sep = "_") %>%
  unite(col = "episode_code_date_17", "f.41202.0.17", "f.41262.0.17", sep = "_") %>%
  unite(col = "episode_code_date_18", "f.41202.0.18", "f.41262.0.18", sep = "_") %>%
  unite(col = "episode_code_date_19", "f.41202.0.19", "f.41262.0.19", sep = "_") %>%
  unite(col = "episode_code_date_20", "f.41202.0.20", "f.41262.0.20", sep = "_") %>%
  unite(col = "episode_code_date_21", "f.41202.0.21", "f.41262.0.21", sep = "_") %>%
  unite(col = "episode_code_date_22", "f.41202.0.22", "f.41262.0.22", sep = "_") %>%
  unite(col = "episode_code_date_23", "f.41202.0.23", "f.41262.0.23", sep = "_") %>%
  unite(col = "episode_code_date_24", "f.41202.0.24", "f.41262.0.24", sep = "_") %>%
  unite(col = "episode_code_date_25", "f.41202.0.25", "f.41262.0.25", sep = "_") %>%
  unite(col = "episode_code_date_26", "f.41202.0.26", "f.41262.0.26", sep = "_") %>%
  unite(col = "episode_code_date_27", "f.41202.0.27", "f.41262.0.27", sep = "_") %>%
  unite(col = "episode_code_date_28", "f.41202.0.28", "f.41262.0.28", sep = "_") %>%
  unite(col = "episode_code_date_29", "f.41202.0.29", "f.41262.0.29", sep = "_") %>%
  unite(col = "episode_code_date_30", "f.41202.0.30", "f.41262.0.30", sep = "_") %>%
  unite(col = "episode_code_date_31", "f.41202.0.31", "f.41262.0.31", sep = "_") %>%
  unite(col = "episode_code_date_32", "f.41202.0.32", "f.41262.0.32", sep = "_") %>%
  unite(col = "episode_code_date_33", "f.41202.0.33", "f.41262.0.33", sep = "_") %>%
  unite(col = "episode_code_date_34", "f.41202.0.34", "f.41262.0.34", sep = "_") %>%
  unite(col = "episode_code_date_35", "f.41202.0.35", "f.41262.0.35", sep = "_") %>%
  unite(col = "episode_code_date_36", "f.41202.0.36", "f.41262.0.36", sep = "_") %>%
  unite(col = "episode_code_date_37", "f.41202.0.37", "f.41262.0.37", sep = "_") %>%
  unite(col = "episode_code_date_38", "f.41202.0.38", "f.41262.0.38", sep = "_") %>%
  unite(col = "episode_code_date_39", "f.41202.0.39", "f.41262.0.39", sep = "_") %>%
  unite(col = "episode_code_date_40", "f.41202.0.40", "f.41262.0.40", sep = "_") %>%
  unite(col = "episode_code_date_41", "f.41202.0.41", "f.41262.0.41", sep = "_") %>%
  unite(col = "episode_code_date_42", "f.41202.0.42", "f.41262.0.42", sep = "_") %>%
  unite(col = "episode_code_date_43", "f.41202.0.43", "f.41262.0.43", sep = "_") %>%
  unite(col = "episode_code_date_44", "f.41202.0.44", "f.41262.0.44", sep = "_") %>%
  unite(col = "episode_code_date_45", "f.41202.0.45", "f.41262.0.45", sep = "_") %>%
  unite(col = "episode_code_date_46", "f.41202.0.46", "f.41262.0.46", sep = "_") %>%
  unite(col = "episode_code_date_47", "f.41202.0.47", "f.41262.0.47", sep = "_") %>%
  unite(col = "episode_code_date_48", "f.41202.0.48", "f.41262.0.48", sep = "_") %>%
  unite(col = "episode_code_date_49", "f.41202.0.49", "f.41262.0.49", sep = "_") %>%
  unite(col = "episode_code_date_50", "f.41202.0.50", "f.41262.0.50", sep = "_") %>%
  unite(col = "episode_code_date_51", "f.41202.0.51", "f.41262.0.51", sep = "_") %>%
  unite(col = "episode_code_date_52", "f.41202.0.52", "f.41262.0.52", sep = "_") %>%
  unite(col = "episode_code_date_53", "f.41202.0.53", "f.41262.0.53", sep = "_") %>%
  unite(col = "episode_code_date_54", "f.41202.0.54", "f.41262.0.54", sep = "_") %>%
  unite(col = "episode_code_date_55", "f.41202.0.55", "f.41262.0.55", sep = "_") %>%
  unite(col = "episode_code_date_56", "f.41202.0.56", "f.41262.0.56", sep = "_") %>%
  unite(col = "episode_code_date_57", "f.41202.0.57", "f.41262.0.57", sep = "_") %>%
  unite(col = "episode_code_date_58", "f.41202.0.58", "f.41262.0.58", sep = "_") %>%
  unite(col = "episode_code_date_59", "f.41202.0.59", "f.41262.0.59", sep = "_") %>%
  unite(col = "episode_code_date_60", "f.41202.0.60", "f.41262.0.60", sep = "_") %>%
  unite(col = "episode_code_date_61", "f.41202.0.61", "f.41262.0.61", sep = "_") %>%
  unite(col = "episode_code_date_62", "f.41202.0.62", "f.41262.0.62", sep = "_") %>%
  unite(col = "episode_code_date_63", "f.41202.0.63", "f.41262.0.63", sep = "_") %>%
  unite(col = "episode_code_date_64", "f.41202.0.64", "f.41262.0.64", sep = "_") %>%
  unite(col = "episode_code_date_65", "f.41202.0.65", "f.41262.0.65", sep = "_") %>%
  unite(col = "episode_code_date_66", "f.41202.0.66", "f.41262.0.66", sep = "_") %>%
  unite(col = "episode_code_date_67", "f.41202.0.67", "f.41262.0.67", sep = "_") %>%
  unite(col = "episode_code_date_68", "f.41202.0.68", "f.41262.0.68", sep = "_") %>%
  unite(col = "episode_code_date_69", "f.41202.0.69", "f.41262.0.69", sep = "_") %>%
  unite(col = "episode_code_date_70", "f.41202.0.70", "f.41262.0.70", sep = "_") %>%
  unite(col = "episode_code_date_71", "f.41202.0.71", "f.41262.0.71", sep = "_") %>%
  unite(col = "episode_code_date_72", "f.41202.0.72", "f.41262.0.72", sep = "_") %>%
  unite(col = "episode_code_date_73", "f.41202.0.73", "f.41262.0.73", sep = "_") %>%
  unite(col = "episode_code_date_74", "f.41202.0.74", "f.41262.0.74", sep = "_") %>%
  unite(col = "episode_code_date_75", "f.41202.0.75", "f.41262.0.75", sep = "_") %>%
  unite(col = "episode_code_date_76", "f.41202.0.76", "f.41262.0.76", sep = "_") %>%
  unite(col = "episode_code_date_77", "f.41202.0.77", "f.41262.0.77", sep = "_") %>%
  unite(col = "episode_code_date_78", "f.41202.0.78", "f.41262.0.78", sep = "_") 
  


#First split the dataset into 10 groups, to allow easier processing
bd_split_1 <- bd_merged[row.names(bd_merged) %in% 1:50000, ]
bd_split_2 <- bd_merged[row.names(bd_merged) %in% 50001:100000, ]
bd_split_3 <- bd_merged[row.names(bd_merged) %in% 100001:150000, ]
bd_split_4 <- bd_merged[row.names(bd_merged) %in% 150001:200000, ]
bd_split_5 <- bd_merged[row.names(bd_merged) %in% 200001:250000, ]
bd_split_6 <- bd_merged[row.names(bd_merged) %in% 250001:300000, ]
bd_split_7 <- bd_merged[row.names(bd_merged) %in% 300001:350000, ]
bd_split_8 <- bd_merged[row.names(bd_merged) %in% 350001:400000, ]
bd_split_9 <- bd_merged[row.names(bd_merged) %in% 400001:450000, ]
bd_split_10 <- bd_merged[row.names(bd_merged) %in% 450001:502369, ]

rm(bd_merged)
gc()
#Within the groups, pivot longer on the episode date columns, and delete the NA rows before moving onto the next group
#(many participants have only a few HES episodes but it goes up to 78 rows for all of them! The above structure saves memory.)
bd_longer_1 <- bd_split_1 %>% pivot_longer(cols=starts_with("episode_code_date"), names_to="episode", values_to="code_date")
bd_longer_1 <- bd_longer_1[!(bd_longer_1$code_date=="NA_NA"),]
gc()
bd_longer_2 <- bd_split_2 %>% pivot_longer(cols=starts_with("episode_code_date"), names_to="episode", values_to="code_date")
bd_longer_2 <- bd_longer_2[!(bd_longer_2$code_date=="NA_NA"),]
gc()
bd_longer_3 <- bd_split_3 %>% pivot_longer(cols=starts_with("episode_code_date"), names_to="episode", values_to="code_date")
bd_longer_3 <- bd_longer_3[!(bd_longer_3$code_date=="NA_NA"),]
gc()
bd_longer_4 <- bd_split_4 %>% pivot_longer(cols=starts_with("episode_code_date"), names_to="episode", values_to="code_date")
bd_longer_4 <- bd_longer_4[!(bd_longer_4$code_date=="NA_NA"),]
gc()
bd_longer_5 <- bd_split_5 %>% pivot_longer(cols=starts_with("episode_code_date"), names_to="episode", values_to="code_date")
bd_longer_5 <- bd_longer_5[!(bd_longer_5$code_date=="NA_NA"),]
gc()
bd_longer_6 <- bd_split_6 %>% pivot_longer(cols=starts_with("episode_code_date"), names_to="episode", values_to="code_date")
bd_longer_6 <- bd_longer_6[!(bd_longer_6$code_date=="NA_NA"),]
gc()
bd_longer_7 <- bd_split_7 %>% pivot_longer(cols=starts_with("episode_code_date"), names_to="episode", values_to="code_date")
bd_longer_7 <- bd_longer_7[!(bd_longer_7$code_date=="NA_NA"),]
gc()
bd_longer_8 <- bd_split_8 %>% pivot_longer(cols=starts_with("episode_code_date"), names_to="episode", values_to="code_date")
bd_longer_8 <- bd_longer_8[!(bd_longer_8$code_date=="NA_NA"),]
gc()
bd_longer_9 <- bd_split_9 %>% pivot_longer(cols=starts_with("episode_code_date"), names_to="episode", values_to="code_date")
bd_longer_9 <- bd_longer_9[!(bd_longer_9$code_date=="NA_NA"),]
gc()
bd_longer_10 <- bd_split_10 %>% pivot_longer(cols=starts_with("episode_code_date"), names_to="episode", values_to="code_date")
bd_longer_10 <- bd_longer_10[!(bd_longer_10$code_date=="NA_NA"),]
gc()

#Merge the bd_longer table, discard the temporary split versions
bd_longer <- bind_rows(bd_longer_1,bd_longer_2,bd_longer_3,bd_longer_4,bd_longer_5,bd_longer_6,bd_longer_7,bd_longer_8,bd_longer_9,bd_longer_10)
rm(bd_longer_1,bd_longer_2,bd_longer_3,bd_longer_4,bd_longer_5,bd_longer_6,bd_longer_7,bd_longer_8,bd_longer_9,bd_longer_10)
rm(bd_split_1, bd_split_2, bd_split_3, bd_split_4, bd_split_5, bd_split_6, bd_split_7, bd_split_8, bd_split_9, bd_split_10)  

#Separate the code_date column into code and date
bd_longer <- separate(bd_longer, code_date, sep = "_", into=c("code", "date"))
gc()

#CREATE COLUMNS FOR MATCHING-----------------------------------------------------------------
#Is the episode DCM?
bd_longer <-  mutate(bd_longer, DCM = case_when(
  code == "M500" | code == "M4712"| code == "M9931" | code == "M9941"| code == "M9951" ~ "Y"))
count(bd_longer, DCM)

#Create a new dataset excluding the other hospital episodes of DCM patients (so they cannot be their own match)
dcm_episodes <- bd_longer %>% filter(DCM == "Y")
dcm_eids <- unique(dcm_episodes$f.eid)
never_dcm_episodes <- bd_longer %>% filter(!(f.eid %in% dcm_eids))

bd_matching<- bind_rows(dcm_episodes, never_dcm_episodes)
count(bd_matching, DCM)
rm(bd_longer)
gc()
#Calculate age at episode
bd_matching <- separate(bd_matching, date, sep = "-", into = c("episode_year", "episode_month", "episode_day"))
bd_matching$episode_year <- as.numeric(bd_matching$episode_year)
bd_matching <- mutate(bd_matching, approx_age_at_episode = episode_year - f.34.0.0)
summary(bd_matching$approx_age_at_episode)

#New column for year_month of episode (for matching)
bd_matching <- bd_matching %>%
  unite(col = "episode_year_month", "episode_year", "episode_month", sep = "_", remove=FALSE)

#New column for date of episode (for op_counts later)
bd_matching <- bd_matching %>%
  unite(col = "episode_date", "episode_year", "episode_month", "episode_day", sep = "-", remove=FALSE)

#Turn the DCM column into a binary variable, drop NA rows
bd_matching$DCM[is.na(bd_matching$DCM)] <- 0
bd_matching$DCM[bd_matching$DCM == 'Y'] <- 1
bd_matching$DCM <- as.integer(bd_matching$DCM)
bd_matching <- drop_na(bd_matching,approx_age_at_episode)

#UNIQUE DCM EIDS------------------------------------------------------------
#Count DCM episodes (833)
count(bd_matching, DCM)
# Count the number of unique values in the "f.eid" column (806)
unique_DCM_count <-bd_matching %>%
  filter(DCM==1)%>%
  summarise(unique_values = n_distinct(f.eid))
print(unique_DCM_count$unique_values)

#Filter to unique DCM eids, dropping the later ones
bd_matching$episode_date <- as.Date(bd_matching$episode_date)
bd_matching_unique_DCM <- bd_matching %>%
  filter(DCM==1) %>%
  group_by(f.eid) %>%
  filter(episode_date == min(episode_date)) %>%
  ungroup()
#Add the non-DCM unique episodes back in
non_dcm_episodes <- bd_matching %>% filter(DCM==0)
bd_matching_V2<- bind_rows(bd_matching_unique_DCM, non_dcm_episodes)
count(bd_matching_V2, DCM)

#RECENT OPERATIONS----------------------------------------------
#Create columns for the range of dates we are looking for an operation in (i.e. 6 months to 2 years preceding episode)
bd_matching_V2 <- bd_matching_V2 %>%
  mutate(
    date_range_start = episode_date - years(2),
    date_range_end = episode_date - months(6),
  )

#TIDYVERSE METHOD (doesn't work)
#Ensure the columns are in date format
#bd_matching_V3 <- bd_matching_V3 %>% mutate(across(starts_with("f.41282.0."), ~ as.Date(.)))

#Create column which calculates, for each row, how many operations within the date range
#bd_split_1 <- bd_matching_V3[row.names(bd_matching_V3) %in% 1:20000, ]

#bd_split_1 <- bd_split_1 %>%
#  rowwise() %>%
#  mutate(num_operations_within_range = sum(across(starts_with("f.41282.0.")) >= date_range_start & across(starts_with("f.41282.0.")) <= date_range_end, na.rm = TRUE)) %>%
#  ungroup()


#DATA-TABLE METHOD (works)
# Convert bd_matching_V3 to data.table
setDT(m.data)

# Get the column names starting with "f.41282.0."
cols_to_convert <- names(m.data)[startsWith(names(m.data), "f.41282.0.")]

# Convert columns to Date format
m.data[, (cols_to_convert) := lapply(.SD, as.Date), .SDcols = cols_to_convert]

# Calculate the number of operations within the date range for each row
m.data[, num_operations_within_range := rowSums(sapply(.SD, function(x) x >= date_range_start & x <= date_range_end), na.rm = TRUE), .SDcols = cols_to_convert]

summary(m.data$num_operations_within_range)


#Categorical
m.data <- m.data %>% 
  mutate(categorical_ops = ifelse(m.data$num_operations_within_range == 0, 0,
                                   ifelse(m.data$num_operations_within_range == 1, 1, 2)))

#Create some plots
dcm.labs <- c("Non-DCM Episodes", "DCM Episodes")
names(dcm.labs) <- c("0", "1")

ggplot(m.data, aes(x = num_operations_within_range)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "Number of Operations", y = "Frequency", title = "Number of operations in the two years to six months prior to episode") +
  facet_grid(rows = vars(DCM), scales = "free", labeller = labeller(DCM = dcm.labs)) +
  theme_light()

m.data %>%
  mutate(categorical_ops = recode(categorical_ops, "0" = "0", 
                      "1" = "1", 
                      "2" = "2+")) %>%
ggplot(aes(x = categorical_ops)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "Number of Operations", y = "Frequency", title = "Number of operations in the two years to six months prior to episode") +
  facet_grid(rows = vars(DCM), scales = "free", labeller = labeller(DCM = dcm.labs)) +
  theme_light()

m.data$DCM <- as.factor(m.data$DCM)
ggplot(m.data, aes(num_operations_within_range, fill = DCM)) + geom_bar(position = "dodge")




#MATCHING--------------------------------------------------------------------
rm(list=setdiff(ls(), "bd_matching_V3"))#Remove all objects that aren't bd_matching from the environment, to save memory
gc()
#The GetMatchedSets / CGEN package, which doesn't seem to work
#bd_matched <- getMatchedSets(bd_matching, CC=TRUE, NN=FALSE, 
#            ccs.var="DCM", dist.vars=c("episode_year_month","approx_age_at_episode","f.31.0.0"), ratio=4)

#The MatchIt package, which does
m.out1 <- matchit(DCM ~ episode_year_month + approx_age_at_episode + f.31.0.0, data = bd_matching_V3,
                  method = "quick", distance = "glm")
summary(m.out1, un = TRUE)
plot(m.out1, type = "jitter", interactive = FALSE)
plot(m.out1, type = "density", interactive = FALSE,
     which.xs = ~episode_year_month + approx_age_at_episode + f.31.0.0)
plot(summary(m.out1))

#ESTIMATING THE TREATMENT EFFECT-------------------------------------------------------

m.data <- match.data(m.out1)

#Try a simple version, without matching or covariates
dcm_numops <- m.data %>%
  glm(formula = DCM ~ num_operations_within_range, family = "binomial")

summ(dcm_numops, digits = 2)

effect_plot(dcm_numops, pred = num_operations_within_range, plot.points = TRUE,
            jitter = c(0.1, 0.05), point.alpha = 0.1)

predictionDat <- tibble(num_operations_within_range = c(0:63))
predictions <- make_predictions(dcm_numops, new_data = predictionDat,
                                outcome.scale = "response")

ggplot(data=predictions, aes(num_operations_within_range, DCM)) +
  geom_point(aes(x=num_operations_within_range, y=DCM)) +
  geom_errorbar(data=predictions, aes(y=DCM, ymin = ymin, ymax = ymax), color = "red") +
  ylab("Probability of episode being DCM") +
  xlab("Number of operations in 24-6 months prior to episode")

#Binarise
m.data <- m.data %>% 
  mutate(op_in_range = ifelse(num_operations_within_range >= 1, 1, 0)) %>%
  relocate(op_in_range, .after = num_operations_within_range)

#Chi Square on num_ops & op_in_range
table=with(m.data,table(DCM,num_operations_within_range))
table
chisq.test(table)
fisher.test(table)

tab=with(m.data,table(DCM,op_in_range))
tab
chisq.test(tab)
fisher.test(tab)

#DCM + matched variables on num_ops_in_range with matching weights (linear regression - not useful!)
#fit1 <- lm(num_operations_within_range ~ DCM * (episode_year_month + approx_age_at_episode + f.31.0.0), data = m.data, weights = weights)

#avg_comparisons(fit1,
#                variables = "DCM",
#                vcov = ~subclass,
#                newdata = subset(m.data, DCM == 1),
#                wts = "weights")

#Num_ops_in_range on DCM with matching weights
fit2 <- glm(DCM ~ num_operations_within_range, data = extracted, weights = weights, family = binomial())


avg_comparisons(fit2,
                variables = "num_operations_within_range",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights",
                comparison = "lnoravg",
                transform = "exp")

#Op_in_range on DCM with matching weights
fit3 <- glm(DCM ~ op_in_range, data = m.data, weights = weights, family = quasibinomial())

avg_comparisons(fit3,
                variables = "op_in_range",
                vcov = ~subclass,
                newdata = subset(m.data, op_in_range == 1),
                wts = "weights", 
                comparison = "lnoravg",
                transform = "exp")

#Try a categorical approach - 0 vs 1 vs 2+ operations
#Calculate categories
extracted <- extracted %>% 
  mutate(categorical_ops_in_range = ifelse(extracted$num_operations_within_range == 0, 0,
                                             ifelse(extracted$num_operations_within_range == 1, 1, 2))) %>%
relocate(categorical_ops_in_range, .after = num_operations_within_range)

#Chi square
tab_categorical=with(extracted,table(DCM,categorical_ops_in_range))
tab_categorical
chisq.test(tab_categorical)

#Logistic regression
fit_categorical <- glm(DCM ~ categorical_ops_in_range, data = extracted, weights = weights, family = binomial())

avg_comparisons(fit_categorical,
                variables = "categorical_ops_in_range",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights", 
                comparison = "lnratioavg",
                transform = "exp")

#Num_ops_in_range + matched variables + covariates on DCM with matching weights
summary(m.data$f.816.0.0)
summary(m.data$icd_CHF_count)
summary(m.data$icd_CVA_count)
summary(m.data$icd_DM_count)
summary(m.data$icd_MI_count)
summary(m.data$icd_PVD_count)

fit_covariates <- glm(DCM ~ num_operations_within_range * (f.816.0.0 + icd_CHF_count + icd_CVA_count + icd_DM_count + icd_MI_count + icd_PVD_count), data = m.data, weights = weights, family = quasibinomial())

avg_comparisons(fit_covariates,
                variables = "num_operations_within_range",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights", 
                comparison = "lnratioavg",
                transform = "exp")

#Op_in_range + matched variables + covariates on DCM with matching weights
fit_covariates_binarised <- glm(DCM ~ op_in_range * (f.816.0.0 + icd_CHF_count + icd_CVA_count + icd_DM_count + icd_MI_count + icd_PVD_count), data = m.data, weights = weights, family = quasibinomial())

avg_comparisons(fit_covariates_binarised,
                variables = "op_in_range",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights", 
                comparison = "lnratioavg",
                transform = "exp")

#Categorical + matched variables + covariates on DCM with matching weights
fit_covariates_categorical <- glm(DCM ~ categorical_ops * (f.816.0.0 + icd_CHF_count + icd_CVA_count + icd_DM_count + icd_MI_count + icd_PVD_count), data = m.data, weights = weights, family = quasibinomial())

avg_comparisons(fit_covariates_categorical,
                variables = "categorical_ops",
                vcov = ~subclass,
                newdata = "mean",
                wts = "weights", 
                comparison = "lnratioavg",
                transform = "exp")

