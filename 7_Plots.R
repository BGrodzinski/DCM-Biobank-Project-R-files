#PLOTS
library(tidyverse)
#Count
dcm.labs <- c("Non-DCM Episodes", "DCM Episodes")
names(dcm.labs) <- c("0", "1")

ggplot(m.data, aes(x = opcs_anaesthetist_count)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "Number of Operations", y = "Frequency", title = "Number of anaesthetist-filtered operations in the two years to six months prior to episode") +
  facet_grid(rows = vars(DCM), scales = "free", labeller = labeller(DCM = dcm.labs)) +
  theme_light()

#Categorical
m.data <- m.data %>% 
  mutate(categorical_anaes = ifelse(m.data$opcs_anaesthetist_count == 0, 0,
                                    ifelse(m.data$opcs_anaesthetist_count == 1, 1, 2)))
m.data <-m.data %>%
  mutate(categorical_anaes = recode(categorical_anaes, "0" = "0", 
                                    "1" = "1", 
                                    "2" = "2+"))

ggplot(m.data, aes(x = categorical_anaes)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "Number of Operations", y = "Frequency", title = "Number of anaesthetist-filtered operations in the two years to six months prior to episode") +
  facet_grid(rows = vars(DCM), scales = "free", labeller = labeller(DCM = dcm.labs)) +
  theme_light()

#Proportion
summary_table <- m.data %>%
  group_by(opcs_anaesthetist_count, DCM) %>%
  summarise(count = n())

summary_table <- summary_table %>%
  mutate(n = ifelse(DCM == 0, 2287432, ifelse(DCM == 1, 806, NA))) %>%
  mutate(proportion = count/n)

summary_table$DCM <- as.factor(summary_table$DCM)

#scatter plot
ggplot(summary_table, aes(x = opcs_anaesthetist_count, y = proportion, color = DCM)) +
  geom_point() +
  labs(x = "Number of Operations", y = "Proportion of Episodes", title = "Proportion of anaesthetist-filtered operations in the two years to six months prior to episode") +
  scale_color_discrete(name = "DCM") +
  theme_classic()

#bar plot
ggplot(summary_table, aes(x = opcs_anaesthetist_count, y = proportion, fill = DCM)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Number of Operations", y = "Proportion of Episodes") +
  scale_fill_discrete(name = "DCM") +
  coord_cartesian(xlim=c(0, 20)) +
  theme_classic()


#Visualising logistic regression------------------
#Num_ops
fit_count <- glm(DCM ~ num_operations_within_range, data = m.data, weights = weights, family = quasibinomial())

predictionDat <- tibble(num_operations_within_range = c(0:63))
predictions <- make_predictions(fit_count, new_data = predictionDat,
                                outcome.scale = "response")

ggplot(data=predictions, aes(num_operations_within_range, DCM)) +
  geom_point(aes(x=num_operations_within_range, y=DCM)) +
  ylab("Probability of episode being DCM") +
  xlab("Number of operations in 24-6 months prior to episode") + 
  theme_classic()

#opcs_anaesthetist_count
fit_count <- glm(DCM ~ opcs_anaesthetist_count, data = m.data, weights = weights, family = quasibinomial())

predictionDat <- tibble(opcs_anaesthetist_count = c(0:28))
predictions <- make_predictions(fit_count, new_data = predictionDat,
                                outcome.scale = "response")

ggplot(data=predictions, aes(opcs_anaesthetist_count, DCM)) +
  geom_point(aes(x=opcs_anaesthetist_count, y=DCM)) +
  geom_errorbar(data=predictions, aes(y=DCM, ymin = ymin, ymax = ymax), color = "red") +
  ylab("Probability of episode being DCM") +
  xlab("Number of operations in 24-6 months prior to episode") + 
  theme_classic()

#categorical_anaes
fit_count <- glm(DCM ~ categorical_anaes, data = m.data, weights = weights, family = quasibinomial())

predictionDat <- tibble(categorical_opcs = c(0:2))
predictions <- make_predictions(fit_opcs_categorical, new_data = predictionDat,
                                outcome.scale = "response")

ggplot(data=predictions, aes(categorical_opcs, DCM)) +
  geom_point(aes(x=categorical_opcs, y=DCM)) +
  geom_errorbar(data=predictions, aes(y=DCM, ymin = ymin, ymax = ymax), color = "red") +
  ylab("Probability of episode being DCM") +
  xlab("Number of operations in 24-6 months prior to episode") + 
  theme_classic()

#Visualising odds ratios
ORs <- read.csv("~/Downloads/ORs.csv")
ggplot(ORs, aes(X, OR, ymin = Min, ymax = Max))+
  geom_errorbar()

dat <- data.frame(
  Index = c(1, 2, 3, 4, 5, 6, 7, 8), ## This provides an order to the data
  label = c("All - Binarised", "All - Categorical", "Nonsupine - Binarised", "Nonsupine - Categorical", "Nonspinal - Binarised", "Nonspinal - Categorical", "Only Spinal - Binarised", "Only Spinal - Categorical"),
  OR = c(1.20, 1.13, 1.12, 1.07, 0.95, 0.93, 6.11, 2.84),
  LL = c(1.02, 1.02, 0.62, 0.66, 0.80, 0.83, 4.67, 2.45),
  UL = c(1.41, 1.25, 2.03, 1.76, 1.13, 1.04, 7.99, 3.29),
  CI = c("blah")
)

plot1 <- ggplot(dat, aes(y = Index, x = OR)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  scale_y_continuous(name = "", breaks=1:8, labels = dat$label, trans = "reverse") +
  xlab("Odds Ratio (95% CI)") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))
plot1
