# All Conditions LME

source(here('scripts', 'Other_Functions.R'))
data <- load_processed_data_all_conditions()
data_long <- load_processed_data_all_conditions_long()

# Figure Parameters
source(here('scripts', 'Figure_Parameters.R'))

# Models

model_1 <- lmer(data = data_long, Trust ~ Condition * Block * Confidence + (1 | Participant))
summary(model_1)
tab_model(model_1)

model_2 <- lmer(data = data_long, Trust ~ Condition * Block * Confidence + (1 | Condition))
summary(model_2)
tab_model(model_2)

model_3 <- lmer(data = data_long, Trust ~ Condition * Block * Confidence + (1 | Participant) + (1 | Condition))
summary(model_3)
tab_model(model_3)

model_4 <- lmer(data = data_long, Trust ~ Condition * Reliability * Confidence + (1 | Participant))
summary(model_4)
tab_model(model_4)

model_5 <- lmer(data = data_long, Trust ~ Condition * as.factor(Reliability) * Confidence + (1 | Condition))
summary(model_5)
tab_model(model_5)

model_6 <- lmer(data = data_long, Trust ~ Condition * Reliability * Confidence + (1 | Participant) + (1 | Condition))
summary(model_6)
tab_model(model_6)

model_7 <- lmer(data = data_long, Trust ~ Condition * Reliability * Confidence * Reliance + (1 | Participant))
summary(model_7)
tab_model(model_7)

model_8 <- lmer(data = data_long, Reliance ~ Condition * Reliability * Confidence * Trust + (1 | Participant))
summary(model_8)
tab_model(model_8)

model_9 <- lmer(data = data_long, Performance_Before ~ Trust * Condition * Reliability * Confidence + (1 | Participant))
summary(model_9)
tab_model(model_9)

model_10 <- lmer(data = data_long, Performance_After ~ Trust * Condition * Reliability * Confidence + (1 | Participant))
summary(model_10)
tab_model(model_10)


# Moving forward with model_8 
model_8
tab_model(model_8)
plot(model_8)
emmeans(model_8, pairwise ~ Condition | Reliability, adjust = "tukey")
flexplot(Reliance~Trust + Condition, data=data_long)
flexplot(Reliance~Trust + Condition, data=data_long, method="lm")
data_long$Reliability_factor <- as.factor(data_long$Reliability)
flexplot(Reliance~Trust + Condition | Reliability_factor + Confidence, data=data_long)
flexplot(Reliance~Trust + Condition | Reliability_factor + Confidence, data=data_long, method="lm")
flexplot(Reliance~Trust + Condition | Reliability_factor + Confidence, data=data_long, method="lm", se=TRUE)
flexplot(Reliance~Trust + Confidence | Reliability_factor, data=data_long, method="lm")
flexplot(Reliance~Trust + Reliability_factor | Confidence, data=data_long, method="lm")
factor_model_8 <- lmer(data = data_long, Reliance ~ Trust * Condition * Reliability_factor * Confidence + (1 | Participant))
emmeans(factor_model_8, pairwise ~ Confidence | Reliability_factor, adjust = "tukey")
flexplot(Reliance~Trust + Confidence, data=data_long, method="lm")
flexplot(Trust~Confidence + Reliance, data=data_long, method="lm")
flexplot(Reliance~Trust + Reliability_factor, data=data_long, method="lm")
flexplot(Reliance~Trust + Condition, data=data_long, method="lm")
flexplot(Reliance~Confidence + Reliability_factor | Condition, data=data_long, method="lm")
emmeans(model_8, c("Condition", "Reliability"))

emmeans_model <- emmeans(model_8, ~ Condition * Reliability * Confidence * Trust)
contrast_results <- contrast(emmeans_model, "pairwise", by = "Condition")
summary(contrast_results, adjust = "bonferroni")

mean_confidence <- mean(data_long$Confidence, na.rm = TRUE)
sd_confidence <- sd(data_long$Confidence, na.rm = TRUE)

emm <- emmeans(model_8, specs = ~ Condition * Reliability * Confidence, 
               at = list(Reliability = c(50, 60, 70, 80, 90, 100), 
                         Confidence = c(mean_confidence - sd_confidence, 
                                        mean_confidence, 
                                        mean_confidence + sd_confidence)))
summary(emm)
contrasts_emm <- contrast(emm, method = "pairwise", by = "Condition", adjust = "none")
summary(contrasts_emm)

emmip(model_8, Condition ~ Reliability | Trust)
emmip(model_8, Condition ~ Reliability)
emmip(model_8, Trust ~ Reliability)
emmip(model_8, Reliability ~ Confidence, at = list(Confidence = quantile(data_long$Confidence, probs = c(0.25, 0.5, 0.75))))



# Visualize
g1 <- ggplot(data_long, aes(x = Confidence, y = Trust, color = as.factor(Block), group = as.factor(Block))) +
  geom_jitter(size = 0.5, alpha = 0.3) + 
  geom_smooth(method = 'lm', alpha = 0.2) + 
  facet_wrap(.~Condition, scales='free') + 
  theme_classic() +
  theme(axis.line=element_line()) +
  labs(title = "Trust as a function of self-confidence",
       x = "Self-confidence",
       y = "Trust",
       color = "Block") +
  ylim(0, 100) +
  xlim(0, 100)
print(g1)

g2 <- ggplot(data_long, aes(x = Confidence, y = Trust, color = as.factor(Reliability), group = as.factor(Reliability))) +
  geom_jitter(size = 0.5, alpha = 0.3) + 
  geom_smooth(method = 'lm', alpha = 0.2) + 
  facet_wrap(.~Condition, scales='free') + 
  theme_classic() +
  theme(axis.line=element_line()) +
  labs(title = "Trust as a function of self-confidence",
       x = "Self-confidence",
       y = "Trust",
       color = "Reliability") +
  ylim(0, 100) +
  xlim(0, 100)
print(g2)


g4 <- ggplot(data_long, aes(x = Trust, y = Reliance, color = as.factor(Reliability), group = as.factor(Reliability))) +
  geom_jitter(size = 0.5, alpha = 0.3) + 
  geom_smooth(method = 'lm', alpha = 0.2) + 
  facet_wrap(.~Condition, scales='free') + 
  theme_classic() +
  theme(axis.line=element_line()) +
  labs(title = "Reliance as a funciton of Trust",
       x = "Trust",
       y = "Reliance",
       color = "Reliability") +
  ylim(0, 1) +
  xlim(0, 100)
print(g4)
ggsave(here('output','figures','22_Trust_by_Reliance_by_Condition.png'), 
       plot = g4, device = device, width = width, height = height, units = units, dpi = dpi)



g6 <- ggplot(data_long, aes(x = Confidence, y = Reliance, color = as.factor(Reliability), group = as.factor(Reliability))) +
  geom_jitter(size = 0.5, alpha = 0.3) + 
  geom_smooth(method = 'lm', alpha = 0.2) + 
  facet_wrap(.~Condition, scales='free') + 
  theme_classic() +
  theme(axis.line=element_line()) +
  labs(title = "Trust as a function of self-confidence",
       x = "Confidence",
       y = "Reliance",
       color = "Reliability") +
  ylim(0, 1) +
  xlim(0, 100)
print(g6)
ggsave(here('output','figures','22_Reliance_by_Confidence_by_Reliability_by_Condition.png'), 
       plot = g6, device = device, width = width, height = height, units = units, dpi = dpi)







