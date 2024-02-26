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


# Visualize
g1 <- ggplot(data_all_conditions_long, aes(x = Confidence, y = Trust, color = as.factor(Block), group = as.factor(Block))) +
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

g2 <- ggplot(data_all_conditions_long, aes(x = Confidence, y = Trust, color = as.factor(Reliability), group = as.factor(Reliability))) +
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
print(g2)






