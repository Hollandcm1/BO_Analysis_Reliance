# Hypothesis Based Models

source(here('scripts', 'Other_Functions.R'))
data_all <- load_processed_data_all_conditions()
data_long_all <- load_processed_data_all_conditions_long()
data_50 <- load_processed_data()
data_long_50 <- load_processed_data_long()


# Figure Parameters
source(here('scripts', 'Figure_Parameters.R'))

# Data manipulation
data_long_all$Reliability_factor <- as.factor(data_long_all$Reliability)
data_long_50$Reliability_factor <- as.factor(data_long_50$Reliability)


###############################################################################
### H1 - between 50% increasing and 50% decreasing, Reliance behaviour will ###
### be different ###
###############################################################################
model_H1 <- lmer(data = data_long_50, Reliance ~ Condition + (1 | Participant))
summary(model_H1)
tab_model(model_H1)

# Visualize Interaction
g1 <- ggplot(data_long_50, aes(x=Condition, y=Reliance)) + 
  geom_boxplot() + 
  geom_jitter(width = 0.2, height = 0) +
  theme_classic() +
  labs(title = "Dependence by Condition", x = "Condition", y = "Dependence") +
  scale_x_discrete(labels = c("Decreasing" = "50% Decreasing", "Increasing" = "50% Increasing")) +
  theme(plot.title = element_text(size=title_size))
print(g1)

### RESULT - does not support the hypothesis ###


###############################################################################
### H2 - between 50% increasing and 50% decreasing, Reliance behaviour will ###
### be different and influenced by trust and self-confidence, similar to    ###
### how performance is influenced by trust and self-confidence              ###
###############################################################################

model_H2 <- lmer(data = data_long_50, Reliance ~ Condition * Trust * Confidence + (1 | Participant))
summary(model_H2)
tab_model(model_H2)

###############################################################################
### H2 - between 50% increasing and 50% decreasing, Reliance behaviour will ###
### be different and influenced intrisict factors (trust and self-confidence)##
### and extrinsic factors (reliability/dependence)                          ###
###############################################################################

model_H3 <- lmer(data = data_long_50, Reliance ~ Condition * Trust * Confidence * Reliability + (1 | Participant))
summary(model_H3)
tab_model(model_H3)

# Visualize Interaction
flexplot(data = data_long_50, Reliance ~ Trust + Reliability_factor | Condition + Confidence, method = 'lm', bins = 3)

# Visualize Interaction
g1 <- ggplot(data = data_long_50, aes(x=Trust, y=Reliance, colour=Reliability_factor)) +
  geom_jitter(alpha=0.5, height = 0.01, width = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  labs(title = "Trust by Reliance by Reliability", x = "Trust", y = "Reliance") +
  theme(plot.title = element_text(size=title_size)) +
  facet_wrap(~Condition)
print(g1)
