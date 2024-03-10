# Beta Regression

source(here('scripts', 'Other_Functions.R'))
data <- load_processed_data_all_conditions()
data_long <- load_processed_data_all_conditions_long()

# Figure Parameters
source(here('scripts', 'Figure_Parameters.R'))

# Regular Model
model_8 <- lmer(data = data_long, Reliance ~ Condition * Reliability * Confidence * Trust + (1 | Participant))
summary(model_8)
tab_model(model_8)

##################
### Beta Model ###
##################

data_long$Reliance_adj <- data_long$Reliance
# Adjust 0 to a small value above 0, e.g., 0.0001
data_long$Reliance_adj[data_long$Reliance == 0] <- 0.0001
# Adjust 1 to a small value below 1, e.g., 0.9999
data_long$Reliance_adj[data_long$Reliance == 1] <- 0.9999

model_beta_adj <- betareg(Reliance_adj ~ Condition * Reliability * Confidence * Trust, data = data_long)
summary(model_beta_adj)
tab_model(model_beta_adj)
