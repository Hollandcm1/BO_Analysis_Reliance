# Model Exploration

source(here('scripts', 'Other_Functions.R'))
data <- load_processed_data_all_conditions()
data_long <- load_processed_data_all_conditions_long()

# Figure Parameters
source(here('scripts', 'Figure_Parameters.R'))

# Data manipulation
data_long$Reliability_factor <- as.factor(data_long$Reliability)

##############
### Models ###
##############
model_trust <- lmer(data = data_long, Trust ~ Performance_Before * Confidence * Reliance * Reliability * Condition + (1 | Participant))
summary(model_trust)
tab_model(model_trust)


model_performance <- lmer(data = data_long, Performance_Before ~ Trust * Confidence * Reliance * Reliability * Condition + (1 | Participant))
summary(model_performance)
tab_model(model_performance)


model_reliance <- lmer(data = data_long, Reliance ~ Trust * Confidence * Performance_Before * Reliability * Condition + (1 | Participant))
summary(model_reliance)
tab_model(model_reliance)
