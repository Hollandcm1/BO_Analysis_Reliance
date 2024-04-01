# LME Follow-up

# load data
source(here('scripts', 'Other_Functions.R'))
data <- load_processed_data()
data_long <- load_processed_data_long()

# Run the best model
model_8 <- lmer(data = data_long, Reliance ~ Condition * Reliability * Confidence * Trust + (1 | Participant))
summary(model_8)
tab_model(model_8)

# Check Residuals and Homoscedasticity
plot(resid(model_8) ~ fitted(model_8))
abline(h = 0, lty = 2, col = "red")
# Theres an issue that is showing up here with how the residuals are distributed.
# This is likely due to the fact that the residuals are not normally distributed.
# Specifically, there is a bi-model trend, anchroing to 1 or 0 with relatively 
# uniform distribution between (see the next histogram)

# Check Normality of data
hist(data_long$Reliance, breaks = 20)
qqnorm(data_long$Reliance)
qqline(data_long$Reliance)

# Check Normality of model
hist(resid(model_8))
qqnorm(resid(model_8))
qqline(resid(model_8))

# Check Cook's Distance
# library(influence.ME)
# infl <- influence(model_8, obs = TRUE)
# plot(infl, which = "cook")


# Check Variance Inflation Factors
library(car)
vif(model_8)
# Not sure I understand the meaning of the output here

# Check for Outliers
outlierTest(model_8)
# Probably should go back to original data to double check that nothing is wrong, 
# given that this is saying there are likely outlines

# Check for Influential Cases
influenceIndexPlot(model_8)
# Need to do more research on interpretation of this

# Check for Multicollinearity
library(corrplot)
cor <- cor(data_long[,c("Trust", "Reliability", "Confidence", "Reliance")])
corrplot(cor, method = "circle")


# This next section has been commented out because it is exploratory and not
# necessary for the final analysis. It is left here for future reference.
# Note the some of the libraries bellow will overwrite the libraries that are 
# currently loaded, causing issues with the current analysis.

# # Check for Overdispersion
# library(blmeco)
# dispersion_glmer(model_8)
# 
# # Check for Overdispersion 2
# library(MuMIn)
# r.squaredGLMM(model_8)
# 
# # Check for Zero Inflation
# library(pscl)
# zeroinfl(model_8)
# 
# library(glmmTMB)
# plot_model(model_8, type = "diag")
