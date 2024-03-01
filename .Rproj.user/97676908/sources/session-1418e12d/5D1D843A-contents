# LME Follow-up

# Check Residuals
plot(resid(model_8) ~ fitted(model_8))
abline(h = 0, lty = 2, col = "red")

# Check Normality
qqnorm(resid(model_8))
qqline(resid(model_8))

# Check Cook's Distance
# library(influence.ME)
# infl <- influence(model_8, obs = TRUE)
# plot(infl, which = "cook")

# Check Homoscedasticity
plot(model_8)

# Check Variance Inflation Factors
library(car)
vif(model_8)

# Check for Outliers
outlierTest(model_8)

# Check for Influential Cases
influenceIndexPlot(model_8)

# Check for Multicollinearity
library(corrplot)
cor <- cor(data_long[,c("Trust", "Reliability", "Confidence", "Reliance")])
corrplot(cor, method = "circle")

# Check for Overdispersion
library(blmeco)
dispersion_glmer(model_8)

# Check for Overdispersion 2
library(MuMIn)
r.squaredGLMM(model_8)

# Check for Zero Inflation
library(pscl)
zeroinfl(model_8)


ggplot(data_long, aes(x = fitted(model_8), y = residuals(model_8))) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red")

library(glmmTMB)
plot_model(model_8, type = "diag")
