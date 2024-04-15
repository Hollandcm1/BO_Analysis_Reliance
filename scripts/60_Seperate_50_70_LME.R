# 60_Seperate_50_70_LME

library(ggbeeswarm)

source(here('scripts', 'Other_Functions.R'))
data <- load_processed_data_all_conditions()
data_long <- load_processed_data_all_conditions_long()

# Figure Parameters
source(here('scripts', 'Figure_Parameters.R'))

# Data manipulation
data_long$Reliability_factor <- as.factor(data_long$Reliability)
data_long$Block_factor <- as.factor(data_long$Block)

# Subsets
data_long_50 <-  data_long %>% 
  filter(Condition == '50% Decreasing' | Condition == '50% Increasing')
# remove the other possible factors
data_long_50$Condition <- factor(data_long_50$Condition, levels = c('50% Decreasing', '50% Increasing'))

data_long_70 <-  data_long %>%
  filter(Condition == '70% Decreasing' | Condition == '70% Increasing')

##########################
### 50% Model Building ###
##########################

################################# Base ########################################
m50_0 <- lmer(data = data_long_50, Trust ~ Confidence + (1|Participant))
summary(m50_0)
tab_model(m50_0)

ggplot(data_long_50, aes(x = Confidence, y = Trust)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  xlim(0, 100) +
  ylim(0, 100)

#################################### m1 #######################################
m50_1 <- lmer(data = data_long_50, Trust ~ Confidence + (1|Condition))
summary(m50_1)
tab_model(m50_1)

ggplot(data_long_50, aes(x = Confidence, y = Trust, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  xlim(0, 100) +
  ylim(0, 100)

ggplot(data_long_50, aes(x = Condition, y = Trust)) +
  ggdist::stat_halfeye(adjust = 0.6, width = .4, .width = 0, justification = -0.2, fill='cyan', alpha=0.4) + 
  geom_boxplot(width = .1, outlier.shape = NA) +
  ggdist::stat_dots(side = "right", dotsize = 4, justification = -0.1, binwidth = .15, colour = 'blue') +
  theme_minimal()

# compare m1 to m0
anova(m50_0, m50_1) # Condition is useful!

################################## m2 ########################################
m50_2 <- lmer(data = data_long_50, Trust ~ Confidence + (1|Participant) + (1|Condition))
summary(m50_2)
tab_model(m50_2)

# compare m2 to m1
anova(m50_1, m50_2) # Both are even more useful!

################################## m3 ########################################
m50_3 <- lmer(data = data_long_50, Trust ~ Confidence + (1|Participant) + (1|Condition) + (1|Participant:Condition))
summary(m50_3)
tab_model(m50_3)

# compare m3 to m2
anova(m50_2, m50_3) # No difference, which i guess should have been expected

################################## m4 ########################################
m50_4 <- lmer(data = data_long_50, Trust ~ Confidence + (1|Participant:Condition))
summary(m50_4)
tab_model(m50_4)

# compare m4 to m3
anova(m50_3, m50_4) # Very similar to m2 and m3, I'm guessing it's doing a similar thing but slight interpretation difference.
# Moving forward with this model

################################## m5 ########################################
m50_5 <- lmer(data = data_long_50, Trust ~ Confidence + Condition + (1|Participant:Condition))
summary(m50_5)
tab_model(m50_5)

m50_5 <- lmer(data = data_long_50, Trust ~ Confidence + Condition + (1|Participant))
summary(m50_5)
tab_model(m50_5)

m50_5 <- lmer(data = data_long_50, Trust ~ Confidence + Condition + (1|Condition:Participant))
summary(m50_5)
tab_model(m50_5)

# Based on Testing and Further Research, it doesn't make sense to include condition 
# in the (1|x), and instead should be included as a fixed effect.

################################## m6 ########################################
m50_6 <- lmer(data = data_long_50, Trust ~ Confidence + Condition + (1|Participant))
summary(m50_6)
tab_model(m50_6)

# compare m6 to base
anova(m50_0, m50_6) # no major change... however theory says there should be

################################## m7 ########################################
m50_7 <- lmer(data = data_long_50, Trust ~ Condition + (1|Participant))
summary(m50_7)
tab_model(m50_7)

# compare m7 to base
anova(m50_0, m50_7) # no major change... however theory says there should be
summary(m50_7)
summary(m50_0)

################################## m8 ########################################
m50_8 <- lmer(data = data_long_50, Trust ~ Confidence + Condition + (1|Participant))
summary(m50_8)
tab_model(m50_8)

################################## m9 ########################################
m50_9 <- lmer(data = data_long_50, Trust ~ Confidence + Condition + Reliability + Reliance + (1|Participant))
summary(m50_9)
tab_model(m50_9)

################################## m10 ########################################
m50_10 <- lmer(data = data_long_50, Trust ~ Confidence * Condition * Reliability * Reliance + (1|Participant))
summary(m50_10)
tab_model(m50_10)

# compared m9 to m10
anova(m50_9, m50_10) 


################################## m11 ########################################
# downgrading to the 3 way interaction
m50_11 <- lmer(data = data_long_50, Trust ~ Condition * Reliability * Reliance + (1|Participant))
summary(m50_11)
tab_model(m50_11)



#############. Visuals


ggplot(data_long_50, aes(x = Confidence, y = Trust, color = Reliability_factor)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  xlim(0, 100) +
  ylim(0, 100) +
  facet_wrap(~Condition)

ggplot(data_long_50, aes(x = Confidence, y = Trust, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_classic() +
  xlim(0, 100) +
  ylim(0, 100)

ggplot(data_long_50, aes(x = , y = Trust, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_classic() +
  xlim(0, 100) +
  ylim(0, 100)

flexplot(data = data_long_50, Trust ~ Confidence + Reliability_factor | Condition + Reliance, method = 'lm')
flexplot(data = data_long_50, Trust ~ Reliance + Reliability_factor | Condition + Confidence, method = 'lm')

              


# Trust Models
m50_0 <- lmer(data = data_long_50, Trust ~ Condition + (1|Participant))
summary(m50_0)
tab_model(m50_0)
plot(m50_0)

m50_0 <- lmer(data = data_long_50, Trust ~ Participant + (1|Condition))
summary(m50_0)
tab_model(m50_0)
plot(m50_0)

m50_1 <- lmer(data = data_long_50, Trust ~ (1|Participant:Condition))
summary(m50_1)
tab_model(m50_1)
plot(m50_1)

m50_2 <- lmer(data = data_long_50, Trust ~ Reliability_factor + (1|Participant) + (1|Condition))

m <- lmer(data = data_long_50, Trust ~ Condition * Reliability * Confidence * Reliance + (1|Participant))
summary(m)
tab_model(m)
plot(m)

# Dependence Models

# Performance Models


######################
### 50% Visualize ####
######################

ggplot(data_long_50, aes(x = Condition, y = Trust)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggdist)

ggplot(data_long_50, aes(Condition, Trust)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA) + 
  geom_boxplot(width = .1, outlier.shape = NA) +
  ggdist::stat_dots(side = "left", dotsize = 5, justification = 1.1, binwidth = .1) +
  theme_minimal() 

ggplot(data_long_50, aes(Participant, Trust, color = Participant)) + 
  geom_boxplot() +
  geom_point() + 
  theme_minimal() +
  theme(legend.position = "none")

flexplot(data = data_long_50, Trust ~ Reliance + Reliability_factor | Condition, method = 'lm')
flexplot(data = data_long_50, Trust ~ Reliance + Condition, method = 'lm')
flexplot(data = data_long_50, Trust ~ Confidence, method = 'lm')
