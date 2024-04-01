# exploratory

# This isn't really a finished script. I wanted to understand more about whether
# block / reliability of the automation had an effect, but I didn't really get
# anywhere with it.

source(here('scripts', 'Other_Functions.R'))
data <- load_processed_data()
data_long <- load_processed_data_long()

# I couldn't decide what I wanted block as so here is both (keep in mind it
# doesn't effect the ANOVAs)
data_long$Block_Factor <- as.factor(data_long$Block)
data_long$Block_Numeric <- as.numeric(data_long$Block)


#############
### Trust ###
#############
# ANOVA 
model_1 <- aov(data = data_long, Trust ~ Condition * Block_Factor + Error(Participant / Block_Factor))
summary(model_1)
tab_model(model_1)

# ANOVA with Reliance as pretictor
model_2 <- aov(data = data_long, Trust ~ Condition * Block_Factor * Reliance + Error(Participant / Block_Factor))
summary(model_2)
tab_model(model_2)

# Add confidence
model_3 <- aov(data = data_long, Trust ~ Condition * Block_Factor * Reliance * Confidence + Error(Participant / Block_Factor))
summary(model_3)
tab_model(model_3)

ggplot(data_long, aes(x = Reliance, y = Trust, color = Block_Factor, group = Block)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_classic()

ggplot(data_long, aes(x = Reliance, y = Trust, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Block) + 
  theme_classic()

ggplot(data_long, aes(x = Reliance, y = Trust, colour = Block_Factor)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Block) + 
  theme_classic()

ggplot(data_long, aes(x = Reliance, y = Trust, colour = Block_Factor)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_classic()






