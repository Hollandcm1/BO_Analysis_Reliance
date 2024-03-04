# exploritory

source(here('scripts', 'Other_Functions.R'))
data <- load_processed_data()
data_long <- load_processed_data_long()

data_long$Block <- as.factor(data_long$Block)
data_long$Block <- as.numeric(data_long$Block)


#############
### Trust ###
#############
# ANOVA 
model_1 <- aov(data = data_long, Trust ~ Condition * Block + Error(Participant / Block))
summary(model_1)
tab_model(model_1)

# ANOVA with Reliance as pretictor
model_2 <- aov(data = data_long, Trust ~ Condition * Block * Reliance + Error(Participant / Block))
summary(model_2)
tab_model(model_2)

model_3 <- aov(data = data_long, Trust ~ Condition * Block * Reliance * Confidence + Error(Participant / Block))
summary(model_3)
tab_model(model_3)

ggplot(data_long, aes(x = Reliance, y = Trust, color = Block, group = Block)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_classic()

ggplot(data_long, aes(x = Reliance, y = Trust, color = Condition)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Block) + 
  theme_classic()

ggplot(data_long, aes(x = Reliance, y = Trust, colour = as.factor(Block))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Block) + 
  theme_classic()

ggplot(data_long, aes(x = Reliance, y = Trust, colour = as.factor(Block))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + 
  theme_classic()






