# LME

source(here('scripts', 'Other_Functions.R'))
data <- load_processed_data()
data_long <- load_processed_data_long()

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




# Visualize Data
# Trust by Confidence
g1 <- ggplot(data_long, aes(x=Confidence, y=Trust, color=Condition, group=Condition)) + 
  geom_smooth(alpha = 0.25) + 
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
  theme_classic() +
  labs(title = "Trust by Confidence by Condition", x = "Confidence", y = "Trust") +
  scale_color_manual(values = c("Decreasing" = colour.decreasing, "Increasing" = colour.increasing))+
  ylim(0, 100) + 
  xlim(0, 100)
print(g1)
ggsave(here('output','figures','09_Trust_by_Confidence_by_Condition.png'), 
       plot = g1, device = device, width = width, height = height, units = units, dpi = dpi)

g2 <- ggplot(data_long, aes(x=Confidence, y=Trust, color=Condition, group=Condition)) + 
  geom_smooth(alpha = 0.25, method = 'lm') + 
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
  theme_classic() +
  labs(title = "Trust by Confidence by Condition", x = "Confidence", y = "Trust") +
  scale_color_manual(values = c("Decreasing" = colour.decreasing, "Increasing" = colour.increasing))+
  ylim(0, 100) + 
  xlim(0, 100)
print(g2)
ggsave(here('output','figures','09_Trust_by_Confidence_by_Condition_(Linear_Model).png'), 
       plot = g2, device = device, width = width, height = height, units = units, dpi = dpi)

g3 <- ggplot(data_long, aes(x=Confidence, y=Trust)) + 
  geom_smooth(alpha = 0.25, method = 'lm', colour = 'green') + 
  geom_jitter(width = 0.2, height = 0, alpha = 0.5, colour = 'green') +
  theme_classic() +
  labs(title = "Trust by Confidence by Condition", x = "Confidence", y = "Trust") +
  ylim(0, 100) + 
  xlim(0, 100)
print(g3)
ggsave(here('output','figures','09_Trust_by_Confidence.png'), 
       plot = g3, device = device, width = width, height = height, units = units, dpi = dpi)


g4 <- ggplot(data_long, aes(x = Trust, y = Reliance, color = as.factor(Reliability), group = as.factor(Reliability))) +
  geom_jitter(size = 0.5, alpha = 0.3) + 
  geom_smooth(method = 'lm', alpha = 0.2) + 
  facet_wrap(.~Condition, scales='free') + 
  theme_classic() +
  theme(axis.line=element_line()) +
  labs(title = "Trust as a function of self-confidence",
       x = "Trust",
       y = "Reliance",
       color = "Reliability") +
  ylim(0, 1) +
  xlim(0, 100)
print(g4)


g5 <- ggplot(data_long, aes(x = Confidence, y = Trust, color = as.factor(Reliability), group = as.factor(Reliability))) +
  geom_jitter(size = 0.5, alpha = 0.3) + 
  geom_smooth(method = 'lm', alpha = 0.2) + 
  facet_wrap(.~Condition, scales='free') + 
  theme_classic() +
  theme(axis.line=element_line()) +
  labs(title = "Trust as a function of self-confidence",
       x = "Confidence",
       y = "Trust",
       color = "Reliability") +
  ylim(0, 100) +
  xlim(0, 100)
print(g5)


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



# dt[, 
#    list(meanRT=mean(RT), sdRT=sd(RT)), 
#    by=.(Condition, Block, Participant)
#    ][, list(meanRT=mean(meanRT), sdRT=mean(sdRT)), 
#       by=.(Condition, Block)]
# 
# dt %>% 
#   group_by(Condition, Block, Participant) %>% 
#   summarise(meanRT = mean(RT), sdRT = sd(RT))


