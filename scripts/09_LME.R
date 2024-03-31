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

# Best 
model_8 <- lmer(data = data_long, Reliance ~ Condition * Reliability * Confidence * Trust + (1 | Participant))
summary(model_8)
tab_model(model_8)

model_9 <- lmer(data = data_long, Reliance_100 ~ Condition * Reliability * Confidence * Trust + (1 + Condition | Participant))
summary(model_9)
tab_model(model_9)

model_10 <- lmer(data = data_long, Reliance_100 ~ Condition * Reliability * Confidence * Trust + (1 | Participant))
summary(model_10)
tab_model(model_10)

model_11 <- lmer(data = data_long, Reliance_100 ~ Reliability * Confidence * Trust + (1 + Participant | Condition))
summary(model_11)
tab_model(model_11)





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
ggsave(here('output','figures','09_Trust_by_Reliance_by_Condition.png'), 
       plot = g4, device = device, width = width, height = height, units = units, dpi = dpi)

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
ggsave(here('output','figures','09_Trust_by_Confidence_by_Reliability_by_Condition.png'), 
       plot = g5, device = device, width = width, height = height, units = units, dpi = dpi)


g6 <- ggplot(data_long, aes(x = Confidence, y = Reliance, color = as.factor(Reliability), group = as.factor(Reliability))) +
  geom_jitter(size = 0.5, alpha = 0.3) + 
  geom_smooth(method = 'lm', alpha = 0.2) + 
  facet_wrap(.~Condition, scales='free') + 
  theme_classic() +
  theme(axis.line=element_line()) +
  labs(title = "Reliance as a function of self-confidence",
       x = "Confidence",
       y = "Reliance",
       color = "Reliability") +
  ylim(0, 1) +
  xlim(0, 100)
print(g6)
ggsave(here('output','figures','09_Reliance_by_Confidence_by_Reliability_by_Condition.png'), 
       plot = g6, device = device, width = width, height = height, units = units, dpi = dpi)


g7 <- ggplot(data_long, aes(x = Trust, y = Reliance)) +
  geom_jitter(size = 0.5, alpha = 0.3) + 
  geom_smooth(method = 'lm', alpha = 0.2) + 
  facet_wrap(.~Condition, scales='free') + 
  theme_classic() +
  theme(axis.line=element_line()) +
  labs(title = "Dependance as a function of Trust",
       x = "Trust",
       y = "Dependance") +
  ylim(0, 1) +
  xlim(0, 100)
print(g7)

g7_0 <- ggplot(data_long, aes(x = Trust, y = Reliance)) +
  geom_jitter(size = 0.5, alpha = 0.3) + 
  geom_smooth(method = 'lm', alpha = 0.2) + 
  theme_classic() +
  theme(axis.line=element_line()) +
  labs(title = "Dependance as a function of Trust",
       x = "Trust",
       y = "Dependance")
print(g7_0)

g7_1 <- ggplot(data_long, aes(x = Trust, y = Reliance, color = Condition, group = Condition)) +
  geom_jitter(size = 0.5, alpha = 0.3) + 
  geom_smooth(method = 'lm', alpha = 0.2) + 
  theme_classic() +
  theme(axis.line=element_line()) +
  labs(title = "Dependance as a function of Trust and Condition",
       x = "Trust",
       y = "Dependance") 
print(g7_1)

g7_2 <- ggplot(data_long, aes(x = Trust, y = Reliance, color = as.factor(Reliability), group = as.factor(Reliability))) +
  geom_jitter(size = 0.5, alpha = 0.3) + 
  geom_smooth(method = 'lm', alpha = 0.2) + 
  theme_classic() +
  theme(axis.line=element_line()) +
  labs(title = "Dependance as a function of Trust and Reliability",
       x = "Trust",
       y = "Dependance") +
  facet_wrap(.~Condition)
print(g7_2)


data_long <- data_long %>%
  mutate(ConfidenceQuantile = ntile(Confidence, 3))
data_long$ConfidenceQuantile <- as.factor(data_long$ConfidenceQuantile)
#rename ConfidenceQuartile as low, medium, and high
data_long <- data_long %>%
  mutate(ConfidenceQuantile = case_when(
    ConfidenceQuantile == 3 ~ "High Confidence",
    ConfidenceQuantile == 2 ~ "Medium Confidence",
    ConfidenceQuantile == 1 ~ "Low Confidence",
    TRUE ~ as.factor(ConfidenceQuantile) # This line is optional, to handle unexpected values
  ))
g7_3 <- ggplot(data_long, aes(x = Trust, y = Reliance, color = Reliability_Factor, group = Reliability_Factor)) +
  geom_jitter(size = 0.5, alpha = 0.3) + 
  geom_smooth(method = 'lm', alpha = 0.2) + 
  theme_classic() +
  theme(axis.line=element_line()) +
  labs(title = "",
       x = "Trust",
       y = "Dependance") +
  facet_wrap(~interaction(Condition, ConfidenceQuantile), ncol = 2) 
print(g7_3)

g7_4 <- ggplot(data_long, aes(x=Trust, y=Reliance, color=ConfidenceQuantile, group=ConfidenceQuantile)) +
  geom_smooth(alpha = 0.25, method = 'lm') +
  geom_point(alpha = 0.5) +
  #geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
  theme_classic() +
  labs(title = "Dependance by Trust by Self-Confidence", x = "Trust", y = "Dependance") +
  facet_wrap(.~Condition) +
  ylim(0, 1) #+
  #scale_color_discrete(name = "Self-Confidence Binned",
                       #labels = c("Low", "Medium", "High"))
print(g7_4)

g7_5 <-ggplot(data_long, aes(x=Confidence, y=Reliance, group=Condition, colour=Condition)) +
  geom_smooth(alpha = 0.25, method = 'lm') +
  geom_point(alpha = 0.5) +
  #geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
  theme_classic() +
  labs(title = "Dependance by Trust by Self-Confidence", x = "Confidence", y = "Dependance") +
  ylim(0, 1) +
  scale_color_discrete(name = "Self-Confidence Binned",
                       labels = c("Low", "Medium", "High"))
print(g7_5)


g7_6 <-ggplot(data_long, aes(x=Confidence, y=Reliance, group=ConfidenceQuantile, colour=ConfidenceQuantile)) +
  geom_smooth(alpha = 0.25, method = 'lm') +
  geom_point(alpha = 0.5) +
  #geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
  theme_classic() +
  labs(title = "Dependance by Trust by Self-Confidence", x = "Confidence", y = "Dependance") +
  ylim(0, 1) +
  scale_color_discrete(name = "Self-Confidence Binned",
                       labels = c("Low", "Medium", "High")) +
  facet_wrap(.~Condition)
print(g7_6)

g7_7 <-ggplot(data_long, aes(x=Confidence, y=Reliance)) +
  geom_smooth(alpha = 0.25) +
  geom_point(alpha = 0.5) +
  #geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
  theme_classic() +
  labs(title = "Dependance by Trust by Self-Confidence", x = "Confidence", y = "Dependance") +
  ylim(0, 1) +
  scale_color_discrete(name = "Self-Confidence Binned",
                       labels = c("Low", "Medium", "High")) +
  facet_wrap(.~Condition)
print(g7_7)


g8 <- ggplot(data_long, aes(x = Trust, y = Reliance, color = as.factor(Reliability), group = Reliability)) +
  geom_jitter(size = 0.5, alpha = 0.3) + 
  geom_smooth(method = 'lm', alpha = 0.2) + 
  facet_wrap(.~Condition, scales='free') + 
  theme_classic() +
  theme(axis.line=element_line()) +
  labs(title = "Dependance as a function of Trust and Reliability",
       x = "Trust",
       y = "Dependance") +
  ylim(0, 1) +
  xlim(0, 100)
print(g8)


ggplot(data_long, aes(x=Trust, y=Reliance, color=Confidence, group=Confidence)) + 
  geom_smooth(alpha = 0.25, method = 'lm') + 
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
  theme_classic() +
  labs(title = "Dependance by Trust by Self-Confidence", x = "Trust", y = "Dependance") +
  ylim(0, 1) + 
  xlim(0, 100)

flexplot(Reliance~Trust + Confidence | Condition, data=data_long, method="lm")
#flexplot(Reliance~Trust + Confidence, data=data_long, method="lm")

data_long$Confidence_binned <- cut(data_long$Confidence, breaks = 3)
summary(data_long$Confidence_binned)

ggplot(data_long, aes(x=Trust, y=Reliance, color=Confidence_binned, group=Confidence_binned)) + 
  geom_smooth(alpha = 0.25, method = 'lm') + 
  geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
  theme_classic() +
  labs(title = "Dependance by Trust by Self-Confidence", x = "Trust", y = "Dependance") +
  facet_wrap(.~Condition)

data_long <- data_long %>%
  mutate(ConfidenceQuantile = ntile(Confidence, 3))




data_long$ConfidenceQuantile <- as.factor(data_long$ConfidenceQuantile)
g9 <- ggplot(data_long, aes(x=Trust, y=Reliance, color=ConfidenceQuantile, group=ConfidenceQuantile)) +
  geom_smooth(alpha = 0.25, method = 'lm') +
  geom_point(alpha = 0.5) +
  #geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
  theme_classic() +
  labs(title = "Dependance by Trust by Self-Confidence", x = "Trust", y = "Dependance") +
  facet_wrap(.~Condition) +
  ylim(0, 1) +
  scale_color_discrete(name = "Self-Confidence Binned",
                       labels = c("Low", "Medium", "High"))
print(g9)
ggsave(here('output','figures', '09_Reliance_by_Trust_by_Self-Confidence_Binned_by_Condition.png'), 
       plot = g9, device = device, width = width, height = height, units = units, dpi = dpi)

# dt[, 
#    list(meanRT=mean(RT), sdRT=sd(RT)), 
#    by=.(Condition, Block, Participant)
#    ][, list(meanRT=mean(meanRT), sdRT=mean(sdRT)), 
#       by=.(Condition, Block)]
# 
# dt %>% 
#   group_by(Condition, Block, Participant) %>% 
#   summarise(meanRT = mean(RT), sdRT = sd(RT))

data_long$Reliability_Factor <- as.factor(data_long$Reliability)
flexplot(data = data_long, Reliance ~ Trust + Reliability_Factor| Condition + Confidence, method = "lm")



data_long <- data_long %>%
  mutate(ConfidenceQuantile = ntile(Confidence, 3))
data_long$ConfidenceQuantile <- as.factor(data_long$ConfidenceQuantile)
flexplot(data = data_long, Reliance~ Trust + ConfidenceQuantile, method = "lm")


flexplot(data=data_long, Trust ~ Confidence + Reliability_Factor | Condition, method="lm")
flexplot(data=data_long, Trust ~ Reliability_Factor + ConfidenceQuantile | Condition, method='lm')



