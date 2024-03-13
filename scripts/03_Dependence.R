# Dependence

source(here('scripts', 'Other_Functions.R'))
data <- load_processed_data()
data_long <- load_processed_data_long()
data_long$Block <- as.numeric(data_long$Block)

# figure size and characteristics
source(here('scripts', 'Figure_Parameters.R'))

# ANOVA
model <- aov(data = data_long, Reliance ~ Condition * Block + Error(Participant / Block))
model_summary <- summary(model)
capture.output(model_summary, file = here('output','tables','03_Dependence_Simple_Anova.txt'))
tab_model(model) # Sig. Interaction

# Visualize Interaction
g1 <- ggplot(data_long, aes(x=Block, y=Reliance, color=Condition, group=Condition)) + 
  geom_smooth(alpha = 0.25) + 
  geom_jitter(width = 0.2, height = 0) +
  theme_classic() +
  labs(title = "Dependence by Block by Condition", x = "Block", y = "Dependence") +
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  scale_color_manual(values = c("Decreasing" = colour.decreasing, "Increasing" = colour.increasing)) +
  theme(plot.title = element_text(size=title_size))
print(g1)
ggsave(here('output','figures','03_Dependence_by_Block_by_Condition.png'), 
       plot = g1, device = device, width = width, height = height, units = units, dpi = dpi)

# Create Summary Data For Next Figures
summary_data <- data_long %>% # calculate relaince
  group_by(Condition, Block) %>%
  summarise(Average_Reliance = mean(Reliance, na.rm = TRUE),
            se = sd(Reliance, na.rm = TRUE) / sqrt(n()))  # Standard Error

# Visualize Interaction with Standard Error
dodge_width <- 0.2 # seperation between conditions
g2 <- ggplot(summary_data, aes(x = Block, y = Average_Reliance, group = Condition, color = Condition)) +
  geom_line() +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_errorbar(aes(ymin = Average_Reliance - se, ymax = Average_Reliance + se), 
                width = 0.4, position = position_dodge(width = dodge_width)) +
  theme_classic() +
  scale_color_manual(values = c("Decreasing" = colour.decreasing, "Increasing" = colour.increasing)) +
  labs(title = "Average Dependence across Conditions with Standard Error",
       x = "Block",
       y = "Dependence",
       color = "Condition") +
  ylim(0, 1) + 
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  geom_point(data = data_long, aes(x = Block, y = Reliance, color = Condition), 
               position = position_jitterdodge(jitter.width = 0.2, dodge.width = dodge_width), alpha = 0.5) +
  theme(plot.title = element_text(size=title_size))
print(g2)
ggsave(here('output','figures','03_Dependence_by_Block_by_Condition_with_SE.png'), 
       plot = g2, device = device, width = width, height = height, units = units, dpi = dpi)

# Visualize Interaction with Standard Error as Ribbon
dodge_width <- 0.2 # seperation between conditions
g3 <- ggplot(summary_data, aes(x = Block, y = Average_Reliance, group = Condition, color = Condition, fill = Condition)) +
  geom_line() +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_ribbon(aes(ymin = Average_Reliance - se, ymax = Average_Reliance + se),
              alpha = 0.1, position = position_dodge(width = dodge_width)) +
  theme_classic() +
  scale_color_manual(values = c("Decreasing" = colour.decreasing, "Increasing" = colour.increasing)) +
  scale_fill_manual(values = c("Decreasing" = colour.decreasing, "Increasing" = colour.increasing)) +
  labs(title = "Average Dependence across Conditions with Standard Error",
       x = "Block",
       y = "Dependence",
       color = "Condition") +
  ylim(0, 1) + 
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  geom_point(data = data_long, aes(x = Block, y = Reliance, color = Condition), 
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = dodge_width), alpha = 0.5) +
  theme(plot.title = element_text(size=title_size))
print(g3)
ggsave(here('output','figures','03_Dependence_by_Block_by_Condition_with_SE_as_Ribbon.png'), 
       plot = g3, device = device, width = width, height = height, units = units, dpi = dpi)
