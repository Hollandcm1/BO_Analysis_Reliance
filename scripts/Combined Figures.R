# Combined Performance Figure

library(here)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(sjPlot)

source(here('scripts', 'Other_Functions.R'))
data <- load_processed_data()
data_long <- load_processed_data_long()

# ANOVA
model <- aov(data = data_long, Performance_Difference ~ Condition * Block + Error(Participant / Block))
model_summary <- summary(model)
capture.output(model_summary, file = here('output','tables','08_Performance_Difference_Simple_Anova.txt'))
tab_model(model)

# Figure Parameters
source(here('scripts', 'Figure_Parameters.R'))

dodge_width <- 0.2 # seperation between conditions

# Create Summary Data For Next Figures
summary_data <- data_long %>% # calculate trust
  group_by(Condition, Block) %>%
  summarise(Average_Performance_Before = mean(Performance_Before, na.rm = TRUE),
            se = sd(Performance_Before, na.rm = TRUE) / sqrt(n()))  # Standard Error

g1 <- ggplot(summary_data, aes(x = Block, y = Average_Performance_Before, group = Condition, color = Condition, fill = Condition)) +
  geom_line() +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_ribbon(aes(ymin = Average_Performance_Before - se, ymax = Average_Performance_Before + se),
              alpha = 0.25, position = position_dodge(width = dodge_width)) +
  theme_classic() +
  scale_color_manual(values = c("Decreasing" = colour.decreasing, "Increasing" = colour.increasing)) +
  scale_fill_manual(values = c("Decreasing" = colour.decreasing, "Increasing" = colour.increasing)) +
  labs(title = "Initial Performance",
       x = "Block",
       y = "Performance",
       color = "Condition") +
  ylim(0, 1) + 
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  geom_point(data = data_long, aes(x = Block, y = Performance_Before, color = Condition), 
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = dodge_width), alpha = 0.5) +
  theme(plot.title = element_text(size=title_size)) + 
  # remove legend
  theme(legend.position = "none")
print(g1)


# Create Summary Data For Next Figures
summary_data <- data_long %>% # calculate trust
  group_by(Condition, Block) %>%
  summarise(Average_Performance_After = mean(Performance_After, na.rm = TRUE),
            se = sd(Performance_After, na.rm = TRUE) / sqrt(n()))  # Standard Error

g2 <- ggplot(summary_data, aes(x = Block, y = Average_Performance_After, group = Condition, color = Condition, fill = Condition)) +
  geom_line() +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_ribbon(aes(ymin = Average_Performance_After - se, ymax = Average_Performance_After + se),
              alpha = 0.25, position = position_dodge(width = dodge_width)) +
  theme_classic() +
  scale_color_manual(values = c("Decreasing" = colour.decreasing, "Increasing" = colour.increasing)) +
  scale_fill_manual(values = c("Decreasing" = colour.decreasing, "Increasing" = colour.increasing)) +
  labs(title = "Aided Performance",
       x = "Block",
       y = "Performance",
       color = "Condition") +
  ylim(0, 1) + 
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  geom_point(data = data_long, aes(x = Block, y = Performance_After, color = Condition), 
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = dodge_width), alpha = 0.5) +
  theme(plot.title = element_text(size=title_size))
print(g2)


# Combined Figure
g3 <- ggarrange(g1, g2, ncol = 2, nrow = 1)
print(g3)

# same figure but give more horizontal space to g2
g4 <- ggarrange(g1, g2, ncol = 2, nrow = 1, widths = c(1, 1.5))
print(g4)






