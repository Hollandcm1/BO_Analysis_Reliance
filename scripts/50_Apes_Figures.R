
source(here('scripts', 'Other_Functions.R'))
data <- load_processed_data_all_conditions()
data_long <- load_processed_data_all_conditions_long()

# Figure Parameters
source(here('scripts', 'Figure_Parameters.R'))

# Create Summary Data For Next Figures
summary_data <- data_long %>% # calculate trust
  group_by(Condition, Block) %>%
  summarise(Average_Trust = mean(Trust, na.rm = TRUE),
            se = sd(Trust, na.rm = TRUE) / sqrt(n()))  # Standard Error

dodge_width <- 0.2
ggplot(summary_data, aes(x=Block, y=Average_Trust, color=Condition)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  #geom_errorbar(aes(ymin=Average_Trust-se, ymax=Average_Trust+se), width=0.5, position = position_dodge(width = dodge_width)) +
  #geom_line() +
  geom_smooth(method='lm', alpha=0.2) +
  theme_classic() +
  labs(title="Trust by Block and Condition", x="Block", y="Trust") +
  ylim(75, 85) 

ggplot(data_long, aes(x=Block, y=Reliance, color=Condition)) +
  geom_jitter(alpha=0.4, height=0, width=0.2) +
  geom_smooth(method="lm", se=TRUE, alpha = 0.2) +
  theme_classic() +
  #theme(legend.position = "none") +
  labs(x="Trial Progression", y="Reliance") +
  scale_x_continuous(breaks = 1:6, labels=c("0", "50", "100", "150", "200", "250")) +
  theme(axis.text.x = element_text(size = 12), # Increase x-axis label font size
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12), # Increase x-axis title font size
        axis.title.y = element_text(size = 12)) # Increase y-axis label font size if needed

ggplot(data_long, aes(x=Reliance, y=Performance_After, color=Condition)) +
  #geom_jitter(alpha=0.4, height=0.01, width=0.01) +
  geom_point(alpha=0.4) +
  geom_smooth(method="lm", se=FALSE, alpha = 0.2) +
  theme_classic() +
  #theme(legend.position = "none") +
  labs(x="Dependance", y="Overall Performance") +
  theme(axis.text.x = element_text(size = 12), # Increase x-axis label font size
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12), # Increase x-axis title font size
        axis.title.y = element_text(size = 12)) # Increase y-axis label font size if needed

ggplot(data_long, aes(x=Reliability, y=Reliance, color=Condition)) +
  geom_jitter(alpha=0.4, height=0.01, width=1.5) +
  #geom_point(alpha=0.4) +
  geom_smooth(method="lm", se=FALSE, alpha = 0.2) +
  theme_classic() +
  facet_wrap(~Condition) +
  #theme(legend.position = "none") +
  labs(x="Reliability", y="Dependance") +
  theme(axis.text.x = element_text(size = 12), # Increase x-axis label font size
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12), # Increase x-axis title font size
        axis.title.y = element_text(size = 12)) # Increase y-axis label font size if needed

ggplot(data_long, aes(x=Reliability, y=Reliance, color=Condition)) +
  geom_jitter(alpha=0.4, height=0.01, width=1.5) +
  #geom_point(alpha=0.4) +
  geom_smooth(method="lm", se=FALSE, alpha = 0.2) +
  theme_classic() +
  #facet_wrap(~Condition) +
  #theme(legend.position = "none") +
  labs(x="Reliability of Automation (%)", y="Dependance") +
  theme(axis.text.x = element_text(size = 12), # Increase x-axis label font size
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12), # Increase x-axis title font size
        axis.title.y = element_text(size = 12)) # I

ggplot(data_long, aes(x=Trust, y=Reliance, color=Condition)) +
  geom_jitter(alpha=0.4, height=0.01, width=1.5) +
  #geom_point(alpha=0.4) +
  geom_smooth(method="lm", se=FALSE, alpha = 0.2) +
  theme_classic() +
  #facet_wrap(~Condition) +
  #theme(legend.position = "none") +
  labs(x="Trust in Automation", y="Reliance") +
  theme(axis.text.x = element_text(size = 12), # Increase x-axis label font size
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12), # Increase x-axis title font size
        axis.title.y = element_text(size = 12)) # Increase y-axis label font size if needed

