# Participant Level Exploration

# All Var Names
names(data_all_conditions_long)

# Single Vars
flexplot(Reliance ~ Participant, data=data_all_conditions_long)
flexplot(Trust ~ Participant, data=data_all_conditions_long)
flexplot(Confidence ~ Participant, data=data_all_conditions_long)
flexplot(Performance_Before ~ Participant, data=data_all_conditions_long)
flexplot(Performance_After ~ Participant, data=data_all_conditions_long)
flexplot(Performance_Difference ~ Participant, data=data_all_conditions_long)

flexplot(Performance_Before ~ Block, data=data_all_conditions_long)
flexplot(Performance_Before ~ Block + Condition, data=data_all_conditions_long)
flexplot(Performance_Before ~ Block + Condition | Trust, data=data_all_conditions_long, method='lm')


# Multiple Vars
ggplot(data_all_conditions_long, aes(x=Confidence, y=Trust, color=Participant, group=Participant)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(title = "Trust by Confidence", x = "Confidence", y = "Trust") + 
  theme_classic() + 
  theme(legend.position = "none") + 
  facet_wrap(.~Condition)

ggplot(data_all_conditions_long, aes(x=Confidence, y=Reliance, color=Participant, group=Participant)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Reliance by Confidence", x = "Confidence", y = "Reliance") +
  theme_classic() +
  theme(legend.position = "none") +
  facet_wrap(.~Condition)

ggplot(data_all_conditions_long, aes(x=Trust, y=Reliance, color=Participant, group=Participant)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Reliance by Trust", x = "Trust", y = "Reliance") +
  theme_classic() +
  theme(legend.position = "none") +
  facet_wrap(.~Condition)

data_all_conditions_long$Reliability_Factor <- as.factor(data_all_conditions_long$Reliability)
ggplot(data_all_conditions_long, aes(x=Trust, y=Reliance, color=Reliability_Factor, group=Reliability_Factor)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Reliance by Trust by Reliability", x = "Trust", y = "Reliance") +
  theme_classic() +
  #theme(legend.position = "none") +
  facet_wrap(.~Condition)


# flexplot(Performance_Before ~ Block + Participant, data=data_all_conditions_long, method='lm', facet.by='Participant')
ggplot(data_all_conditions_long, aes(x=Block, y=Performance_Before, color=Participant, group=Participant)) +
  geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  geom_line() +
  labs(title = "Performance by Block", x = "Block", y = "Performance") +
  theme_classic() +
  theme(legend.position = "none") +
  facet_wrap(.~Condition)
