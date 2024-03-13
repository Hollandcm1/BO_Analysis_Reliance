# All figures

source(here('scripts', '00_Setup.R'))
source(here('scripts', '01_Load_Data.R'))
source(here('scripts', '20_Preprocessing_All_Conditions.R'))

# Trust Histogram
flexplot(Trust~1, data=data_all_conditions_long)

# Confidence Histogram
flexplot(Confidence~1, data=data_all_conditions_long)

# Reliance Histogram
flexplot(Reliance~1, data=data_all_conditions_long)

# Performance_Before Histogram
flexplot(Performance_Before~1, data=data_all_conditions_long)

# Performance_After Histogram
flexplot(Performance_After~1, data=data_all_conditions_long)

# Trust Histogram
flexplot(Trust~1 | Condition, data=data_all_conditions_long)

# Trust by Confidence
flexplot(Trust ~ Confidence, data=data_all_conditions_long, method='lm')

# Trust by Dependence
flexplot(Reliance ~ Trust, data=data_all_conditions_long, method='lm')

# Trust by Performance_Before
flexplot(Performance_Before ~ Trust, data=data_all_conditions_long, method='lm')

# Trust by Performance_After
flexplot(Performance_After ~ Trust, data=data_all_conditions_long, method='lm')

# Trust by Block
flexplot(Trust ~ Block, data=data_all_conditions_long, method='lm')

# Trust by Reliability
flexplot(Trust ~ Reliability, data=data_all_conditions_long, method='lm')

# Trust by Participant
flexplot(Trust ~ Participant, data=data_all_conditions_long, method='lm')

# Trust by Condition
flexplot(Trust ~ Condition, data=data_all_conditions_long, method='lm')

# Confidence by Dependence
flexplot(Reliance ~ Confidence, data=data_all_conditions_long, method='lm')

# Confidence by Performance_Before
flexplot(Performance_Before ~ Confidence, data=data_all_conditions_long, method='lm')

# Confidence by Performance_After
flexplot(Performance_After ~ Confidence, data=data_all_conditions_long, method='lm')

# Confidence by Block
flexplot(Confidence ~ Block, data=data_all_conditions_long, method='lm')

# Confidence by Reliability
flexplot(Confidence ~ Reliability, data=data_all_conditions_long, method='lm')

# Confidence by Participant
flexplot(Confidence ~ Participant, data=data_all_conditions_long, method='lm')

# Confidence by Condition
flexplot(Confidence ~ Condition, data=data_all_conditions_long, method='lm')

# Dependence by Performance_Before
flexplot(Performance_Before ~ Reliance, data=data_all_conditions_long, method='lm')

# Dependence by Performance_After
flexplot(Performance_After ~ Reliance, data=data_all_conditions_long, method='lm')

# Dependence by Block
flexplot(Reliance ~ Block, data=data_all_conditions_long, method='lm')

# Dependence by Reliability
flexplot(Reliance ~ Reliability, data=data_all_conditions_long, method='lm')

# Dependence by Participant
flexplot(Reliance ~ Participant, data=data_all_conditions_long, method='lm')

# Dependence by Condition
flexplot(Reliance ~ Condition, data=data_all_conditions_long, method='lm')

# Performance_Before by Performance_After
flexplot(Performance_After ~ Performance_Before, data=data_all_conditions_long, method='lm')

# Performance_Before by Block
flexplot(Performance_Before ~ Block, data=data_all_conditions_long, method='lm')

# Performance_Before by Reliability
flexplot(Performance_Before ~ Reliability, data=data_all_conditions_long, method='lm')

# Performance_Before by Participant
flexplot(Performance_Before ~ Participant, data=data_all_conditions_long, method='lm')

# Performance_Before by Condition
flexplot(Performance_Before ~ Condition, data=data_all_conditions_long, method='lm')

# Block by Performance_After
flexplot(Performance_After ~ Block, data=data_all_conditions_long, method='lm')

# Reliability by Performance_After
flexplot(Performance_After ~ Reliability, data=data_all_conditions_long, method='lm')

# Performance_After by Condition
flexplot(Performance_After ~ Condition, data=data_all_conditions_long, method='lm')

# Performance_After by Participant
flexplot(Performance_After ~ Participant, data=data_all_conditions_long, method='lm')

# Trust by Confidence by Condition
flexplot(Trust ~ Confidence + Condition, data=data_all_conditions_long, method='lm', se=TRUE)

# Trust by Reliability by Condition
flexplot(Trust ~ Reliability + Condition, data=data_all_conditions_long, method='lm', se=TRUE)

# Trust by Block by Condition
flexplot(Trust ~ Block + Condition, data=data_all_conditions_long, method='lm', se=TRUE)

# Trust by Performance_Before by Condition
flexplot(Performance_Before ~ Trust + Condition, data=data_all_conditions_long, method='lm', se=TRUE)

# Trust by Performance_After by Condition
flexplot(Performance_After ~ Trust + Condition, data=data_all_conditions_long, method='lm', se=TRUE)

# Trust by Reliance by Condition
flexplot(Reliance ~ Trust + Condition, data=data_all_conditions_long, method='lm', se=TRUE)

# Trust by Participant by Condition
flexplot(Trust ~ Participant | Condition, data=data_all_conditions_long)

# Confidence by Dependence by Condition
flexplot(Reliance ~ Confidence + Condition, data=data_all_conditions_long, method='lm', se=TRUE)

# Confidence by Performance_Before by Condition
flexplot(Performance_Before ~ Confidence + Condition, data=data_all_conditions_long, method='lm', se=TRUE)

# Confidence by Block by Condition
flexplot(Confidence ~ Block + Condition, data=data_all_conditions_long, method='lm', se=TRUE)

# Confidence by Reliability by Condition
flexplot(Confidence ~ Reliability + Condition, data=data_all_conditions_long, method='lm', se=TRUE)

# Confidence by Performance_After by Condition
flexplot(Performance_After ~ Confidence + Condition, data=data_all_conditions_long, method='lm', se=TRUE)

# Confidence by Participant by Condition
flexplot(Confidence ~ Participant | Condition, data=data_all_conditions_long)

# Dependence by Performance_Before by Condition
flexplot(Performance_Before ~ Reliance + Condition, data=data_all_conditions_long, method='lm', se=TRUE)

# Dependence by Block by Condition
flexplot(Reliance ~ Block + Condition, data=data_all_conditions_long, method='lm', se=TRUE)

# Dependence by Reliability by Condition
flexplot(Reliance ~ Reliability + Condition, data=data_all_conditions_long, method='lm', se=TRUE)

# Dependence by Performance_After by Condition
flexplot(Performance_After ~ Reliance + Condition, data=data_all_conditions_long, method='lm', se=TRUE)

# Dependence by Participant by Condition
flexplot(Reliance ~ Participant | Condition, data=data_all_conditions_long)

# Performance_Before by Performance_After by Condition
flexplot(Performance_After ~ Performance_Before + Condition, data=data_all_conditions_long, method='lm', se=TRUE)

# Performance_Before by Block by Condition
flexplot(Performance_Before ~ Block + Condition, data=data_all_conditions_long, method='lm', se=TRUE)

# Performance_Before by Reliability by Condition
flexplot(Performance_Before ~ Reliability + Condition, data=data_all_conditions_long, method='lm', se=TRUE)

# Performance_Before by Participant by Condition
flexplot(Performance_Before ~ Participant | Condition, data=data_all_conditions_long)

# Block by Performance_After by Condition
flexplot(Performance_After ~ Block + Condition, data=data_all_conditions_long, method='lm', se=TRUE)

