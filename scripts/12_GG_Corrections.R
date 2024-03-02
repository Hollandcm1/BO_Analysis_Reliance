# GG corrections

library(ez)
library(afex)
library(sjstats)

source(here('scripts', 'Other_Functions.R'))
data <- load_processed_data()
data_long <- load_processed_data_long()

#data_long$Block <- as.numeric(data_long$Block)
data_long$Block <- as.factor(data_long$Block)

numver_of_tests <- 6 # for bonferroni correction

#############
### Trust ###
#############
results <- aov_ez(id = "Participant", 
                  dv = "Trust", 
                  data = data_long, 
                  within = "Block", 
                  between = "Condition",
                  detailed = TRUE)

print(results)
capture.output(results, file = here('output','tables','12_Trust_GG_Anova.txt'))
# summary(results)

effect_sizes <- effectsize::eta_squared(results, partial = TRUE, ci.lvl = NA)  # Set ci.lvl=NA to exclude confidence intervals for faster computation
print(effect_sizes)
capture.output(effect_sizes, file = here('output','tables','12_Trust_Effect_Sizes.txt'))


##################
### Dependence ###
##################
results <- aov_ez(id = "Participant", 
                  dv = "Reliance", 
                  data = data_long, 
                  within = "Block", 
                  between = "Condition",
                  detailed = TRUE)

print(results)
capture.output(results, file = here('output','tables','12_Dependence_GG_Anova.txt'))
# summary(results)

effect_sizes <- effectsize::eta_squared(results, partial = TRUE, ci.lvl = NA)  # Set ci.lvl=NA to exclude confidence intervals for faster computation
print(effect_sizes)
capture.output(effect_sizes, file = here('output','tables','12_Dependence_Effect_Sizes.txt'))


##########################
### Performance_Before ###
##########################
results <- aov_ez(id = "Participant", 
                  dv = "Performance_Before", 
                  data = data_long, 
                  within = "Block", 
                  between = "Condition",
                  detailed = TRUE)

print(results)
capture.output(results, file = here('output','tables','12_Performance_Before_GG_Anova.txt'))
# summary(results)

effect_sizes <- effectsize::eta_squared(results, partial = TRUE, ci.lvl = NA)  # Set ci.lvl=NA to exclude confidence intervals for faster computation
print(effect_sizes)
capture.output(effect_sizes, file = here('output','tables','12_Performance_Before_Effect_Sizes.txt'))


#########################
### Performance_After ###
#########################
results <- aov_ez(id = "Participant", 
                  dv = "Performance_After", 
                  data = data_long, 
                  within = "Block", 
                  between = "Condition",
                  detailed = TRUE)

print(results)
capture.output(results, file = here('output','tables','12_Performance_After_GG_Anova.txt'))
# summary(results)

effect_sizes <- effectsize::eta_squared(results, partial = TRUE, ci.lvl = NA)  # Set ci.lvl=NA to exclude confidence intervals for faster computation
print(effect_sizes)
capture.output(effect_sizes, file = here('output','tables','12_Performance_After_Effect_Sizes.txt'))


# Performance_Difference
# results <- ezANOVA(data = data_long, 
#                    dv = Performance_Difference, 
#                    wid = Participant, 
#                    within = .(Block),
#                    between = .(Condition),
#                    detailed = TRUE,
#                    type = 3)
# print(results)
results <- aov_ez(id = "Participant", 
                  dv = "Performance_Difference", 
                  data = data_long, 
                  within = "Block", 
                  between = "Condition",
                  detailed = TRUE)

print(results)
capture.output(results, file = here('output','tables','12_Performance_Difference_GG_Anova.txt'))
# summary(results)

effect_sizes <- effectsize::eta_squared(results, partial = TRUE, ci.lvl = NA)  # Set ci.lvl=NA to exclude confidence intervals for faster computation
print(effect_sizes)
capture.output(effect_sizes, file = here('output','tables','12_Performance_Difference_Effect_Sizes.txt'))


##################
### Confidence ###
##################
results <- aov_ez(id = "Participant", 
                  dv = "Confidence", 
                  data = data_long, 
                  within = "Block", 
                  between = "Condition",
                  detailed = TRUE)

print(results)
capture.output(results, file = here('output','tables','12_Confidence_GG_Anova.txt'))
# summary(results)

effect_sizes <- effectsize::eta_squared(results, partial = TRUE, ci.lvl = NA)  # Set ci.lvl=NA to exclude confidence intervals for faster computation
print(effect_sizes)
capture.output(effect_sizes, file = here('output','tables','12_Confidence_Effect_Sizes.txt'))


