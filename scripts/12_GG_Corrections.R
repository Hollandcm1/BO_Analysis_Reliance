# GG corrections

library(ez)
library(afex)
library(sjstats)

source(here('scripts', 'Other_Functions.R'))
data <- load_processed_data()
data_long <- load_processed_data_long()

#data_long$Block <- as.numeric(data_long$Block)
data_long$Block <- as.factor(data_long$Block)


# Trust
# results <- ezANOVA(data = data_long,
#                    dv = Trust,
#                    wid = Participant,
#                    within = .(Block),
#                    between = .(Condition),
#                    detailed = TRUE,
#                    type = 3)
# print(results)
# 
# anova_table <- results$ANOVA
# corrected_table <- results$'Sphericity Corrections'
# 
# p_value_condition <- anova_table[anova_table$Effect == "Condition", "p"]
# p_value_block <- corrected_table[anova_table$Effect == "Block", "p[GG]"]
# p_value_interaction <- corrected_table[anova_table$Effect == "Condition:Block", "p[GG]"]
# corrected_DFn <- anova_table


results <- aov_ez(id = "Participant", 
                  dv = "Trust", 
                  data = data_long, 
                  within = "Block", 
                  between = "Condition",
                  detailed = TRUE)

print(results)
# summary(results)

effect_sizes <- effectsize::eta_squared(results, partial = TRUE, ci.lvl = NA)  # Set ci.lvl=NA to exclude confidence intervals for faster computation
print(effect_sizes)


# Extracting p-values for the main effects and interactions
# Note: The exact names of the effects would depend on your ezANOVA output.
# You might have "Condition", "Block", "Condition:Block", etc.
p_value_condition <- anova_table[anova_table$Effect == "Condition", "p"]
p_value_block <- anova_table[anova_table$Effect == "Block", "p[GG]"]
p_value_interaction <- anova_table[anova_table$Effect == "Condition:Block", "p"]

# Storing p-values in a vector (if interaction exists, else adjust accordingly)
p_values_trust <- c(p_value_condition, p_value_block, p_value_interaction)

# Repeat similar steps for other dependent variables: Reliance, Performance_Before, etc.

# After collecting all p-values across your analyses, you can adjust them together.
# Example for Bonferroni correction using a combined vector of all extracted p-values
#all_p_values <- c(p_values_trust, p_values_reliance, ... ) # Continue for all variables
all_p_values <- c(p_values_trust)
p_adjusted_bonferroni <- p.adjust(all_p_values, method = "bonferroni")

# Print or use the adjusted p-values as needed
print(p_adjusted_bonferroni)



# Dependence
# results <- ezANOVA(data = data_long, 
#                    dv = Reliance, 
#                    wid = Participant, 
#                    within = .(Block),
#                    between = .(Condition),
#                    detailed = TRUE,
#                    type = 3)
# print(results)
results <- aov_ez(id = "Participant", 
                  dv = "Reliance", 
                  data = data_long, 
                  within = "Block", 
                  between = "Condition",
                  detailed = TRUE)

print(results)
# summary(results)
effect_sizes <- effectsize::eta_squared(results, partial = TRUE, ci.lvl = NA)  # Set ci.lvl=NA to exclude confidence intervals for faster computation
print(effect_sizes)

# Performance_Before
# results <- ezANOVA(data = data_long, 
#                    dv = Performance_Before, 
#                    wid = Participant, 
#                    within = .(Block),
#                    between = .(Condition),
#                    detailed = TRUE,
#                    type = 3)
# print(results)
results <- aov_ez(id = "Participant", 
                  dv = "Performance_Before", 
                  data = data_long, 
                  within = "Block", 
                  between = "Condition",
                  detailed = TRUE)

print(results)
# summary(results)
effect_sizes <- effectsize::eta_squared(results, partial = TRUE, ci.lvl = NA)  # Set ci.lvl=NA to exclude confidence intervals for faster computation
print(effect_sizes)


# Performance_After
# results <- ezANOVA(data = data_long, 
#                    dv = Performance_After, 
#                    wid = Participant, 
#                    within = .(Block),
#                    between = .(Condition),
#                    detailed = TRUE,
#                    type = 3)
# print(results)
results <- aov_ez(id = "Participant", 
                  dv = "Performance_After", 
                  data = data_long, 
                  within = "Block", 
                  between = "Condition",
                  detailed = TRUE)

print(results)
# summary(results)
effect_sizes <- effectsize::eta_squared(results, partial = TRUE, ci.lvl = NA)  # Set ci.lvl=NA to exclude confidence intervals for faster computation
print(effect_sizes)


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
# summary(results)
effect_sizes <- effectsize::eta_squared(results, partial = TRUE, ci.lvl = NA)  # Set ci.lvl=NA to exclude confidence intervals for faster computation
print(effect_sizes)



# Confidence
# results <- ezANOVA(data = data_long, 
#                    dv = Confidence, 
#                    wid = Participant, 
#                    within = .(Block),
#                    between = .(Condition),
#                    detailed = TRUE,
#                    type = 3)
# print(results)
results <- aov_ez(id = "Participant", 
                  dv = "Confidence", 
                  data = data_long, 
                  within = "Block", 
                  between = "Condition",
                  detailed = TRUE)

print(results)
summary(results)
effect_sizes <- effectsize::eta_squared(results, partial = TRUE, ci.lvl = NA)  # Set ci.lvl=NA to exclude confidence intervals for faster computation
print(effect_sizes)


