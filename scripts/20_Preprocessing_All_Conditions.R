# Preprocessing All Conditions

# Very similar to the original preprocessing script, but now we are using 
# all conditions. Note that once this is run, it saves the data so that it
# can be accessed again withhout running this script.

# Origianl data
source(here('scripts', '01_Load_Data.R'))

data_all_conditions <- data_original

# Remove uneeded data
# data_all_conditions <- filter(data_all_conditions, condition <= 2) # condition 1 and 2 are 50% decreasing
data_all_conditions <- as.data.frame(data_all_conditions)

# Remove poorly performing participants
data_all_conditions <- mutate(data_all_conditions, totalPerformance = rowSums(select(data_all_conditions, p1_before:p6_before), na.rm = TRUE)) # calculate total performance
data_all_conditions <- na.omit(data_all_conditions[data_all_conditions$totalPerformance > 170, ]) # removes participants not able to achieve 170 correct trials

# Remove data that isn't working properly
data_all_conditions = na.omit(data_all_conditions[!is.na(data_all_conditions$t1), ]) # removes participants with NA in t1

# Data Renaming
data_all_conditions <- data_all_conditions %>%
  mutate(condition = case_when(
    condition == 1 ~ "50% Decreasing", # Relabel condition 1
    condition == 2 ~ "50% Increasing", # Relabel condition 2
    condition == 3 ~ "70% Decreasing", # Relabel condition 3
    condition == 4 ~ "70% Increasing", # Relabel condition 4
    TRUE ~ as.character(condition)  # Handles any other unforeseen values gracefully
  ))
data_all_conditions <- rename(data_all_conditions, Participant = participant.number)
data_all_conditions <- rename(data_all_conditions, Condition = condition)

# Data Formatting
data_all_conditions$Condition <- as.factor(data_all_conditions$Condition)
data_all_conditions$Participant <- as.factor(data_all_conditions$Participant)

# Long Format Data
data_all_conditions_long <- data_all_conditions %>%
  select(Participant, Condition, r1, r2, r3, r4, r5, r6) %>% # sel
  pivot_longer(cols = starts_with("r"), # find a r1:r6
               names_to = "Block", # put in Block column
               values_to = "Reliance") # values to Reliance column
tmp <- data_all_conditions %>% 
  select(-totalPerformance) %>% # remove unneeded columns)
  pivot_longer(cols = starts_with("t"), # find a t1:t6
               names_to = "Block", # put in Block column
               values_to = "Trust") # values to Trust column
data_all_conditions_long$Trust <- tmp$Trust # add Trust to data_all_conditions_long
tmp <- data_all_conditions %>%
  select(-Participant) %>% # remove unneeded columns
  pivot_longer(cols =  matches("^p\\d+_before$"), # find a p1_before:p6_before
               names_to = "Block", # put in Block column
               values_to = "Performance_Before") # values to Performance column
data_all_conditions_long$Performance_Before <- tmp$Performance_Before # add Performance_Before to data_all_conditions_long
tmp <- data_all_conditions %>%
  select(-Participant) %>% # remove unneeded columns
  pivot_longer(cols =  matches("^p\\d+_after$"), # find a p1_after:p6_after
               names_to = "Block", # put in Block column
               values_to = "Performance_After") # values to Performance column
data_all_conditions_long$Performance_After <- tmp$Performance_After # add Performance_After to data_all_conditions_long
tmp <- data_all_conditions %>%
  select(-Condition) %>% # remove unneeded columns
  pivot_longer(cols = starts_with('c'), # find a c1:c6
               names_to = "Block", # put in Block column
               values_to = "Confidence") # values to Confidence column)
data_all_conditions_long$Confidence <- tmp$Confidence # add Confidence to data_all_conditions_long
tmp <- data_all_conditions %>%
  pivot_longer(cols = starts_with('a'),
               names_to = "Block",
               values_to = "Reliability")
data_all_conditions_long$Reliability <- tmp$Reliability
rm(tmp)
data_all_conditions_long <- data_all_conditions_long %>% # replace r1:r6 with just numbers
  mutate(Block = case_when(
    Block == "r1" ~ '1', 
    Block == "r2" ~ '2', 
    Block == "r3" ~ '3', 
    Block == "r4" ~ '4', 
    Block == "r5" ~ '5', 
    Block == "r6" ~ '6', 
    TRUE ~ as.character(Condition)  # Handles any other unforeseen values gracefully
  ))
data_all_conditions_long$Block <- as.numeric(data_all_conditions_long$Block)
data_all_conditions_long$Performance_Before <- data_all_conditions_long$Performance_Before / 50
data_all_conditions_long$Performance_After <- data_all_conditions_long$Performance_After / 50

# Calculate Performance Difference
data_all_conditions_long$Performance_Difference <- data_all_conditions_long$Performance_After - data_all_conditions_long$Performance_Before

# New data
# str(data)
# str(data_long)

# Store processed data_all_conditions
write.csv(data_all_conditions, here('data', 'processed', 'processed_data_all_conditions.csv'), row.names = FALSE)
write.csv(data_all_conditions_long, here('data', 'processed', 'processed_data_all_conditions_long.csv'), row.names = FALSE)



