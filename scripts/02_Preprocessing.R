# Preprocessing


# Remove uneeded data
data <- filter(data, condition <= 2) # condition 1 and 2 are 50% decreasing
data <- as.data.frame(data)

# Remove poorly performing participants
data <- mutate(data, totalPerformance = rowSums(select(data, p1_before:p6_before), na.rm = TRUE)) # calculate total performance
data <- na.omit(data[data$totalPerformance > 170, ]) # removes participants not able to achieve 170 correct trials

# Remove data that isn't working properly
data = na.omit(data[!is.na(data$t1), ]) # removes participants with NA in t1

# Data Renaming
data <- data %>%
  mutate(condition = case_when(
    condition == 1 ~ "Decreasing", # Relabel condition 1
    condition == 2 ~ "Increasing", # Relabel condition 2
    TRUE ~ as.character(condition)  # Handles any other unforeseen values gracefully
  ))
data <- rename(data, Participant = participant.number)
data <- rename(data, Condition = condition)

# Data Formatting
data$Condition <- as.factor(data$Condition)
data$Participant <- as.factor(data$Participant)

# Long Format Data
data_long <- data %>%
  select(Participant, Condition, r1, r2, r3, r4, r5, r6) %>% # sel
  pivot_longer(cols = starts_with("r"), # find a r1:r6
               names_to = "Block", # put in Block column
               values_to = "Reliance") # values to Reliance column
tmp <- data %>% 
  select(-totalPerformance) %>% # remove unneeded columns)
  pivot_longer(cols = starts_with("t"), # find a t1:t6
               names_to = "Block", # put in Block column
               values_to = "Trust") # values to Trust column
data_long$Trust <- tmp$Trust # add Trust to data_long
tmp <- data %>%
  select(-Participant) %>% # remove unneeded columns
  pivot_longer(cols =  matches("^p\\d+_before$"), # find a p1_before:p6_before
               names_to = "Block", # put in Block column
               values_to = "Performance_Before") # values to Performance column
data_long$Performance_Before <- tmp$Performance_Before # add Performance_Before to data_long
tmp <- data %>%
  select(-Participant) %>% # remove unneeded columns
  pivot_longer(cols =  matches("^p\\d+_after$"), # find a p1_after:p6_after
               names_to = "Block", # put in Block column
               values_to = "Performance_After") # values to Performance column
data_long$Performance_After <- tmp$Performance_After # add Performance_After to data_long
tmp <- data %>%
  select(-Condition) %>% # remove unneeded columns
  pivot_longer(cols = starts_with('c'), # find a c1:c6
               names_to = "Block", # put in Block column
               values_to = "Confidence") # values to Confidence column)
data_long$Confidence <- tmp$Confidence # add Confidence to data_long
tmp <- data %>%
  pivot_longer(cols = starts_with('a'),
               names_to = "Block",
               values_to = "Reliability")
data_long$Reliability <- tmp$Reliability
rm(tmp)
data_long <- data_long %>% # replace r1:r6 with just numbers
  mutate(Block = case_when(
    Block == "r1" ~ '1', 
    Block == "r2" ~ '2', 
    Block == "r3" ~ '3', 
    Block == "r4" ~ '4', 
    Block == "r5" ~ '5', 
    Block == "r6" ~ '6', 
    TRUE ~ as.character(Condition)  # Handles any other unforeseen values gracefully
  ))
data_long$Block <- as.numeric(data_long$Block)
data_long$Performance_Before <- data_long$Performance_Before / 50
data_long$Performance_After <- data_long$Performance_After / 50

# Calculate Performance Difference
data_long$Performance_Difference <- data_long$Performance_After - data_long$Performance_Before

# New data
# str(data)
# str(data_long)

# Store processed data
write.csv(data, here('data', 'processed', 'processed_data.csv'), row.names = FALSE)
write.csv(data_long, here('data', 'processed', 'processed_data_long.csv'), row.names = FALSE)



