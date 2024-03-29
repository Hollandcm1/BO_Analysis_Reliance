# Other Functions

load_processed_data <- function() {
  # Load processed data
  data <- read.csv(here("data", "processed", "processed_data.csv"))
  # Correct Factors
  data$Participant <- as.factor(data$Participant)
  data$Condition <- as.factor(data$Condition)
  
  return(data)
}

load_processed_data_long <- function() {
  # Load processed data
  data_long <- read.csv(here("data", "processed", "processed_data_long.csv"))
  # Correct Factors
  data_long$Participant <- as.factor(data_long$Participant)
  data_long$Condition <- as.factor(data_long$Condition)
  #data_long$Block <- as.factor(data_long$Block)
  
  return(data_long)
}

load_processed_data_all_conditions <- function() {
  # Load processed data
  data <- read.csv(here("data", "processed", "processed_data_all_conditions.csv"))
  # Correct Factors
  data$Participant <- as.factor(data$Participant)
  data$Condition <- as.factor(data$Condition)
  
  return(data)
}

load_processed_data_all_conditions_long <- function() {
  # Load processed data
  data_long <- read.csv(here("data", "processed", "processed_data_all_conditions_long.csv"))
  # Correct Factors
  data_long$Participant <- as.factor(data_long$Participant)
  data_long$Condition <- as.factor(data_long$Condition)
  #data_long$Block <- as.factor(data_long$Block)
  
  return(data_long)
}