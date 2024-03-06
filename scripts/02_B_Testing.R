# Preprocessing

# Load data
if (!exists("dat")) {
  dat <- read.csv(here('data', 'raw', 'Reliance_Data.csv'), header = TRUE)
} else {
  message("data already loaded")
  if (exists("dat_original")) {
    dat_original <- dat
    message("reset dat to original data")
  }
}

# Filter conditions and convert to a datframe
dat <- dat %>%
  filter(condition <= 2) %>%
  mutate(totalPerformance = rowSums(select(., starts_with("p") & ends_with("_before")), na.rm = TRUE)) %>%
  filter(totalPerformance > 170, !is.na(t1)) %>%
  mutate(condition = case_when(
    condition == 1 ~ "Decreasing",
    condition == 2 ~ "Increasing",
    TRUE ~ as.character(condition)
  )) %>%
  rename(Participant = participant.number, Condition = condition) %>%
  mutate(Condition = as.factor(Condition), Participant = as.factor(Participant))

# Transform data to long format efficiently
dat_long <- dat %>%
  mutate(across(c(starts_with("r"), starts_with("t"), matches("^p\\d+_before$"), matches("^p\\d+_after$"), starts_with('c'), starts_with('a'), Condition), as.character)) %>%
  pivot_longer(cols = c(starts_with("r"), starts_with("t"), matches("^p\\d+_before$"), matches("^p\\d+_after$"), starts_with('c'), starts_with('a')),
               names_to = "Measure", values_to = "Value") %>%
  separate(Measure, into = c("Type", "Block"), sep = 1) %>%
  pivot_wider(names_from = Type, values_from = Value) %>%
  mutate(Block = as.numeric(gsub("\\D", "", Block)),
         Performance_Before = as.numeric(Performance_Before) / 50,
         Performance_After = as.numeric(Performance_After) / 50,
         Performance_Difference = Performance_After - Performance_Before)

# Store processed dat
write.csv(dat, here('dat', 'processed', 'processed_dat.csv'), row.names = FALSE)
write.csv(dat_long, here('dat', 'processed', 'processed_dat_long.csv'), row.names = FALSE)