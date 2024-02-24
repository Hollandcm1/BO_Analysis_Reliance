# Load Data

filename <- 'Reliance_Data.csv'
data <- read.csv(here('data', 'raw', filename), header = TRUE)

# used for accessing all conditions later
data_original <- data
