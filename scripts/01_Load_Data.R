# Load Data
file_name <- 'Reliance_Data.csv'
file_path <- here('data', 'raw', file_name) # construct the path to the file
data <- read.csv(file_path, header = TRUE)

# Copy the original dataset for later use
# Useful for operations where you need an untouched version of the dataset
data_original <- data

