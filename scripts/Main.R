# Main

rm(list = ls())
path <- 'scripts/'
source(paste0(path, '00_Setup.R'))
source(paste0(path, '01_Load_Data.R'))
source(paste0(path, '02_Preprocessing.R'))
source(paste0(path, '03_Dependence.R'))
source(paste0(path, '04_Trust.R'))
source(paste0(path, '05_Performance_Before.R'))
source(paste0(path, '06_Performance_After.R'))
source(paste0(path, '07_Self_Confidence.R'))
source(paste0(path, '08_LME.R'))

