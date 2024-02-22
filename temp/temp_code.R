# tmp code

# basically just a code dump, ignore for now

hist(data$Trust)
hist(data$Self_Confidence)
hist(data$Reliability, breaks=5)
hist(subset(data, Condition == '50% Increasing')$Reliability, breaks=8)
hist(subset(data, Condition == '70% Increasing')$Reliability, breaks=8)
hist(data$correct1)
hist(data$correct2) # problem that needs to be resolved here
hist(data$block)
hist(data$p_num, breaks=300)
hist(data_by_participant$Avg_Performance, breaks=10)

