# Normality Checks

# Load data
source(here('scripts', 'Other_Functions.R'))
data <- load_processed_data()
data_long <- load_processed_data_long()

# Shapiro-Wilk Test
shapiro.test(data_long$Trust) # Not Normally Distributed
shapiro.test(data_long$Confidence) # Not Normally Distributed
shapiro.test(data_long$Performance_Before) # Not Normally Distributed
shapiro.test(data_long$Performance_After) # Not Normally Distributed
shapiro.test(data_long$Performance_Difference) # Not Normally Distributed
shapiro.test(data_long$Reliance) # Not Normally Distributed

# Nothing is considered normally distributed, so GGe correction will be used 
# pretty much everywhere. This doesn't strictly solve the problem, but at least
# it's a more conservative approach to the problem.

################################
### Visually check normality ###
################################

# Trust
qqnorm(data_long$Trust)
qqline(data_long$Trust)
hist(data_long$Trust, breaks = 20)
# possible multi modal distribution? 
# There is likely some anchoring to 100

# Confidence
qqnorm(data_long$Confidence)
qqline(data_long$Confidence)
hist(data_long$Confidence, breaks = 20)
# possible multi modal distribution?
# There is likely some anchoring to 100

# Performance_Before
qqnorm(data_long$Performance_Before)
qqline(data_long$Performance_Before)
hist(data_long$Performance_Before, breaks = 30)
# Hoenstly kinda suprised this wasn't deemed normal. Looks pretty normal to me.

# Performance_After
qqnorm(data_long$Performance_After)
qqline(data_long$Performance_After)
hist(data_long$Performance_After, breaks = 30)
# This is hinting at possible dropout at a certain point, where participants 
# just stop caring (high peak around 0.5)
# There also seems to be some people that are performing in the opposite
# direction of what they should be? (<0.5)

# Performance_Difference
qqnorm(data_long$Performance_Difference)
qqline(data_long$Performance_Difference)
hist(data_long$Performance_Difference, breaks = 50)
# This is probably hitning at people who were not willing to change their opinions 
# ever. Not totally sure if we could do this but maybe could pull a subset 
# of people that never changed their behavior? Maybe they act differently than
# other people? 

# Reliance
qqnorm(data_long$Reliance)
qqline(data_long$Reliance)
hist(data_long$Reliance, breaks = 20)
# Already did this and ya there is strong anchoring to 0 or 1