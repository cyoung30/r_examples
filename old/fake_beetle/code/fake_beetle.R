


### test for learning things!
# Carter Young
# Jan 21 2024
# Base R

###save this now!

### create dataset

# Set a seed for reproducibility
set.seed(123)

# create a vector of beetle names
beetle_names <- c("Ladybug", "Stag Beetle", "Firefly", "Dung Beetle", "Jewel Beetle")

# Create a vector of beetle lengths
beetle_lengths <- runif(20, 1, 5) # Random lengths between 1 and 5

# Create a vector of beetle colors
beetle_colors <- sample(c("Red", "Black", "Green", "Yellow", "Blue"), 20, replace = TRUE)

# Create the random beetle dataframe
beetle_df <- data.frame(Name = sample(beetle_names, 20, replace = TRUE),
                        length = beetle_lengths,
                        color = beetle_colors)

# save this fake data to data directory
write.csv(beetle_df, file = "data/fakebeetle_data.csv")


### Make chart

length_chart <- barplot(height = beetle_df$length, names = beetle_df$Name)

