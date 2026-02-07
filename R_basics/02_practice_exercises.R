# ============================================
# R Practice Exercises for Beginners
# Try these exercises to test your skills!
# ============================================

# -----------------------------------------
# EXERCISE 1: Basic Math
# -----------------------------------------
# Calculate the following:
# a) 25 + 17
# b) 100 divided by 8
# c) 3 to the power of 4

# Your code here:



# -----------------------------------------
# EXERCISE 2: Variables
# -----------------------------------------
# Create variables for:
# a) Your first name
# b) Your age
# c) Your height in meters
# Print each variable

# Your code here:



# -----------------------------------------
# EXERCISE 3: Vectors
# -----------------------------------------
# a) Create a vector called 'temperatures' with values: 22, 25, 19, 30, 28
# b) Calculate the mean temperature
# c) Find the maximum temperature
# d) How many elements are in the vector?

# Your code here:



# -----------------------------------------
# EXERCISE 4: Data Frame
# -----------------------------------------
# Create a data frame called 'movies' with:
# - title: "Movie A", "Movie B", "Movie C"
# - year: 2020, 2019, 2021
# - rating: 8.5, 7.2, 9.0
# Then find the movie with the highest rating

# Your code here:



# -----------------------------------------
# EXERCISE 5: Simple Function
# -----------------------------------------
# Create a function called 'celsius_to_fahrenheit'
# that converts Celsius to Fahrenheit
# Formula: F = C * 9/5 + 32
# Test it with 25 degrees Celsius

# Your code here:



# -----------------------------------------
# EXERCISE 6: Loop Practice
# -----------------------------------------
# Write a for loop that prints the squares
# of numbers from 1 to 5
# Output should be: 1, 4, 9, 16, 25

# Your code here:



# ============================================
# SOLUTIONS (try yourself first!)
# ============================================

# Scroll down for solutions...
#
#
#
#
#
#
#
#
#
#
#
#
#
#

# -----------------------------------------
# SOLUTION 1
# -----------------------------------------
25 + 17           # 42
100 / 8           # 12.5
3 ^ 4             # 81

# -----------------------------------------
# SOLUTION 2
# -----------------------------------------
first_name <- "John"
my_age <- 25
height <- 1.75

print(first_name)
print(my_age)
print(height)

# -----------------------------------------
# SOLUTION 3
# -----------------------------------------
temperatures <- c(22, 25, 19, 30, 28)
mean(temperatures)     # 24.8
max(temperatures)      # 30
length(temperatures)   # 5

# -----------------------------------------
# SOLUTION 4
# -----------------------------------------
movies <- data.frame(
  title = c("Movie A", "Movie B", "Movie C"),
  year = c(2020, 2019, 2021),
  rating = c(8.5, 7.2, 9.0)
)
movies[which.max(movies$rating), ]  # Movie C

# -----------------------------------------
# SOLUTION 5
# -----------------------------------------
celsius_to_fahrenheit <- function(celsius) {
  fahrenheit <- celsius * 9/5 + 32
  return(fahrenheit)
}
celsius_to_fahrenheit(25)  # 77

# -----------------------------------------
# SOLUTION 6
# -----------------------------------------
for (i in 1:5) {
  print(i ^ 2)
}
