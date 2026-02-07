# ============================================
# R Basics for Beginners
# Getting Started with R Programming
# ============================================

# -----------------------------------------
# 1. BASIC ARITHMETIC
# -----------------------------------------

# R can work like a calculator
2 + 3         # Addition
10 - 4        # Subtraction
5 * 6         # Multiplication
20 / 4        # Division
2 ^ 3         # Exponentiation (2 to the power of 3)
17 %% 5       # Modulus (remainder after division)
17 %/% 5      # Integer division

# -----------------------------------------
# 2. VARIABLES
# -----------------------------------------

# Use <- to assign values to variables
x <- 10
y <- 5
z <- x + y
print(z)

# You can also use = but <- is preferred in R
name <- "Alice"
age <- 25

# Print variables
print(name)
print(age)

# -----------------------------------------
# 3. DATA TYPES
# -----------------------------------------

# Numeric
my_number <- 42.5
class(my_number)

# Integer
my_integer <- 10L
class(my_integer)

# Character (text/string)
my_text <- "Hello, R!"
class(my_text)

# Logical (TRUE/FALSE)
my_logical <- TRUE
class(my_logical)

# -----------------------------------------
# 4. VECTORS
# -----------------------------------------

# Vectors are the most basic data structure in R
# Create a vector using c() function

numbers <- c(1, 2, 3, 4, 5)
print(numbers)

fruits <- c("apple", "banana", "cherry")
print(fruits)

# Access elements (R uses 1-based indexing)
print(numbers[1])      # First element
print(numbers[3])      # Third element
print(fruits[2])       # Second fruit

# Vector operations
numbers * 2            # Multiply each element by 2
sum(numbers)           # Sum of all elements
mean(numbers)          # Average
length(numbers)        # Number of elements

# Create sequences
seq_1 <- 1:10                    # Numbers 1 to 10
seq_2 <- seq(0, 100, by = 10)    # 0, 10, 20, ... 100
seq_3 <- rep(5, times = 3)       # Repeat 5 three times

print(seq_1)
print(seq_2)
print(seq_3)

# -----------------------------------------
# 5. BASIC FUNCTIONS
# -----------------------------------------

# Built-in functions
sqrt(16)              # Square root
abs(-5)               # Absolute value
round(3.7)            # Round to nearest integer
round(3.14159, 2)     # Round to 2 decimal places
max(numbers)          # Maximum value
min(numbers)          # Minimum value

# Create your own function
add_two_numbers <- function(a, b) {
  result <- a + b
  return(result)
}

# Use the function
add_two_numbers(10, 20)

# -----------------------------------------
# 6. DATA FRAMES
# -----------------------------------------

# Data frames are like tables (rows and columns)
students <- data.frame(
  name = c("Alice", "Bob", "Carol"),
  age = c(20, 22, 21),
  grade = c(85, 90, 88)
)

print(students)

# Access columns
students$name          # Using $ notation
students[, "age"]      # Using bracket notation

# Access rows
students[1, ]          # First row
students[2, ]          # Second row

# Get specific value
students[1, "grade"]   # Alice's grade

# Summary of data
summary(students)

# -----------------------------------------
# 7. BASIC PLOTTING
# -----------------------------------------

# Simple scatter plot
x_values <- c(1, 2, 3, 4, 5)
y_values <- c(2, 4, 5, 4, 5)

plot(x_values, y_values,
     main = "My First Plot",
     xlab = "X Axis",
     ylab = "Y Axis",
     col = "blue",
     pch = 19)

# Bar plot
counts <- c(10, 20, 15)
names(counts) <- c("A", "B", "C")
barplot(counts,
        main = "Simple Bar Plot",
        col = "steelblue")

# Histogram
random_data <- rnorm(100)    # 100 random normal values
hist(random_data,
     main = "Histogram",
     col = "lightgreen",
     breaks = 10)

# -----------------------------------------
# 8. CONDITIONALS
# -----------------------------------------

# if-else statements
score <- 75

if (score >= 90) {
  print("Grade: A")
} else if (score >= 80) {
  print("Grade: B")
} else if (score >= 70) {
  print("Grade: C")
} else {
  print("Grade: F")
}

# -----------------------------------------
# 9. LOOPS
# -----------------------------------------

# for loop
for (i in 1:5) {
  print(paste("Iteration:", i))
}

# Loop through a vector
fruits <- c("apple", "banana", "cherry")
for (fruit in fruits) {
  print(paste("I like", fruit))
}

# while loop
count <- 1
while (count <= 3) {
  print(paste("Count is:", count))
  count <- count + 1
}

# -----------------------------------------
# 10. GETTING HELP
# -----------------------------------------

# Get help on a function
?mean
?plot

# Search for help
help.search("regression")

# Examples of a function
example(mean)

# ============================================
# End of R Basics Tutorial
# Practice these examples and experiment!
# ============================================
