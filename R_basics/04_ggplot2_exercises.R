# ============================================
# ggplot2 Practice Exercises
# Test your visualization skills!
# ============================================

library(ggplot2)

# -----------------------------------------
# SAMPLE DATA FOR EXERCISES
# -----------------------------------------

# Dataset 1: Student performance
students <- data.frame(
  name = c("Alice", "Bob", "Carol", "David", "Eva",
           "Frank", "Grace", "Henry", "Iris", "Jack"),
  math_score = c(85, 92, 78, 95, 88, 72, 90, 84, 91, 76),
  science_score = c(90, 85, 82, 88, 95, 70, 88, 79, 93, 81),
  study_hours = c(10, 15, 8, 18, 12, 6, 14, 9, 16, 7),
  gender = c("F", "M", "F", "M", "F", "M", "F", "M", "F", "M")
)

# Dataset 2: Monthly temperatures
weather <- data.frame(
  month = factor(month.abb, levels = month.abb),
  temperature = c(2, 4, 10, 15, 20, 25, 28, 27, 22, 15, 8, 3),
  rainfall = c(50, 40, 45, 55, 70, 30, 25, 35, 50, 65, 60, 55)
)

# -----------------------------------------
# EXERCISE 1: Basic Scatter Plot
# -----------------------------------------
# Create a scatter plot showing the relationship between
# study_hours (x-axis) and math_score (y-axis)
# Add appropriate title and axis labels

# Your code here:



# -----------------------------------------
# EXERCISE 2: Colored Scatter Plot
# -----------------------------------------
# Using the students dataset, create a scatter plot of
# math_score vs science_score, colored by gender
# Add a title and use theme_minimal()

# Your code here:



# -----------------------------------------
# EXERCISE 3: Bar Plot
# -----------------------------------------
# Create a bar plot showing each student's math_score
# Fill the bars with a color of your choice
# Add title and labels

# Your code here:



# -----------------------------------------
# EXERCISE 4: Line Plot
# -----------------------------------------
# Using the weather dataset, create a line plot
# showing temperature across months
# Add points on the line and use theme_classic()

# Your code here:



# -----------------------------------------
# EXERCISE 5: Histogram
# -----------------------------------------
# Create a histogram of math_score from the students dataset
# Use 5 bins and fill with "steelblue"
# Add a white border around bars

# Your code here:



# -----------------------------------------
# EXERCISE 6: Box Plot
# -----------------------------------------
# Create a box plot comparing math_score by gender
# Color the boxes by gender
# Add jittered points on top

# Your code here:



# -----------------------------------------
# EXERCISE 7: Faceted Plot
# -----------------------------------------
# Create a scatter plot of study_hours vs math_score
# Facet (split) by gender
# Add a trend line using geom_smooth()

# Your code here:



# -----------------------------------------
# EXERCISE 8: Combined Visualization
# -----------------------------------------
# Using the weather dataset, create a plot showing both
# temperature (as a line) and rainfall (as bars) across months
# Hint: You may need to use two geom layers

# Your code here:



# ============================================
# SOLUTIONS
# ============================================
# Scroll down...
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
ggplot(students, aes(x = study_hours, y = math_score)) +
  geom_point(size = 3, color = "blue") +
  labs(
    title = "Study Hours vs Math Score",
    x = "Hours Studied",
    y = "Math Score"
  )

# -----------------------------------------
# SOLUTION 2
# -----------------------------------------
ggplot(students, aes(x = math_score, y = science_score, color = gender)) +
  geom_point(size = 3) +
  labs(
    title = "Math vs Science Scores by Gender",
    x = "Math Score",
    y = "Science Score",
    color = "Gender"
  ) +
  theme_minimal()

# -----------------------------------------
# SOLUTION 3
# -----------------------------------------
ggplot(students, aes(x = name, y = math_score)) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(
    title = "Student Math Scores",
    x = "Student",
    y = "Math Score"
  ) +
  theme_minimal()

# -----------------------------------------
# SOLUTION 4
# -----------------------------------------
ggplot(weather, aes(x = month, y = temperature, group = 1)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "darkred", size = 3) +
  labs(
    title = "Monthly Temperature",
    x = "Month",
    y = "Temperature (C)"
  ) +
  theme_classic()

# -----------------------------------------
# SOLUTION 5
# -----------------------------------------
ggplot(students, aes(x = math_score)) +
  geom_histogram(bins = 5, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Math Scores",
    x = "Math Score",
    y = "Count"
  ) +
  theme_minimal()

# -----------------------------------------
# SOLUTION 6
# -----------------------------------------
ggplot(students, aes(x = gender, y = math_score, fill = gender)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, size = 2) +
  labs(
    title = "Math Scores by Gender",
    x = "Gender",
    y = "Math Score"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("F" = "pink", "M" = "lightblue"))

# -----------------------------------------
# SOLUTION 7
# -----------------------------------------
ggplot(students, aes(x = study_hours, y = math_score)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ gender) +
  labs(
    title = "Study Hours vs Math Score by Gender",
    x = "Hours Studied",
    y = "Math Score"
  ) +
  theme_bw()

# -----------------------------------------
# SOLUTION 8
# -----------------------------------------
# Option 1: Using secondary axis (advanced)
ggplot(weather, aes(x = month, group = 1)) +
  geom_bar(aes(y = rainfall), stat = "identity",
           fill = "lightblue", alpha = 0.7) +
  geom_line(aes(y = temperature * 2), color = "red", size = 1) +
  geom_point(aes(y = temperature * 2), color = "darkred", size = 2) +
  scale_y_continuous(
    name = "Rainfall (mm)",
    sec.axis = sec_axis(~ . / 2, name = "Temperature (C)")
  ) +
  labs(title = "Monthly Weather: Temperature and Rainfall") +
  theme_minimal() +
  theme(axis.title.y.right = element_text(color = "red"))

# Option 2: Simpler approach - two separate plots
# Temperature plot
p1 <- ggplot(weather, aes(x = month, y = temperature, group = 1)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Temperature", y = "Temp (C)") +
  theme_minimal()

# Rainfall plot
p2 <- ggplot(weather, aes(x = month, y = rainfall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Rainfall", y = "mm") +
  theme_minimal()

# Display both (requires gridExtra package)
# library(gridExtra)
# grid.arrange(p1, p2, nrow = 2)

# ============================================
# End of ggplot2 Exercises
# ============================================
