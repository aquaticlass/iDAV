# ============================================
# Introduction to Data Visualization with ggplot2
# Basic Graphs for Beginners
# ============================================

# -----------------------------------------
# INSTALLING AND LOADING GGPLOT2
# -----------------------------------------

# Install ggplot2 (only need to do this once)
# install.packages("ggplot2")

# Load the library
library(ggplot2)

# -----------------------------------------
# UNDERSTANDING GGPLOT2 BASICS
# -----------------------------------------

# ggplot2 uses a "grammar of graphics" approach:
# 1. DATA: Your dataset
# 2. AESTHETICS (aes): Map variables to visual properties (x, y, color, size)
# 3. GEOMETRIES (geom): The type of plot (points, lines, bars, etc.)

# Basic structure:
# ggplot(data, aes(x = ..., y = ...)) + geom_xxx()

# -----------------------------------------
# SAMPLE DATA
# -----------------------------------------

# We'll use the built-in 'mtcars' dataset
# It contains data about 32 cars from 1974

# View the first few rows
head(mtcars)

# Understanding the data
# mpg  = miles per gallon
# cyl  = number of cylinders
# hp   = horsepower
# wt   = weight (1000 lbs)
# gear = number of gears

# Create a simple dataset for examples
students <- data.frame(
  name = c("Alice", "Bob", "Carol", "David", "Eva"),
  score = c(85, 92, 78, 95, 88),
  hours_studied = c(10, 15, 8, 18, 12),
  subject = c("Math", "Science", "Math", "Science", "Math")
)

# -----------------------------------------
# 1. SCATTER PLOTS
# -----------------------------------------

# Basic scatter plot
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

# Add color based on a variable
ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point()

# Change point size
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(size = 3, color = "blue")

# Size based on a variable
ggplot(mtcars, aes(x = wt, y = mpg, size = hp)) +
  geom_point(color = "darkgreen", alpha = 0.6)

# Add titles and labels
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "steelblue", size = 3) +
  labs(
    title = "Car Weight vs Fuel Efficiency",
    subtitle = "Data from 1974 Motor Trend magazine",
    x = "Weight (1000 lbs)",
    y = "Miles per Gallon"
  )

# -----------------------------------------
# 2. LINE PLOTS
# -----------------------------------------

# Sample time series data
months <- data.frame(
  month = 1:12,
  sales = c(120, 150, 170, 160, 180, 220, 250, 240, 200, 190, 210, 280)
)

# Basic line plot
ggplot(months, aes(x = month, y = sales)) +
  geom_line()

# Line with points
ggplot(months, aes(x = month, y = sales)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3)

# Styled line plot
ggplot(months, aes(x = month, y = sales)) +
  geom_line(color = "#2E86AB", size = 1.2) +
  geom_point(color = "#2E86AB", size = 3) +
  labs(
    title = "Monthly Sales",
    x = "Month",
    y = "Sales ($)"
  ) +
  scale_x_continuous(breaks = 1:12)

# -----------------------------------------
# 3. BAR PLOTS
# -----------------------------------------

# Basic bar plot (for counts)
ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar()

# Bar plot with specific values
ggplot(students, aes(x = name, y = score)) +
  geom_bar(stat = "identity")

# Colored bars
ggplot(students, aes(x = name, y = score, fill = name)) +
  geom_bar(stat = "identity") +
  labs(title = "Student Scores", x = "Student", y = "Score")

# Remove legend when not needed
ggplot(students, aes(x = name, y = score, fill = name)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Student Scores", x = "Student", y = "Score")

# Horizontal bar plot
ggplot(students, aes(x = score, y = name, fill = name)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Student Scores", x = "Score", y = "Student")

# Grouped bar plot
ggplot(mtcars, aes(x = factor(cyl), fill = factor(gear))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Cars by Cylinders and Gears",
    x = "Number of Cylinders",
    fill = "Gears"
  )

# -----------------------------------------
# 4. HISTOGRAMS
# -----------------------------------------

# Basic histogram
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram()

# Control number of bins
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "white")

# Control bin width
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "coral", color = "white") +
  labs(title = "Distribution of MPG", x = "Miles per Gallon", y = "Count")

# Histogram with density curve
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(aes(y = ..density..), bins = 10,
                 fill = "lightblue", color = "white") +
  geom_density(color = "red", size = 1)

# -----------------------------------------
# 5. BOX PLOTS
# -----------------------------------------

# Basic box plot
ggplot(mtcars, aes(y = mpg)) +
  geom_boxplot()

# Box plot by group
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_boxplot()

# Colored box plots
ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    title = "MPG by Number of Cylinders",
    x = "Cylinders",
    y = "Miles per Gallon"
  )

# Box plot with data points
ggplot(mtcars, aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "MPG Distribution by Cylinders", x = "Cylinders", y = "MPG")

# -----------------------------------------
# 6. THEMES
# -----------------------------------------

# ggplot2 has built-in themes to change the overall look

# Default theme
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "steelblue", size = 3) +
  labs(title = "Weight vs MPG")

# Minimal theme
p + theme_minimal()

# Black and white theme
p + theme_bw()

# Classic theme
p + theme_classic()

# Dark theme
p + theme_dark()

# Light theme
p + theme_light()

# -----------------------------------------
# 7. COLORS
# -----------------------------------------

# Manual colors for discrete variables
ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar() +
  scale_fill_manual(values = c("red", "green", "blue"))

# Using color palettes
ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2")

# Gradient colors for continuous variables
ggplot(mtcars, aes(x = wt, y = mpg, color = hp)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "yellow", high = "red")

# -----------------------------------------
# 8. FACETS (SMALL MULTIPLES)
# -----------------------------------------

# Split into multiple panels by a variable
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  facet_wrap(~ cyl)

# Grid of panels with two variables
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  facet_grid(gear ~ cyl)

# Styled faceted plot
ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 2) +
  facet_wrap(~ cyl, labeller = labeller(cyl = c("4" = "4 Cylinders",
                                                 "6" = "6 Cylinders",
                                                 "8" = "8 Cylinders"))) +
  theme_minimal() +
  labs(title = "Weight vs MPG by Cylinder Count") +
  theme(legend.position = "none")

# -----------------------------------------
# 9. SAVING PLOTS
# -----------------------------------------

# Create a plot
my_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "steelblue", size = 3) +
  theme_minimal() +
  labs(title = "Car Weight vs Fuel Efficiency")

# Save to file
# ggsave("my_plot.png", my_plot, width = 8, height = 6)
# ggsave("my_plot.pdf", my_plot, width = 8, height = 6)

# -----------------------------------------
# 10. COMBINING ELEMENTS
# -----------------------------------------

# A complete, polished plot
ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl), size = hp)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, size = 0.8) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Relationship Between Car Weight and Fuel Efficiency",
    subtitle = "Colored by cylinder count, sized by horsepower",
    x = "Weight (1000 lbs)",
    y = "Miles per Gallon",
    color = "Cylinders",
    size = "Horsepower"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

# ============================================
# QUICK REFERENCE CHEAT SHEET
# ============================================

# GEOMS (Plot Types):
# geom_point()     - Scatter plot
# geom_line()      - Line plot
# geom_bar()       - Bar plot
# geom_histogram() - Histogram
# geom_boxplot()   - Box plot
# geom_density()   - Density plot
# geom_smooth()    - Trend line
# geom_text()      - Add text labels
# geom_area()      - Area plot

# AESTHETICS (aes):
# x, y       - Position
# color      - Outline/point color
# fill       - Fill color
# size       - Size of points/lines
# shape      - Point shape
# alpha      - Transparency (0-1)
# linetype   - Line style

# THEMES:
# theme_minimal()  - Clean, minimal
# theme_bw()       - Black and white
# theme_classic()  - Classic look
# theme_dark()     - Dark background

# ============================================
# End of ggplot2 Basics Tutorial
# ============================================
