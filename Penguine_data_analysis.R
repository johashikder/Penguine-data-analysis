# Install & Load Necessary Packages
install.packages(c("tidyverse", "ggplot2", "corrplot", "GGally"))
library(tidyverse)
library(ggplot2)
library(corrplot)
library(GGally)

# Load the Dataset
df <- read.csv(choose.files(), stringsAsFactors = FALSE)
df
# Data Overview & Cleaning -----
str(df)  # Check structure
summary(df)  # Summary statistics

# Check for Missing Values
missing_values <- colSums(is.na(df))
missing_values

# Visualize missing values
install.packages("VIM")
library(VIM)
aggr(df, col=c("blue", "red"), numbers=TRUE, sortVars=TRUE, labels=names(df), 
     cex.axis=.7, gap=3, ylab="Missing Data Pattern")


# Convert Categorical Variables to Factors
df$Species <- as.factor(df$Species)
df$Sex <- as.factor(df$Sex)
df$Island <- as.factor(df$Island)

# Exploratory Data Analysis (EDA) -----
# Distribution of Species
ggplot(df, aes(x = Species, fill = Species)) + 
  geom_bar() + 
  theme_minimal() + 
  labs(title = "Distribution of Penguin Species", x = "Species", y = "Count")

# Boxplot of Body Mass by Species and Sex
ggplot(df, aes(x = Species, y = Body.Mass..g., fill = Sex)) + 
  geom_boxplot() + 
  theme_minimal() + 
  labs(title = "Body Mass Distribution by Species & Sex", x = "Species", y = "Body Mass (g)")

# Correlation Analysis -----
# Subset Numeric Variables
numeric_vars <- df %>% select(Culmen.Length..mm., Culmen.Depth..mm., Flipper.Length..mm., Body.Mass..g.)

# Correlation Matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs")
corrplot(cor_matrix, method = "color", tl.col = "black", tl.srt = 45)

# Scatter Plot of Culmen Length vs. Depth -----
ggplot(df, aes(x = Culmen.Length..mm., y = Culmen.Depth..mm., color = Species)) + 
  geom_point() + 
  theme_minimal() + 
  labs(title = "Culmen Length vs Depth by Species", x = "Culmen Length (mm)", y = "Culmen Depth (mm)")

# Analyzing Nitrogen and Carbon Isotope Ratios -----
ggplot(df, aes(x = Delta.13.C..o.oo., y = Delta.15.N..o.oo., color = Species)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Nitrogen vs Carbon Isotope Ratios", x = "Delta 13 C", y = "Delta 15 N")

# Save Cleaned Data
write.csv(df, "cleaned_penguin_data.csv", row.names = FALSE)
