# -------------------------------
# Data Cleaning in R
# Handling Customer Data: Typos, Missing Values, Duplicates, and Outliers
# -------------------------------

# Load necessary libraries
library(dplyr)   # For data manipulation
library(tidyr)   # For handling missing values
library(ggplot2) # For data visualization

# Simulated Dataset: Customer Information
data <- data.frame(
  Customer_ID = c(101, 102, 103, 104, 105, 102, 106, 107, 108, 109),
  Name = c("John Doe", "Jane Smith", "Sam Brown", "Sue Johson", "Mike White",
           "Jane Smith", "Emily Davis", "Michael Johnson", "Chris Lee", "Chris Lee"),
  Email = c("john@example.com", "jane@example.com", NA, "sue_j@example.com", "mike.w@example.com",
            "jane@example.com", "emily.d@example.com", "michael.j@example.com", "chris.l@example.com", "chris.l@example.com"),
  Purchase_Amount = c(200, 300, 150, NA, 4000, 300, 250, 180, 190, 200),
  stringsAsFactors = FALSE
)

# Introduce missing values for demonstration
data$Purchase_Amount[c(5, 4)] <- NA  # Set Purchase_Amount for some customers as missing

# View the original dataset
print("Original Data:")
print(data)

# -------------------------------
# 1. Fixing Typos (using a correction table)
# -------------------------------

# Create a lookup table for correcting name typos
correction_table <- data.frame(
  Incorrect = c("Sue Johson"), 
  Correct = c("Sue Johnson"),
  stringsAsFactors = FALSE
)

# Correct the typo in the Name column
data_cleaned <- data %>%
  mutate(Name = ifelse(Name %in% correction_table$Incorrect, 
                       correction_table$Correct[match(Name, correction_table$Incorrect)], 
                       Name))

# -------------------------------
# 2. Handling Missing Values
# -------------------------------

# Check for missing data
missing_data_summary <- colSums(is.na(data_cleaned))
print("Missing Data Summary:")
print(missing_data_summary)

# Option 1: Impute missing Purchase_Amount with the median
median_purchase <- median(data_cleaned$Purchase_Amount, na.rm = TRUE)

data_cleaned <- data_cleaned %>%
  mutate(Purchase_Amount = ifelse(is.na(Purchase_Amount), 
                                  median_purchase, 
                                  Purchase_Amount))

# Option 2: Remove rows with missing Email (assuming Email is essential)
data_cleaned <- data_cleaned %>%
  filter(!is.na(Email))

# -------------------------------
# 3. Removing Duplicates
# -------------------------------

# Identify and count the number of duplicate entries based on Customer_ID
num_duplicates <- data %>%
  group_by(Customer_ID) %>%
  filter(n() > 1) %>%
  summarise(duplicate_count = n())

# Print the number of duplicates found
print(paste("Number of duplicate entries based on Customer_ID:", nrow(num_duplicates)))

# Remove duplicates, keeping the first occurrence
data_cleaned <- data_cleaned %>%
  distinct(Customer_ID, .keep_all = TRUE)

# -------------------------------
# 4. Detecting and Removing Outliers
# -------------------------------

# Visualize Purchase_Amount to detect outliers
ggplot(data_cleaned, aes(x = "", y = Purchase_Amount)) +
  geom_boxplot() +
  ggtitle("Boxplot of Purchase Amounts") +
  ylab("Purchase Amount") +
  xlab("")

# Identify outliers using the interquartile range (IQR) method
Q1 <- quantile(data_cleaned$Purchase_Amount, 0.25)
Q3 <- quantile(data_cleaned$Purchase_Amount, 0.75)
IQR <- Q3 - Q1

# Define outlier thresholds
lower_threshold <- Q1 - 1.5 * IQR
upper_threshold <- Q3 + 1.5 * IQR

# Filter out the outliers
data_cleaned <- data_cleaned %>%
  filter(Purchase_Amount >= lower_threshold & Purchase_Amount <= upper_threshold)

# -------------------------------
# Final Data Check
# -------------------------------

# View the cleaned dataset
print("Cleaned Data:")
print(data_cleaned)

# -------------------------------
# Impact on Business Metrics
# -------------------------------

# Total Purchase Amount before cleaning
total_purchase_before_cleaning <- sum(data$Purchase_Amount, na.rm = TRUE)
print(paste("Total Purchase Amount (Before Cleaning):", total_purchase_before_cleaning))

# Total Purchase Amount after cleaning
total_purchase_after_cleaning <- sum(data_cleaned$Purchase_Amount)
print(paste("Total Purchase Amount (After Cleaning):", total_purchase_after_cleaning))
