library(readxl)

# Load data
file_path <- "Book3 (1).xlsx"
non_male_df <- read_excel(file_path, sheet = "Non-male")
male_df <- read_excel(file_path, sheet = "Male")

# Define the exact questions related to individual contribution
individual_questions <- c(
  "I was able to contribute effectively to the programming tasks",
  "I played  a significant role in the research activities",
  "My contributions were important in solving problems that arose during the project",
  "I actively participated in preparing the project presentation",
  "I contributed substantially to writing the project report"
)

# Extract and sum counts across those rows for non-male
rows_nm <- non_male_df[non_male_df$`Comparison of group dynamics` %in% individual_questions, 2:6]
nm_counts <- colSums(sapply(rows_nm, as.numeric), na.rm = TRUE)

# Extract and sum counts across those rows for male
rows_m <- male_df[male_df$`Comparison of group dynamics` %in% individual_questions, 2:6]
m_counts <- colSums(sapply(rows_m, as.numeric), na.rm = TRUE)

# Create contingency table
contingency_table <- rbind(nm_counts, m_counts)
rownames(contingency_table) <- c("Non-male", "Male")
colnames(contingency_table) <- c("Strongly disagree", "Somewhat disagree", "Neutral", "Somewhat agree", "Strongly agree")

# Print contingency table
print("Contingency Table:")
print(contingency_table)

# Perform chi-square test if valid
if (sum(contingency_table) > 0 && all(contingency_table >= 0)) {
  chi_test <- chisq.test(contingency_table)
  print(chi_test)
} else {
  print("Chi-square test could not be run: table contains only zeros or invalid values.")
}
