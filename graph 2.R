# Load required libraries
library(gt)
library(webshot2)

# Define Likert scale
likert_scores <- c(1, 2, 3, 4, 5)
reverse_likert <- 6 - likert_scores

# Replace with actual frequency data (Strongly Disagree to Strongly Agree)
male_data <- matrix(c(
  11, 12, 20, 31, 31,
  11, 11, 30, 19, 34,
  32, 28, 24, 17, 3,
  11,12, 33, 30, 18,
  14, 10, 14, 33, 33,
  7, 10, 33, 31, 23
), nrow = 6, byrow = TRUE)

non_male_data <- matrix(c(
  1, 5, 3, 8, 24,
  2, 3,8, 4, 24,
  22, 8, 4, 6, 1,
  2, 4,8, 10, 17,
  2, 5, 5, 7, 22,
  2, 4, 3, 6, 26
), nrow = 6, byrow = TRUE)

# Reverse code flags
reverse_code_flags <- c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)

# Combine both groups
combined_data <- male_data + non_male_data

# Compute mean Likert score per question
compute_mean <- function(freq, reverse = FALSE) {
  scores <- if (reverse) reverse_likert else likert_scores
  sum(freq * scores) / sum(freq)
}

combined_means <- mapply(function(row, rev) compute_mean(combined_data[row, ], rev),
                         1:nrow(combined_data), reverse_code_flags)

# Labels
question_labels <- c(
  "Group dynamics improved",
  "More satisfied with group composition",
  "Learning experience better",
  "Conflict resolution improved",
  "More equal participation",
  "More motivated"
)

# Create table data
table_df <- data.frame(
  Question = question_labels,
  Mean_Score = round(combined_means, 2)
)

# Generate and save pretty table
gt_table <- gt(table_df) %>%
  tab_header(
    title = "Overall Mean Likert Scores (Semester 2 vs Semester 1)"
  ) %>%
  cols_label(
    Question = "Question",
    Mean_Score = "Mean Score"
  ) %>%
  fmt_number(
    columns = c(Mean_Score),
    decimals = 2
  )

gtsave(gt_table, "overall_group_dynamics_table.pdf")