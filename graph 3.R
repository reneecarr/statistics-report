library(dplyr)

# Deine Daten (Antwortverteilungen)
contribution_data <- list(
  Programming     = c(1, 0, 4, 17, 17),
  Research        = c(1, 2, 14, 13, 9),
  ProblemSolving  = c(1, 2, 7, 15, 14),
  Presentation    = c(0, 0, 4, 17, 18),
  ReportWriting   = c(1, 8, 6, 8, 16)
)

# Hilfsfunktion zur Umwandlung in long format mit Gender
generate_responses <- function(question, freq_vec) {
  responses <- rep(1:5, times = freq_vec)
  n <- length(responses)
  gender_vec <- rep(c("Male", "Non-male"), length.out = n)
  data.frame(
    Question = question,
    Response = responses,
    Gender = gender_vec
  )
}

# Alle Fragen zusammenführen
df_list <- lapply(names(contribution_data), function(q) {
  generate_responses(q, contribution_data[[q]])
})

df <- bind_rows(df_list)

# Mann-Whitney-U-Test für jede Frage
test_results <- df %>%
  group_by(Question) %>%
  summarise(
    p_value = wilcox.test(Response ~ Gender, exact = FALSE)$p.value,
    statistic = wilcox.test(Response ~ Gender, exact = FALSE)$statistic
  )

# Ausgabe
for (i in 1:nrow(test_results)) {
  cat(paste0("Question: ", test_results$Question[i], "\n",
             "  p-value: ", round(test_results$p_value[i], 4), "\n",
             "  U-statistic: ", test_results$statistic[i], "\n\n"))
}
