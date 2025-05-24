satisfaction_scores <- c(3.74, 3.68, 3.78, 3.47, 3.71, 3.71)

neutral_point <- 3.0

t_test_result <- t.test(satisfaction_scores, mu = neutral_point, alternative = "greater")

print(t_test_result)

mean_score <- mean(satisfaction_scores)
sd_score <- sd(satisfaction_scores)
cohens_d <- (mean_score - neutral_point) / sd_score
print(paste("Cohen's d effect size:", round(cohens_d, 2)))

overall_mean <- mean(satisfaction_scores)
confidence_interval <- t_test_result$conf.int

cat("\nSummary Statistics for Quantifying the Effect:\n")
cat("Overall mean satisfaction score:", round(overall_mean, 2), "\n")
cat("95% Confidence Interval:", round(confidence_interval[1], 2), "to Inf\n")
cat("Standard Deviation:", round(sd_score, 2), "\n")
cat("Mean difference from neutral point:", round(overall_mean - neutral_point, 2), "\n")
cat("Percentage above neutral:", round(mean(satisfaction_scores > neutral_point) * 100), "%\n")
