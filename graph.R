library(ggplot2)

# Hauptdaten
data <- data.frame(
  Kategorie = rep(c(
    "I was able to contribute effectively to the programming tasks",
    "I played a significant role in the research activities",
    "My contributions were important in solving problems \nthat arose during the project",
    "I actively participated in preparing the project presentation",
    "I contributed substantially to writing the project report"
  ), each = 5),
  Bewertung = rep(c("1", "2", "3", "4", "5"), times = 5),
  Wert = c(
    1+3, 0+8, 4+6, 17+23, 17+50,
    1+2, 2+12, 14+12, 13+38, 9+26,
    1+1, 2+3, 7+8, 15+38, 14+40,
    0+5, 0+8, 4+12, 17+28, 18+37,
    1+6, 8+17, 6+22, 8+23, 16+22
  )
)

# Dummy-Daten zur Erklärung der X-Achse
xaxis_expl <- data.frame(
  Rating = factor(c("1", "2", "3", "4", "5")),
  Explanation = c("1 Strongly disagree", "2 Somewhat disagree", "3 Neither agree or disagree", "4 Somewhat agree", "5 Strongly agree")
)

# Plot with reduced text size (size = 7)
ggplot(data, aes(x = Bewertung, y = Wert, fill = Kategorie)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_point(data = xaxis_expl, aes(x = Rating, y = 0, shape = Explanation), inherit.aes = FALSE) +
  scale_shape_manual(
    name = "X-Axis Explanation",
    values = rep(15, 5)
  ) +
  scale_fill_manual(
    name = "Contribution Category",
    values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")
  ) +
  labs(
    title = "Individual Contribution",
    x = "Rating",
    y = "Sum of Responses"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 7),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 7),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    plot.title = element_text(size = 7, face = "bold"),
    strip.text = element_text(size = 7)
  )
