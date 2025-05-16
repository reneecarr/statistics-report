# 1. Load libraries and data
library(readxl)
library(lattice)
library(reshape2)

# Make sure to escape spaces or use forward slashes
xls_path <- "C:/scripts R/data/Data statistics assignment.xlsx"
df_raw   <- read_excel(xls_path, sheet = 1, col_names = TRUE)

output_dir <- "C:/scripts R/data/plots"
if (!dir.exists(output_dir)) dir.create(output_dir)

save_plot <- function(filename, plot_code, width = 1000, height = 800) {
  png(filename = file.path(output_dir, filename), width = width, height = height)
  plot_code()
  dev.off()
}

# 2. Clean and label
df <- df_raw[!is.na(df_raw[[1]]), ]
colnames(df)[1] <- "Question"

# Assign numeric question labels
question_ids <- paste0("Q", seq_len(nrow(df)))
question_lookup <- setNames(df$Question, question_ids)

# Print the lookup table (Q# = full text)
cat("QUESTION LOOKUP TABLE:\n\n")
for (i in seq_along(question_lookup)) {
  cat(sprintf("%s = %s\n", names(question_lookup)[i], question_lookup[i]))
}

# Prepare matrix for plotting
response_matrix <- as.matrix(df[, -1])
rownames(response_matrix) <- names(question_lookup)

# Colours for each response category (1 colour per column of original matrix)
response_colors <- c(
  "Strongly Disagree" = "#D73027",
  "Disagree"          = "#FC8D59",
  "Neutral"           = "#FFFFBF",
  "Agree"             = "#91BFDB",
  "Strongly Agree"    = "#4575B4"
)  # e.g. 5 Likert levels
category_labels <- colnames(response_matrix)        # "Strongly Disagree", ...

# 3.1 Stacked bar‑plot  (legend = response categories)
save_plot("stacked_barplot.png", function() {
 barplot(
   t(response_matrix),            # ⬅️ transpose!
   beside      = FALSE,           # stacked
   las         = 2,
   col         = response_colors,
   legend.text = category_labels, # legend shows categories, not Q‑numbers
   args.legend = list(x = "topright", cex = 0.7),
   main        = "Stacked Barplot of Survey Responses",
   xlab        = "Question",
   ylab        = "Count",
   names.arg   = rownames(response_matrix)   # Q1, Q2, …
 )
})
# 3.2 Grouped bar‑plot  (legend = response categories)
barplot(
  t(response_matrix),            # ⬅️ transpose!
  beside      = TRUE,            # grouped
  las         = 2,
  col         = response_colors,
  legend.text = category_labels,
  args.legend = list(x = "topright", cex = 0.7),
  main        = "Grouped Barplot of Survey Responses",
  xlab        = "Question",
  ylab        = "Count",
  names.arg   = rownames(response_matrix)   # Q1, Q2, …
)

# 3.3 Pie chart for Q1
pie(
  response_matrix[1, ],
  labels = paste0(colnames(response_matrix), " (", response_matrix[1, ], ")"),
  main   = paste0(names(question_lookup)[1], ": ", substr(question_lookup[1], 1, 50), "...")
)

# 3.4 Dotchart
dotchart(
  x = as.vector(response_matrix),
  labels = paste(
    rep(rownames(response_matrix), times = ncol(response_matrix)),
    rep(colnames(response_matrix), each  = nrow(response_matrix)),
    sep = " — "
  ),
  main = "Dotchart of Survey Counts",
  xlab = "Count"
)

# 3.5 Heatmap
heatmap(
  response_matrix,
  Rowv    = NA,
  Colv    = NA,
  scale   = "column",
  col     = heat.colors(256),
  margins = c(5, 10),
  main    = "Heatmap of Response Distribution"
)

# 3.6 Lattice barchart
long <- melt(response_matrix)
names(long) <- c("Question", "Response", "Count")

barchart(
  Count ~ Response | Question,
  data     = long,
  layout   = c(2, ceiling(nrow(response_matrix)/2)),
  scales   = list(x = list(rot = 45)),
  strip    = strip.custom(par.strip.text = list(cex = 0.8)),
  as.table = TRUE,
  main     = "Lattice Barchart by Question",
  ylab     = "Count"
)


