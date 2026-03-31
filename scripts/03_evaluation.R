library(dplyr)
library(tm)
library(ggplot2)
library(here)

source(here::here("scripts", "02_modeling.R"))

cat("LOADING MODEL...\n\n")
model  <- build_model()
movies <- model$movies
cat("MODEL LOADED SUCCESSFULLY\n\n")

eval_results <- evaluate_model(model)

cat("TESTING RECOMMENDATIONS...\n\n")
get_recommendations("The Dark Knight", model)
get_recommendations("Annabelle", model)
get_recommendations("The Notebook", model)
cat("RECOMMENDATION TEST COMPLETED\n\n")

cat("PLOTTING SIMILARITY DISTRIBUTION...\n\n")
png(here::here("results", "figures", "cosine_similarity_dist.png"), width = 800, height = 500)
plot_similarity(model)
dev.off()
cat("DONE\n")

# Save evaluation results to results/tables/
cat("SAVING EVALUATION RESULTS...\n\n")

# Create tables directory if it doesn't exist
dir.create(here::here("results", "tables"), recursive = TRUE, showWarnings = FALSE)

results_df <- data.frame(
  Metric = c(
    "Total Movies",
    "Vocabulary Size (Terms)",
    "TF-IDF Density (%)",
    "Average Cosine Similarity",
    "Max Cosine Similarity",
    "Coverage (%)",
    "Sparsity (%)",
    "Precision @ 0.05 (%)",
    "Genre Precision@10 (%)",
    "Movies with Genres"
  ),
  Value = c(
    nrow(model$movies),
    ncol(model$tfidf),
    round(eval_results$density,                2),
    round(eval_results$avg_sim,                4),
    round(eval_results$max_sim,                4),
    round(eval_results$coverage    * 100,      2),
    round((1 - eval_results$coverage) * 100,   2),
    round(eval_results$precision_cos * 100,    2),
    round(eval_results$genre_precision,        2),
    sum(sapply(model$movies$genres_list, length) > 0)
  )
)

write.csv(
  results_df,
  here::here("results", "tables", "model_performance.csv"),
  row.names = FALSE
)

cat("Evaluation results saved to results/tables/model_performance.csv\n")
