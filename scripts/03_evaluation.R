library(dplyr)
library(tm)
library(ggplot2)
library(here)

source(here::here("scripts", "02_modeling.R"))

cat("LOADING MODEL...\n\n")
model  <- build_model()
movies <- model$movies
cat("MODEL LOADED SUCCESSFULLY\n\n")

evaluate_model(model)

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