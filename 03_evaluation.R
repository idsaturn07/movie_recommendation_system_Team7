library(dplyr)
library(tm)
library(ggplot2)

source("utils/hybrid_model.R")

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
plot_similarity(model)
cat("DONE\n")
