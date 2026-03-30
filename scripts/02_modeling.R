library(dplyr)
library(tm)
library(ggplot2)

parse_genre_names <- function(genre_str) {
  if (is.na(genre_str) || trimws(genre_str) == "" || genre_str == "[]")
    return(character(0))
  
  # Match both 'name': 'Value'  AND  "name": "Value"
  matches <- regmatches(
    genre_str,
    gregexpr("['\"]name['\"]\\s*:\\s*['\"]([^'\"]+)['\"]", genre_str)
  )[[1]]
  
  if (length(matches) == 0) return(character(0))
  
  # Extract just the value part after the colon
  vals <- gsub(".*['\"]name['\"]\\s*:\\s*['\"]([^'\"]+)['\"].*", "\\1", matches)
  tolower(trimws(vals))
}

clean_genres <- function(genre_str) {
  g <- parse_genre_names(genre_str)
  if (length(g) == 0) return("")
  paste(g, collapse = " ")
}

get_genre_list <- function(genre_str) {
  parse_genre_names(genre_str)
}

# BUILD MODEL
build_model <- function() {
  
  cat("STEP 1: LOADING DATA\n")
  movies <- read.csv(
    here::here("data", "movies_final.csv"),
    stringsAsFactors = FALSE
  )
  cat("Total movies:", nrow(movies), "\n\n")
  
  # Keep top 3000 by popularity
  movies <- movies %>%
    arrange(desc(popularity)) %>%
    slice(1:3000)
  
  # Genre cleaning 
  cat("STEP 2: CLEANING GENRES\n")
  movies$genres_clean <- sapply(movies$genres, clean_genres)
  movies$genres_list  <- lapply(movies$genres, get_genre_list)
  
  # Quick sanity check
  n_with_genres <- sum(sapply(movies$genres_list, length) > 0)
  cat("Movies with parsed genres:", n_with_genres, "/", nrow(movies), "\n")
  if (n_with_genres < 100)
    warning("Very few genres parsed — check your CSV genre column format!")
  
  # Content feature
  cat("STEP 3: CREATING CONTENT FEATURE\n")
  
  overview <- ifelse(is.na(movies$overview) | movies$overview == "",
                     "", movies$overview)
  
  # Use keywords column if it exists (same format as genres)
  if ("keywords" %in% colnames(movies)) {
    keywords_text <- sapply(movies$keywords, clean_genres)
  } else {
    keywords_text <- rep("", nrow(movies))
  }
  
  # Use tagline if it exists
  if ("tagline" %in% colnames(movies)) {
    tagline_text <- ifelse(is.na(movies$tagline), "", movies$tagline)
  } else {
    tagline_text <- rep("", nrow(movies))
  }
  
  # Weights: genres x2 (important but not dominant), overview x1, keywords x1
  movies$content <- trimws(paste(
    movies$genres_clean, movies$genres_clean,   # genres weighted x2
    keywords_text,                               # keyword tags
    tagline_text,                                # tagline
    overview                                     # full description
  ))
  
  movies <- movies %>% filter(nchar(content) > 10)
  cat("Movies after content filter:", nrow(movies), "\n\n")
  
  # TF-IDF 
  cat("STEP 4: BUILDING TF-IDF MATRIX\n")
  corpus <- Corpus(VectorSource(movies$content))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(removePunctuation))
  corpus <- tm_map(corpus, content_transformer(removeNumbers))
  corpus <- tm_map(corpus, content_transformer(function(x)
    removeWords(x, c(stopwords("english"),
                     # Remove generic movie words that add noise
                     "film", "movie", "story", "life", "world", "man",
                     "woman", "one", "two", "find", "must", "new", "will"))))
  corpus <- tm_map(corpus, content_transformer(stripWhitespace))
  
  dtm <- DocumentTermMatrix(
    corpus,
    control = list(
      weighting   = weightTfIdf,
      wordLengths = c(3, Inf),
      bounds      = list(global = c(2, Inf))  # FIX: was c(5, Inf)
    )
  )
  
  tfidf <- as.matrix(dtm)
  cat("TF-IDF matrix:", nrow(tfidf), "x", ncol(tfidf), "\n\n")
  
  # Normalisation 
  cat("STEP 5: NORMALISING\n")
  norms <- sqrt(rowSums(tfidf^2))
  norms[norms == 0] <- 1
  tfidf_norm <- tfidf / norms
  
  # Cosine similarity 
  cat("STEP 6: COMPUTING COSINE SIMILARITY\n")
  similarity_matrix <- tfidf_norm %*% t(tfidf_norm)
  similarity_matrix[is.na(similarity_matrix)] <- 0
  similarity_matrix <- pmin(similarity_matrix, 1)
  cat("Similarity matrix ready\n\n")
  
  #  Final score 
  if (!"final_score" %in% colnames(movies)) {
    pop_scaled    <- as.numeric(scale(log1p(movies$popularity)))
    vote_scaled   <- as.numeric(scale(movies$vote_average))
    count_scaled  <- as.numeric(scale(log1p(movies$vote_count)))
    
    movies$final_score <- pop_scaled + vote_scaled + count_scaled
    movies$final_score[is.na(movies$final_score)] <- 0
  }
  
  cat("MODEL BUILD COMPLETE\n\n")
  
  return(list(
    movies     = movies,
    tfidf      = tfidf,
    tfidf_norm = tfidf_norm,
    similarity = similarity_matrix
  ))
}

# Alias kept for compatibility with app.R
build_hybrid_model <- function(movies_final = NULL) build_model()

# RECOMMENDATIONS
get_recommendations <- function(movie_name, model, n = 10) {
  
  movies     <- model$movies
  sim_matrix <- model$similarity
  
  idx <- which(tolower(movies$title) == tolower(movie_name))
  if (length(idx) == 0) {
    cat("Movie not found:", movie_name, "\n")
    return(NULL)
  }
  idx <- idx[1]
  
  query_genres   <- movies$genres_list[[idx]]
  query_language <- movies$original_language[idx]

  if (length(query_genres) == 0) {
    cat("Warning: no genres found for", movie_name, "— using full pool\n")
    filtered_idx <- seq_len(nrow(movies))
  } else {
    # Primary: same language + genre overlap
    filtered_idx <- which(
      sapply(movies$genres_list, function(g) length(intersect(query_genres, g)) > 0) &
        movies$original_language == query_language
    )
    
    # Fallback 1: relax language constraint
    if (length(filtered_idx) < 15) {
      filtered_idx <- which(
        sapply(movies$genres_list, function(g) length(intersect(query_genres, g)) > 0)
      )
    }
    
    # Fallback 2: use everything (shouldn't normally reach here)
    if (length(filtered_idx) < 15) {
      filtered_idx <- seq_len(nrow(movies))
    }
  }
  
  #  Cosine similarity scores 
  scores <- as.numeric(sim_matrix[idx, filtered_idx])
  
  #  Genre similarity (Jaccard) 
  genre_score <- sapply(filtered_idx, function(i) {
    g       <- movies$genres_list[[i]]
    overlap <- length(intersect(query_genres, g))
    uni     <- length(union(query_genres, g))
    if (uni == 0) return(0)
    overlap / uni
  })
  
  # Normalised final_score 
  fs <- as.numeric(scale(movies$final_score[filtered_idx]))
  fs[is.na(fs)] <- 0
  # Clamp to [-2, 2] so extreme outliers (Minions) don't dominate
  fs <- pmax(pmin(fs, 2), -2)
  
  #  Combined score 
  # Cosine similarity carries the description/keyword signal 
  # Genre Jaccard ensures genre match even when descriptions differ
  # final_score gives a mild quality boost — kept small on purpose
  final_scores <- (0.55 * scores) +
    (0.40 * genre_score) +
    (0.05 * fs)        
 
  self_pos <- which(filtered_idx == idx)
  if (length(self_pos) > 0) final_scores[self_pos] <- -Inf
  
  # Top-N 
  top_pos <- order(final_scores, decreasing = TRUE)[seq_len(min(n, length(final_scores)))]
  result  <- movies[filtered_idx[top_pos], ]
  
  cat("\nRecommendations for:", toupper(movie_name), "\n\n")
  print(result[, c("title", "genres_clean", "original_language")])
  
  return(as.character(result$title))
}

# EVALUATE MODEL
evaluate_model <- function(model) {
  
  sim   <- model$similarity
  tfidf <- model$tfidf
  
  avg_sim   <- mean(sim, na.rm = TRUE)
  max_sim   <- max(sim,  na.rm = TRUE)
  coverage  <- sum(sim > 0, na.rm = TRUE) / length(sim)
  density   <- sum(tfidf > 0) / length(tfidf) * 100
  precision <- sum(sim > 0.3, na.rm = TRUE) /
    max(sum(sim > 0, na.rm = TRUE), 1)
  
  cat("MODEL EVALUATION\n\n")
  cat("Total movies          :", nrow(model$movies), "\n")
  cat("Vocabulary size       :", ncol(tfidf), "terms\n")
  cat("TF-IDF density        :", round(density,    2), "%\n")
  cat("Avg cosine similarity :", round(avg_sim,    4), "\n")
  cat("Max cosine similarity :", round(max_sim,    4), "\n")
  cat("Coverage              :", round(coverage * 100, 2), "%\n")
  cat("Sparsity              :", round((1-coverage)*100, 2), "%\n")
  cat("Precision @ 0.3       :", round(precision * 100, 2), "%\n\n")
  
  # Genre parse sanity check
  n_genres <- sum(sapply(model$movies$genres_list, length) > 0)
  cat("Movies with genres    :", n_genres, "/", nrow(model$movies), "\n\n")
  
  return(invisible(list(
    avg_sim   = avg_sim,
    max_sim   = max_sim,
    coverage  = coverage,
    density   = density,
    precision = precision
  )))
}

# SIMILARITY PLOT
plot_similarity <- function(model) {
  sim_values <- as.vector(model$similarity)
  sim_values <- sim_values[sim_values > 0 & sim_values < 1]
  
  p <- ggplot(data.frame(sim_values), aes(x = sim_values)) +
    geom_histogram(bins = 50, fill = "steelblue", color = "white", alpha = 0.85) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 14)) +
    labs(title    = "Cosine Similarity Distribution",
         subtitle = "Non-zero pairwise similarity scores",
         x = "Similarity Score",
         y = "Count")
  
  print(p)
}