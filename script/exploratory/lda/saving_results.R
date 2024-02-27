# Execute "reading_lda.R" file before running this.

topic_tables <- topic_tables(lda_models, span_levels) #topic_tables function is locally saved in "reading_lda.R" file.
###############################################################################

#Linking how topics are represented over time.

# Extract theta matrices from each LDA model
theta_matrices <- lapply(lda_models, function(model) model$theta)

# Combine the list of matrices into a 3D array
theta_array <- array(unlist(theta_matrices),
                     dim = c(nrow(theta_matrices[[1]]),
                             ncol(theta_matrices[[1]]),
                             length(theta_matrices)))

# Reshape the array into a matrix
theta_matrix <- matrix(theta_array, nrow = nrow(theta_array),
                       ncol = ncol(theta_array) * length(theta_matrices))

# Calculate correlations between topic vectors
cor_matrix <- cor(theta_matrix, method = "spearman")



### topic evolution

# Visualize the correlation matrix
heatmap(cor_matrix,
        Rowv = NA, Colv = NA,
        main = "Topic Correlation Matrix",
        xlab = "Topic", ylab = "Topic",
        col = heat.colors(256))

# Identify highly correlated topics
highly_correlated_pairs <- which(cor_matrix > 0.8, arr.ind = TRUE)
print(paste("Highly Correlated Topic Pairs: ", toString(highly_correlated_pairs)))

# Extracting terms from the phi matrix (word matrix)
phi_matrix_terms <- colnames(lda_models[[1]]$phi)

# Initialize lists to store word vectors and corresponding terms
word_vectors_list <- list()
word_vector_terms_list <- list()

# Extract theta matrices and terms
for (i in seq_along(lda_models)) {
  theta_matrix <- lda_models[[i]]$theta
  terms <- colnames(theta_matrix)

  # Extracting word vectors from the phi matrix for terms in theta
  phi_matrix_terms_intersection <- intersect(terms, phi_matrix_terms)
  word_vector_terms_list[[i]] <- phi_matrix_terms_intersection

  # Extracting word vectors from the phi matrix
  word_vectors_list[[i]] <- lda_models[[i]]$phi[, phi_matrix_terms_intersection]
}

# Combine the list of matrices into a 3D array
word_vectors_array <- array(unlist(word_vectors_list), dim = c(nrow(word_vectors_list[[1]]), ncol(word_vectors_list[[1]]), length(word_vectors_list)))

# Reshape the array into a matrix
word_vectors_matrix <- matrix(word_vectors_array, nrow = nrow(word_vectors_array), ncol = ncol(word_vectors_array) * length(word_vectors_list))

# Calculate correlations between word vectors
word_vectors_cor_matrix <- cor(word_vectors_matrix, method = "spearman")


###########################################
#Similarity matrix
###########################################

# Load required libraries
library(tm)
library(textTinyR)

# Create a Document-Term Matrix (DTM) for each time interval
dtm_list <- lapply(light, function(docs) {
  corpus <- Corpus(VectorSource(docs))
  dtm <- DocumentTermMatrix(corpus)
  as.matrix(dtm)
})

# Calculate semantic similarity between each pair of time intervals
semantic_similarity <- matrix(NA, nrow = length(dtm_list), ncol = length(dtm_list))
for (i in 1:length(dtm_list)) {
  for (j in 1:length(dtm_list)) {
    if (i != j) {
      similarity <- textTinyR::COS_TEXT(dtm_list[[i]], dtm_list[[j]])
      semantic_similarity[i, j] <- similarity
    }
  }
}

# Visualize semantic similarity matrix
heatmap(semantic_similarity,
        Rowv = NA, Colv = NA,
        main = "Semantic Similarity Between Time Intervals",
        xlab = "Time Interval", ylab = "Time Interval",
        col = heat.colors(256))

# Identify pairs of time intervals with high semantic similarity
similar_pairs <- which(semantic_similarity > threshold, arr.ind = TRUE)
print(paste("Pairs with High Semantic Similarity: ", toString(similar_pairs)))
