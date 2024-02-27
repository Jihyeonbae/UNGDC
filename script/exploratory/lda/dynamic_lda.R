# Execute "reading_lda.R" file before running this.
# Dynamic Topic Modeling
library(dplyr)
library(tidyr)

model1<-lda_models[[1]]
model2<-lda_models[[2]]

# phi value is a topic probability of every word
phi1 <- model1$phi

#phi1$topic <- sequence(nrow(phi1))

phi2 <- model2$phi
#phi2$topic <- sequence(nrow(phi2))


# Convert matrices to data frames
phi1_df <- as.data.frame(phi1)
phi2_df <- as.data.frame(phi2)

order_phi1 <- order(colMeans(phi1_df), decreasing = TRUE)
order_phi2 <- order(colMeans(phi2_df), decreasing = TRUE)

# Reorder columns based on the mean
phi1_df <- phi1_df[, order_phi1]
phi2_df <- phi2_df[, order_phi2]

# Identify columns to drop based on colMeans
## Try without dropping
columns_to_drop_phi1 <- colMeans(phi1_df) < 0.00001
columns_to_drop_phi2 <- colMeans(phi2_df) < 0.00001

# Drop identified columns
phi1_df <- phi1_df[, !columns_to_drop_phi1, drop = FALSE]
phi2_df <- phi2_df[, !columns_to_drop_phi2, drop = FALSE]


# Get the union of column names
all_terms <- union(colnames(phi1_df), colnames(phi2_df))

#fill missing values with zeros
phi1_union <- bind_cols(phi1_df, setNames(data.frame(matrix(0, nrow = nrow(phi1_df), ncol = length(setdiff(all_terms, colnames(phi1_df))))), setdiff(all_terms, colnames(phi1_df))))
phi2_union <- bind_cols(phi2_df, setNames(data.frame(matrix(0, nrow = nrow(phi2_df), ncol = length(setdiff(all_terms, colnames(phi2_df))))), setdiff(all_terms, colnames(phi2_df))))

# Reorder columns alphabetically
phi1_union <- phi1_union[, order(colnames(phi1_union))]
phi2_union <- phi2_union[, order(colnames(phi2_union))]


dim(phi1_union)
dim(phi2_union)


cor<-cor(t(phi1_union), t(phi2_union))

heatmap(cor[, 10:1],
        Rowv = NA, Colv = NA,
        main = "Topic Correlation Matrix",
        xlab = "Topic", ylab = "Topic",
        col = heat.colors(256))

library(gplots)

heatmap.2(cor,
          Rowv = FALSE, Colv = FALSE,
          col = heat.colors(256),
          trace = "none", # no row/column names
          key = TRUE, keysize = 1.5,
          density.info = "none", margins = c(5, 5),
          cexCol = 1, cexRow = 1, # adjust text size
          notecol = "black", notecex = 0.7,
          symkey = FALSE)

order_phi1_union <- order(colMeans(phi1_union), decreasing = TRUE)
phi1_result <- phi1_union[, order_phi1_union]

order_phi2_union <- order(colMeans(phi2_union), decreasing = TRUE)
phi2_result <- phi2_union[, order_phi2_union]

#Function to print out the words

orderBasedOnRow <- function(df, I) {
  # Order columns based on the Ith row values
  ordered_cols <- order(apply(df, 2, function(x) x[I]), decreasing = TRUE)

  # Reorder the data frame columns
  ordered_df <- df[, ordered_cols]

  ordered_row <- ordered_df[I, 1:10]

  return(ordered_row)
}

phi1_result_row <- orderBasedOnRow(phi1_union, 1)
print(phi1_result_row)

