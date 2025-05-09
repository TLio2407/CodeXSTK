
# Read data from CSV file into the dataframe 'add'
add <- read.csv("add.csv", na.strings = c("?", ""))

# Remove the first column (ID column)
add <- add[, -1]

# Show first 5 rows
head(add, 5)

# Remove duplicates
num_dups <- sum(duplicated(add))
cat("Num_duplicated_rows:", num_dups, "\n")
add <- add[!duplicated(add), ]

# Rename columns: 'height', 'width', 'ratio', 'target'
colnames(add)[1] <- "height"
colnames(add)[2] <- "width"
colnames(add)[3] <- "ratio"
colnames(add)[1559] <- "target"

# Replace target variable values: "ad." → 1, "nonad" → 0
add$target <- ifelse(add$target == "ad.", 1, 0)

# Current data
head(add, 5)

# Convert all columns to numeric
add[] <- lapply(add, as.numeric)

# Count number and percentage of NA values per column
na_counts <- colSums(is.na(add))
na_percentage <- colMeans(is.na(add)) * 100

# Print NA summary table
na_summary <- data.frame(
  NA_Count = na_counts,
  NA_Percent = round(na_percentage, 2)
)
print(na_summary)

# Replace NA in 'height' and 'width' with the column median
add$height[is.na(add$height)] <- median(add$height, na.rm = TRUE)
add$width[is.na(add$width)] <- median(add$width, na.rm = TRUE)

# Function to recalculate 'ratio' if 'height' and 'width' are available
update_ratio <- function(df) {
  df$ratio[is.na(df$ratio) & !is.na(df$height) & !is.na(df$width)] <- 
    df$height[is.na(df$ratio) & !is.na(df$height) & !is.na(df$width)] / 
    df$width[is.na(df$ratio) & !is.na(df$height) & !is.na(df$width)]
  return(df)
}

# Apply the update_ratio function to fill missing 'ratio' values
add <- update_ratio(add)

# Calculate NA counts and percentages for selected columns
selected_cols <- c("height", "width", "ratio")
na_counts <- colSums(is.na(add[selected_cols]))
na_percentage <- colMeans(is.na(add[selected_cols])) * 100
na_summary <- data.frame(
  NA_Count = na_counts,
  NA_Percent = round(na_percentage, 2)
)
print(na_summary)

# Remove rows with outliers in 'height', 'width', or 'ratio' using z-score
z_scores <- scale(add[, c("height", "width", "ratio")])
mask_z <- apply(abs(z_scores), 1, function(row) all(row < 3, na.rm = TRUE))
add <- add[mask_z, ]


# Data after processing
head(add, 5)

# Save processed data to CSV
cat("After outlier removal:", nrow(add), "rows remain\n")
write.csv(add, file = "processed_add.csv", row.names = FALSE)
