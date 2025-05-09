#The file contains 1559 columns of data (not counting the ID column - 1st colum in add.csv)
#__________________3279 rows of data
#Columns [1][2][3] is "height" "width" "ratio" and last column [1559] is label("ad"/"nonad")
#_______________________________________________________________________________
#Load required library
library(ggplot2)
library(dplyr)
library(tidyr)

#_______________________________________________________________________________
#Load the Dataset to "data"
data <- read.csv(file.choose())
#Remove the first column
data <- data[, -1]
# Remove columns from 4 to 1558 (binaries data) -> just take height, width, ratio and label->data
data <- data[, -c(4:1558)]

#_______________________________________________________________________________
#Name the columns
colnames(data)[1:4] <- c("height", "width", "ratio","label")
# Convert 'height', 'width', 'ratio' columns to numeric
# Missing value "?" = "NA"
data$height <- as.numeric(data$height)
data$width <- as.numeric(data$width)
data$ratio <- as.numeric(data$ratio)

#Convert label to factor (ad/nonad)
data$label <- as.factor(data$label)

#_______________________________________________________________________________
#Report Missing Value Percentages
cat("Missing Value Percentages (%):\n")
missing_pct <- sapply(data, function(x) mean(is.na(x)) * 100)
print(round(missing_pct, 2))

#_______________________________________________________________________________
#Summary statistic (Mean, Median, Standard Deviation)
cat("\n Summary Statistics:\n")
summary_stats <- data %>%
  summarise(
    height_mean = mean(height, na.rm = TRUE),
    height_median = median(height, na.rm = TRUE),
    height_sd = sd(height, na.rm = TRUE),
    
    width_mean = mean(width, na.rm = TRUE),
    width_median = median(width, na.rm = TRUE),
    width_sd = sd(width, na.rm = TRUE),
    
    ratio_mean = mean(ratio, na.rm = TRUE),
    ratio_median = median(ratio, na.rm = TRUE),
    ratio_sd = sd(ratio, na.rm = TRUE)
  )
print(round(summary_stats, 4))

#_______________________________________________________________________________
#Label Distribution
cat("\n Label Distribution:\n")
print(table(data$label))

#_______________________________________________________________________________
#Histograms for Continuous features by label
features <- c("height", "width", "ratio")

for (feature in features) {
  print(
    ggplot(data, aes_string(x = feature, fill = "label")) +
      geom_histogram(position = "identity", bins = 30, alpha = 0.5, color = "black", na.rm = TRUE) +
      ggtitle(paste("Histogram of", feature, "by Label")) +
      xlab(feature) +
      ylab("Count") +
      scale_fill_manual(values = c("ad." = "#FF7F50", "nonad." = "#4682B4")) +
      theme_minimal()
  )
}

#_______________________________________________________________________________
#Boxplots Grouped by Label
for (feature in features) {
  print(
    ggplot(data, aes_string(x = "label", y = feature)) +
      geom_boxplot(fill = c("#FFA07A", "#90EE90"), na.rm = TRUE) +
      ggtitle(paste("Boxplot of", feature, "by Label")) +
      xlab("Label") +
      ylab(feature) +
      theme_minimal()
  )
}

#_______________________________________________________________________________
# Step 10: Scatter Plot - Height vs Width by Label
print(
  ggplot(data, aes(x = height, y = width, color = label)) +
    geom_point(alpha = 0.6, na.rm = TRUE) +
    ggtitle("Scatter Plot: Height vs Width by Label") +
    xlab("Height") +
    ylab("Width") +
    scale_color_manual(values = c("ad." = "#FF7F50", "nonad." = "#4682B4")) +
    theme_minimal()
)