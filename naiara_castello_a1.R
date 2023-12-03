library(tidyverse)
data(msleep)
?msleep

# 1. Convert into factors
msleep$genus <- factor(msleep$genus)
msleep$vore <- factor(msleep$vore)
msleep$order <- factor(msleep$order)
msleep$conservation <- factor(msleep$conservation)

# 2. Shortest sleep time
shortest_sleep <- 1.9
shortest_sleep_mammal <- "Giraffe"

# 3. Most missing
## Calculate total missing per column
missing_data_counts <- colSums(is.na(msleep))

## Find the column with the most missing
column_with_most_missing <- names(missing_data_counts)[which.max(missing_data_counts)]

## Print the result
cat("Column with the most missing values:", column_with_most_missing, "\n")
cat("Number of missing values:", max(missing_data_counts), "\n")

## Create variables
most_missing <- "sleep_cycle"
missing_values <- 51

# 4. Correlations
correlations <- cor(msleep[, c("sleep_total", "sleep_rem", "sleep_cycle", "awake", "brainwt", "bodywt")], use = "pairwise.complete.obs")

# 5. Highest correlation
highest_corr <- 0.9337822

# 6. Sleep time distribution
sleep_histogram <- ggplot(
  data = msleep,
  mapping = aes(x = sleep_total)) +
  geom_histogram(binwidth = 1)

# 7. Bar chart for food categories
food_barchart <- ggplot(
  data = msleep,
  mapping = aes(x = vore)) +
  geom_bar()

# 8. Grouped box plot for sleep time
sleep_boxplot <- ggplot(msleep, aes(x = vore, y = sleep_total)) +
  geom_boxplot()

# 9. Longest average sleep time
## Calculate group means
group_means <- msleep |>
  group_by(vore) |>
  summarize(mean_sleep = mean(sleep_total, na.rm = TRUE))

## Create variable
highest_average <- 14.940000

# 10. REM sleep vs. total sleep, colored by order
sleep_scatterplot <- ggplot(msleep, aes(x = sleep_total, y = sleep_rem)) +
  geom_point(aes(color = order))

# 11. REM sleep vs. total sleep for the order most common in the data
## Determining the most common order
order_frequency <- table(msleep$order)
print(order_frequency)

## Filter "Rodentia" cases
subset_data <- msleep |>
  filter(order == "Rodentia")

## Plot total vs. rem sleep time
sleep_scatterplot2 <- ggplot(subset_data, aes(x = sleep_total, y = sleep_rem)) +
  geom_point()