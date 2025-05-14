# Call libraries
library(ggplot2)
library(reshape2)  # For preparing data for correlation plot
library(caret)     # For training data
library(lattice)   # For boxplot

# Create directories for results
if (!dir.exists("Results")) {
  dir.create("Results")
  dir.create("Results/Histogram")
  dir.create("Results/Boxplot")
  dir.create("Results/Correlation")
  dir.create("Results/Missing")
  dir.create("Results/Residual")
  dir.create("Results/Scatter")
}

# Read CSV file
gpu_data <- read.csv("All_GPUs.csv")
head(gpu_data)

# Collect data
main_data <- gpu_data[c("Memory_Bandwidth", "Memory_Speed", "L2_Cache",
                        "Memory_Bus", "Shader", "Dedicated", "Manufacturer")]
head(main_data)

# Clean data
main_data[main_data == ""] <- NA
main_data[] <- lapply(main_data, function(x) gsub("^\\n-␣$", NA, x))
main_data[main_data == "NA"] <- NA

# Missing data statistics
na_summary <- data.frame(
  Column = names(main_data),
  NA_Count = sapply(main_data, function(x) sum(is.na(x))),
  NA_Percentage = sapply(main_data, function(x) mean(is.na(x)) * 100)
)

main_data <- na.omit(main_data)

# Plot statistical data
ggplot(na_summary, aes(x = Column, y = NA_Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
  geom_text(
    aes(label = paste0(round(NA_Percentage, 1), "%")),
    hjust = -0.2,
    size = 3
  ) +
  labs(
    title = "Proportion of missing data in variables",
    x = "Variables",
    y = "Proportion of missing data (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10, angle = 90, hjust = 1)
  ) +
  coord_flip()
ggsave("Results/Missing/missing_data_plot.png",
       width = 10, height = 7,
       units = "in", dpi = 300, bg = "white")

columns_to_clean <- c("Memory_Bandwidth", "Memory_Bus", "Memory_Speed")

remove_units <- function(column) {
  cleaned_column <- gsub("[^0-9.]", "", column)
  as.numeric(cleaned_column)
}

main_data[columns_to_clean] <- lapply(main_data[columns_to_clean], remove_units)

reformat_cache <- function(cache_values) {
  extract_values <- function(x) {
    x <- gsub("\\s*KB", "", x)
    multiplier_match <- regexpr("\\(x[2-4]\\)", x)
    if (multiplier_match != -1) {
      multiplier <- as.numeric(substr(x, multiplier_match + 2,
                                      multiplier_match + 2))
      base_value <- as.numeric(substr(x, 1, multiplier_match - 1))
      base_value * multiplier
    } else {
      as.numeric(x)
    }
  }
  sapply(cache_values, extract_values)
}

main_data["L2_Cache"] <- lapply(main_data["L2_Cache"], reformat_cache)

# Transform variables
main_data$Memory_Bandwidth <- log(main_data$Memory_Bandwidth + 1)
main_data$Memory_Speed <- log(main_data$Memory_Speed + 1)
main_data$L2_Cache <- log(main_data$L2_Cache + 1)
main_data$Memory_Bus <- log(main_data$Memory_Bus + 1)

main_data$Memory_Bandwidth <- exp(main_data$Memory_Bandwidth) - 1
main_data$Memory_Speed <- exp(main_data$Memory_Speed) - 1
main_data$L2_Cache <- exp(main_data$L2_Cache) - 1
main_data$Memory_Bus <- exp(main_data$Memory_Bus) - 1

# Histograms
ggplot(main_data, aes(x = Memory_Bandwidth)) +
  geom_histogram(fill = "lightblue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Memory Bandwidth",
    x = "Memory Bandwidth (GB/s)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Results/Histogram/memory_bandwidth_histogram.png",
       width = 10, height = 7,
       units = "in", dpi = 300, bg = "white")

ggplot(main_data, aes(x = Memory_Speed)) +
  geom_histogram(fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Memory Speed",
    x = "Memory Speed (MHz)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Results/Histogram/memory_speed_histogram.png",
       width = 10, height = 7,
       units = "in", dpi = 300, bg = "white")

ggplot(main_data, aes(x = L2_Cache)) +
  geom_histogram(fill = "lightpink", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of L2 Cache Size",
    x = "L2 Cache (KB)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Results/Histogram/l2_cache_histogram.png",
       width = 10, height = 7,
       units = "in", dpi = 300, bg = "white")

ggplot(main_data, aes(x = Memory_Bus)) +
  geom_histogram(fill = "lightyellow", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Memory Bus (after using logarithm)",
    x = "Memory Bus (Bit)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Results/Histogram/memory_bus_histogram.png",
       width = 10, height = 7,
       units = "in", dpi = 300, bg = "white")

# Scatter plots
ggplot(main_data, aes(x = Memory_Speed, y = Memory_Bandwidth)) +
  geom_point(color = "green", alpha = 0.7) +
  labs(
    title = "Scatter Plot of Memory Speed and Memory Bandwidth",
    x = "Memory Speed (MHz)",
    y = "Memory Bandwidth (GB/s)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Results/Scatter/memory_speed_scatter.png",
       width = 10, height = 7,
       units = "in", dpi = 300, bg = "white")

ggplot(main_data, aes(x = L2_Cache, y = Memory_Bandwidth)) +
  geom_point(color = "red", alpha = 0.7) +
  labs(
    title = "Scatter Plot of L2 Cache Size and Memory Bandwidth",
    x = "L2 Cache (KB)",
    y = "Memory Bandwidth (GB/s)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Results/Scatter/l2_cache_scatter.png",
       width = 10, height = 7,
       units = "in", dpi = 300, bg = "white")

ggplot(main_data, aes(x = Memory_Bus, y = Memory_Bandwidth)) +
  geom_point(color = "blue", alpha = 0.7) +
  labs(
    title = "Scatter Plot of Memory Bus and Memory Bandwidth",
    x = "Memory Bus (Bit)",
    y = "Memory Bandwidth (GB/s)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("Results/Scatter/memory_bus_scatter.png",
       width = 10, height = 7,
       units = "in", dpi = 300, bg = "white")

# Correlation matrix
data <- main_data[c("Memory_Bandwidth", "Memory_Speed",
                    "L2_Cache", "Memory_Bus")]
cor_matrix <- cor(data)
cor_data <- melt(cor_matrix)

ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limit = c(-1, 1),
    name = "Correlation"
  ) +
  labs(title = "Correlation Diagram", x = "", y = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )
ggsave("Results/Correlation/correlation_diagram.png",
       width = 10, height = 8,
       units = "in", dpi = 300, bg = "white")

# Boxplots
png("Results/Boxplot/memory_speed_boxplot.png",
    width = 8, height = 6,
    units = "in", res = 300, bg = "white")
boxplot(main_data$Memory_Speed,
        main = "Boxplot of Memory Speed",
        ylab = "Memory Speed (MHz)",
        col = "lightblue")
dev.off()

png("Results/Boxplot/l2_cache_boxplot.png",
    width = 8, height = 6,
    units = "in", res = 300, bg = "white")
boxplot(main_data$L2_Cache,
        main = "Boxplot of L2_Cache Size",
        ylab = "L2_Cache (KB)",
        col = "lightblue")
dev.off()

png("Results/Boxplot/memory_bus_boxplot.png",
    width = 8, height = 6,
    units = "in", res = 300, bg = "white")
boxplot(main_data$Memory_Bus,
        main = "Boxplot of Memory_Bus",
        ylab = "Memory_Bus (Bit)",
        col = "lightblue")
dev.off()

png("Results/Boxplot/memory_bandwidth_boxplot.png",
    width = 8, height = 6,
    units = "in", res = 300, bg = "white")
boxplot(main_data$Memory_Bandwidth,
        main = "Boxplot of Memory_Bandwidth",
        ylab = "Memory_Bandwidth (GB/s)",
        col = "lightblue")
dev.off()

# Linear regression model
set.seed(31112024)
train_index <- createDataPartition(
  main_data$Memory_Bandwidth,
  p = 0.8,
  list = FALSE
)
train_data <- main_data[train_index, ]
test_data <- main_data[-train_index, ]

# Model building
model <- lm(Memory_Bandwidth ~ Memory_Speed + L2_Cache + Memory_Bus,
            data = train_data)
summary(model)

# Residual plots
png("Results/Residual/residual_plots.png",
    width = 10, height = 12,
    units = "in", res = 300, bg = "white")
par(mfrow = c(2, 2))
plot(model)
dev.off()