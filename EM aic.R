# Load necessary library
if (!requireNamespace("mclust", quietly = TRUE)) install.packages("mclust")
library(mclust)

# Load or simulate a dataset
data <- iris[, 1:2]  # Use your dataset here, e.g., iris dataset's first two columns

# Fit GMMs with different numbers of components (e.g., 1 to 10)
n_components <- 1:10
aic_values <- numeric(length(n_components))  # Store AIC values

for (k in n_components) {
  # Fit GMM with k components
  model <- Mclust(data, G = k)
  
  # Calculate AIC and store it
  aic_values[k] <- model$aic
}

# Plot AIC vs. Number of Components
plot(n_components, aic_values, type = "b", pch = 19,
     xlab = "Number of Components",
     ylab = "AIC",
     main = "AIC vs. Number of Components in GMM")

# Find the optimal number of components based on minimum AIC
optimal_k <- n_components[which.min(aic_values)]
cat("Optimal number of components based on AIC:", optimal_k, "\n")
