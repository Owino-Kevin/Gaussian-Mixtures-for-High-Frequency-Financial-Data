library(mclust)

# Load a dataset similar to `fetch_sdss_sspp` from Python's astroML
# In this example, we will use the iris dataset as an example
data <- Soy_Beans_Chinese_Futures$Close  # Using first two columns for simplicity (you can replace this with actual data)

# Plot the raw data
plot(data, col = "gray", main = "Scatter Plot of Data")

# Fit Gaussian Mixture Models for different component numbers
# We'll fit models with 1 to 13 components
models <- Mclust(data, G = 1:13)
summary(models)
models$BIC
models$loglik
models$bic

# Identify the best model based on BIC
best_model <- models$modelName[which.min(models$BIC)]
best_model
best_components <- models$G[which.min(models$BIC)]
best_components
print(paste("Best model by BIC:", best_model, "with", best_components, "components"))

# Plot BIC values for each number of components
plot(1:13, models$BIC, type = "b", xlab = "Number of Components", ylab = "BIC", main = "BIC for Gaussian Mixture Models")

# Plot the data with ellipses representing each Gaussian component
plot(data, col = "gray", main = paste("Best GMM fit with", best_components, "components"))
for(i in 1:best_components) {
  mean <- models$parameters$mean[, i]
  covariance <- models$parameters$variance$sigma[,,i]
  ellipse(mean, covariance, level = 0.95, col = "black", add = TRUE)
}
