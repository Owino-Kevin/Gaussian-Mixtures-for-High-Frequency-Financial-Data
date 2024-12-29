# Load required package
library(mclust)

# Load or simulate dataset
data <- Soy_Beans_Chinese_Futures$Close

# Define the function to compute BIC for given number of components
compute_BIC <- function(data, G) {
  model <- Mclust(data, G = G)
  return(model$bic)
}

# Compute BIC gradient for a range of G values
finite_difference_gradient <- function(data, G_range, epsilon = 1) {
  gradients <- numeric(length(G_range))
  
  for (i in seq_along(G_range)) {
    G <- G_range[i]
    BIC_G <- compute_BIC(data, G)
    BIC_G_plus <- compute_BIC(data, G + epsilon)
    
    # Gradient (approximate derivative of BIC with respect to G)
    gradients[i] <- (BIC_G - BIC_G_plus) / epsilon
  }
  
  return(gradients)
}

# Set range of components and compute gradients
G_range <- 1:10  # Range of components to test
epsilon <- 1
BIC_gradients <- finite_difference_gradient(data, G_range, epsilon)

# Plot the BIC gradient
plot(G_range, BIC_gradients, type = "b", pch = 19, col = "blue",
     xlab = "Number of Components (G)", 
     ylab = "Gradient of BIC", 
     main = "Gradient of BIC vs Number of Components")
abline(h = 0, col = "red", lty = 2)  # Add a horizontal line at gradient = 0
