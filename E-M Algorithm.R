# Load necessary library
library(GeneralizedHyperbolic)
library(EMCluster)
library(ghyp)
x <- Soy_Beans_Chinese_Futures$Close
hist(x, breaks = 30, prob = TRUE, main = "Histogram for Chinese Futures", xlab = "Closing Prices")
lines(density(x), col = "red", lwd = 1)
matrixdat<-matrix(x, ncol = 1)
x<-matrixdat
x
in_para <- init.EM(matrixdat, nclass = 2, lab = NULL, EMC = .EMC, stable.solution = TRUE, min.n = NULL,min.n.iter = 10, method = c("em.EM","Rnd.EM"))
in_para
em_algo <- emcluster(matrixdat, in_para)
print(em_algo)
print(em_algo$mu)

curve(dnorm(matrixdat, mean = em_algo$Mu[1], sd = sqrt(em_algo$Sigma[1, 1])), col = 'red', add = TRUE)
curve(dnorm(matrixdat, mean = em_result$Mu[2], sd = sqrt(em_result$Sigma[2, 2])), col = 'blue', add = TRUE)










# Generate data using the ghyp library (NIG as a special case of GH)
nig_data <- rghyp(n, ghyp(lambda = 1, alpha = alpha, beta = beta, delta = delta, mu = mu))

# Plot histogram of the data
hist(nig_data, breaks = 30, prob = TRUE, main = "Histogram of NIG Data", xlab = "x", col = "lightblue")
lines(density(nig_data), col = "red", lwd = 2)

# Log-likelihood function for NIG
nig_loglikelihood <- function(params, x) {
  alpha <- params[1]
  beta <- params[2]
  delta <- params[3]
  mu <- params[4]
  
  # Negative log-likelihood based on NIG density
  -sum(dnig(x, alpha = alpha, beta = beta, delta = delta, mu = mu, log = TRUE))
}

# E-step: Expectation of latent variable w given parameters and data
E_step <- function(x, alpha, beta, delta, mu) {
  # Compute w: latent variable (weight factor in NIG distribution)
  w <- (x - mu)^2 / delta^2 + 1 / alpha^2
  sqrt(w)
}

# M-step: Maximization step to update parameters using weighted averages
M_step <- function(x, w) {
  # Update the parameters based on the E-step result
  alpha_new <- 1 / sqrt(mean(w))
  beta_new <- mean((x - mu_est) / w) / delta_est
  delta_new <- sqrt(mean((x - mu_est)^2 / w))
  mu_new <- mean(x)  # Simple estimate for location
  
  return(c(alpha_new, beta_new, delta_new, mu_new))
}

# EM algorithm for NIG parameter estimation
EM_NIG <- function(x, max_iter = 100, tol = 1e-6) {
  # Initial parameter estimates (random initialization)
  alpha_est <- 1
  beta_est <- 0
  delta_est <- 1
  mu_est <- mean(x)
  
  loglik_old <- nig_loglikelihood(c(alpha_est, beta_est, delta_est, mu_est), x)
  
  for (iter in 1:max_iter) {
    # E-step: Compute latent variable expectation
    w <- E_step(x, alpha_est, beta_est, delta_est, mu_est)
    
    # M-step: Update parameters based on the expectation
    params_new <- M_step(x, w)
    alpha_est <- params_new[1]
    beta_est <- params_new[2]
    delta_est <- params_new[3]
    mu_est <- params_new[4]
    
    # Compute the new log-likelihood
    loglik_new <- nig_loglikelihood(c(alpha_est, beta_est, delta_est, mu_est), x)
    
    # Check for convergence
    if (abs(loglik_new - loglik_old) < tol) {
      cat("Converged after", iter, "iterations\n")
      break
    }
    
    loglik_old <- loglik_new
  }
  
  return(list(alpha_est = alpha_est, beta_est = beta_est, delta_est = delta_est, mu_est = mu_est))
}

# Run the EM algorithm to estimate NIG parameters
nig_params_est <- EM_NIG(nig_data)

# Print estimated parameters
cat("Estimated NIG Parameters:\n")
print(nig_params_est)

# Compare with the true parameters
cat("True NIG Parameters:\n")
cat("alpha =", true_alpha, "beta =", true_beta, "delta =", true_delta, "mu =", true_mu, "\n")
