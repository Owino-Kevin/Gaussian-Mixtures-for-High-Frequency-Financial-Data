x=Soy_Beans_Chinese_Futures$Close
x
plot(density(x))

# Estimates of starting values
mem <- kmeans(x,2)$cluster
mu1 <- mean(x[mem==1])
mu2 <- mean(x[mem==2])
sigma1 <- sd(x[mem==1])
sigma2 <- sd(x[mem==2])
pi1 <- sum(mem==1)/length(mem)
pi2 <- sum(mem==2)/length(mem)

# modified sum only considers finite values
sum.finite <- function(x) {
  sum(x[is.finite(x)])
}
Q <- 0
# starting value of expected value of the log likelihood
Q[2] <- sum.finite(log(pi1)+log(dnorm(x, mu1, sigma1))) + sum.finite(log(pi2)+log(dnorm(x, mu2, sigma2)))
Q[2]
k <- 2
while (abs(Q[k]-Q[k-1])>=1e-6) {
  # E step
  comp1 <- pi1 * dnorm(x, mu1, sigma1)
  comp2 <- pi2 * dnorm(x, mu2, sigma2)
  comp.sum <- comp1 + comp2
  
  p1 <- comp1/comp.sum
  p2 <- comp2/comp.sum
  # M step
  pi1 <- sum.finite(p1) / length(x)
  pi2 <- sum.finite(p2) / length(x)
  
  mu1 <- sum.finite(p1 * x) / sum.finite(p1)
  mu2 <- sum.finite(p2 * x) / sum.finite(p2)  
  sigma1 <- sqrt(sum.finite(p1 * (x-mu1)^2) / sum.finite(p1))
  sigma2 <- sqrt(sum.finite(p2 * (x-mu2)^2) / sum.finite(p2))
  
  p1 <- pi1 
  p2 <- pi2
  
  k <- k + 1
  Q[k] <- sum(log(comp.sum))
} 

#Final parameter estimates
cat("Final Parameters:\n")
cat("Mean 1:", mu1, "\n")
cat("Mean 2:", mu2, "\n")
cat("Sigma 1:", sigma1, "\n")
cat("Sigma 2:", sigma2, "\n")
cat("Pi 1:", pi1, "\n")
cat("Pi 2:", pi2, "\n")


hist(x, prob=T, breaks=32, xlim=c(range(x)[1], range(x)[2]), main='2-Component Gaussian Mixture')
lines(density(x), col="green", lwd=2)
x1 <- seq(from=range(x)[1], to=range(x)[2], length.out=1000)
y <- pi1 * dnorm(x1, mean=mu1, sd=sigma1) + pi2 * dnorm(x1, mean=mu2, sd=sigma2)
lines(x1, y, col="red", lwd=2)
legend('bottomleft', col=c("green", 'red'), lwd=2, legend=c("kernal", "fitted"))
