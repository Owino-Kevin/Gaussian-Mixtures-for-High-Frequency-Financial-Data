x=Soy_Beans_Chinese_Futures$Close
x
plot(density(x))

library

# Estimates of starting values
mem <- kmeans(x,5)$cluster
mu1 <- mean(x[mem==1])
mu2 <- mean(x[mem==2])
mu3 <- mean(x[mem==3])
mu4 <- mean(x[mem==4])
mu5 <- mean(x[mem==5])
sigma1 <- sd(x[mem==1])
sigma2 <- sd(x[mem==2])
sigma3 <- sd(x[mem==3])
sigma4 <- sd(x[mem==4])
sigma5 <- sd(x[mem==5])
pi1 <- sum(mem==1)/length(mem)
pi2 <- sum(mem==2)/length(mem)
pi3 <- sum(mem==3)/length(mem)
pi4 <- sum(mem==4)/length(mem)
pi5 <- sum(mem==5)/length(mem)

# modified sum only considers finite values
sum.finite <- function(x) {
  sum(x[is.finite(x)])
}
Q <- 0
# starting value of expected value of the log likelihood
Q[5] <- sum.finite(log(pi1)+log(dnorm(x, mu1, sigma1))) + sum.finite(log(pi2)+log(dnorm(x, mu2, sigma2))) + sum.finite(log(pi3)+log(dnorm(x, mu3, sigma3))) + sum.finite(log(pi4)+log(dnorm(x, mu4, sigma4))) + sum.finite(log(pi5)+log(dnorm(x, mu5, sigma5)))
Q[5]
k <- 5
while (abs(Q[k]-Q[k-1])>=1e-6) {
  # E step
  comp1 <- pi1 * dnorm(x, mu1, sigma1)
  comp2 <- pi2 * dnorm(x, mu2, sigma2)
  comp3 <- pi3 * dnorm(x, mu3, sigma3)
  comp4 <- pi4 * dnorm(x, mu4, sigma4)
  comp5 <- pi5 * dnorm(x, mu5, sigma5)
  comp.sum <- comp1 + comp2 + comp3 + comp4 + comp5
  
  p1 <- comp1/comp.sum
  p2 <- comp2/comp.sum
  p3 <- comp3/comp.sum
  p4 <- comp4/comp.sum
  p5 <- comp5/comp.sum
  # M step
  pi1 <- sum.finite(p1) / length(x)
  pi2 <- sum.finite(p2) / length(x)
  pi3 <- sum.finite(p3) / length(x)
  pi4 <- sum.finite(p4) / length(x)
  pi5 <- sum.finite(p5) / length(x)
  
  mu1 <- sum.finite(p1 * x) / sum.finite(p1)
  mu2 <- sum.finite(p2 * x) / sum.finite(p2) 
  mu3 <- sum.finite(p3 * x) / sum.finite(p3)
  mu4 <- sum.finite(p4 * x) / sum.finite(p4)
  mu5 <- sum.finite(p5 * x) / sum.finite(p5)
  sigma1 <- sqrt(sum.finite(p1 * (x-mu1)^2) / sum.finite(p1))
  sigma2 <- sqrt(sum.finite(p2 * (x-mu2)^2) / sum.finite(p2))
  sigma3 <- sqrt(sum.finite(p3 * (x-mu3)^2) / sum.finite(p3))
  sigma4 <- sqrt(sum.finite(p4 * (x-mu4)^2) / sum.finite(p4))
  sigma5 <- sqrt(sum.finite(p5 * (x-mu5)^2) / sum.finite(p5))
  
  p1 <- pi1 
  p2 <- pi2
  p3 <- pi3
  p4 <- pi4
  p5 <- pi5
  
  k <- k + 1
  Q[k] <- sum(log(comp.sum))
} 

#Final parameter estimates
cat("Final Parameters:\n")
cat("Mean 1:", mu1, "\n")
cat("Mean 2:", mu2, "\n")
cat("Mean 3:", mu3, "\n")
cat("Mean 4:", mu4, "\n")
cat("Mean 5:", mu5, "\n")
cat("Sigma 1:", sigma1, "\n")
cat("Sigma 2:", sigma2, "\n")
cat("Sigma 3:", sigma3, "\n")
cat("Sigma 4:", sigma4, "\n")
cat("Sigma 5:", sigma5, "\n")
cat("Pi 1:", pi1, "\n")
cat("Pi 2:", pi2, "\n")
cat("Pi 3:", pi3, "\n")
cat("Pi 4:", pi4, "\n")
cat("Pi 5:", pi5, "\n")


hist(x, prob=T, breaks=32, xlim=c(range(x)[1], range(x)[2]), main='Histogram of the Chinese Soy Beans Futures')
lines(density(x), col="green", lwd=2)
x1 <- seq(from=range(x)[1], to=range(x)[2], length.out=1000)
y <- pi1 * dnorm(x1, mean=mu1, sd=sigma1) + pi2 * dnorm(x1, mean=mu2, sd=sigma2) + pi3 * dnorm(x1, mean=mu3, sd=sigma3) + pi4 * dnorm(x1, mean=mu4, sd=sigma4) + pi5 * dnorm(x1, mean=mu5, sd=sigma5)
lines(x1, y, col="red", lwd=2)
legend('topright', col=c("green", 'red'), lwd=2, legend=c("kernal", "fitted"))

