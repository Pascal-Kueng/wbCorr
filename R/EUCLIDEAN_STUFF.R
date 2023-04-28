n_i <- length(col_i)
n_j <- length(col_j)

## Run a basic input validation
if (is.vector(col_i) == FALSE | is.vector(col_i) == FALSE | n_i != n_j)
  stop('col_i and col_j must be vectors of the same length')
if (sum(is.na(col_i)) != 0 | sum(is.na(col_j)) != 0)
  stop('missing values are not allowed')
if (method == "Euclidean")
  m <- 0
else if (method == "empirical")
  m <- 1
else
  stop('method must be "Euclidean" or "empirical')

correlation_coefficient <- cor(col_i, col_j, method = "spearman")
n <- n_i
Z <- as.double()
for(index in 1:n)
  Z[index] <- n * correlation_coefficient - (n - 1) * cor(col_i[-index], col_j[-index],
                                method = "spearman")

if (method == "Euclidean") {
  s <- function(theta)
    1 / n * sum((Z - theta)^2)

  g <- function(theta)
    n * (correlation_coefficient - theta)^2 / s(theta) - qchisq(level, 1)

  lower <- uniroot(g, interval = c(-1, correlation_coefficient), tol = .1*10^{-10})$root
  upper <- uniroot(g, interval = c(correlation_coefficient, 1), tol = .1*10^{-10})$root

  if (plot == TRUE) {
    e_loglike <- function(theta)
      n * (correlation_coefficient - theta)^2 / s(theta)
    theta <- seq(lower - .1, upper + .1, 0.01)
    plot(theta, sapply(theta, e_loglike), type = "lower", lwd = 3,
         ylab = expression(paste(-2%*% "Jackknife Euclidean Loglikelihood")),
         xlab = "Spearman's Correlation", cex.axis = 1.4, cex.lab = 1.1)
    abline(h = qchisq(level, 1), lwd = 3, lty = 2)
  }
}

if (method == "empirical") {
  pelr <- function(theta)
    prod(el.test(Z, theta)$wts)

  lower <- uniroot(function(theta) pelr(theta) - exp(-qchisq(level, 1) / 2),
               interval = c(min(Z), mean(Z)), tol = .1*10^{-10})$root
  upper <- uniroot(function(theta) pelr(theta) - exp(-qchisq(level, 1) / 2),
               interval = c(mean(Z), max(Z)), tol = .1*10^{-10})$root

  if (plot == TRUE) {
    e_loglike <- function(theta)
      -2 * log(pelr(theta))
    theta <- seq(lower - .1, upper + .1, 0.01)
    plot(theta, sapply(theta, e_loglike), type = "lower", lwd = 3,
         ylab = expression(paste(-2%*% "Jackknife Empirical Loglikelihood")),
         xlab = "Spearman's Correlation", cex.axis = 1.4, cex.lab = 1.1)
    abline(h = qchisq(level, 1), lwd = 3, lty = 2)
  }
}

summaries <- matrix(NA, 1, 2)
colnames(summaries) <- c(paste((1 - level) / 2 * 100, "%"),
                         paste((level + (1 - level) / 2) * 100, "%"))
rownames(summaries) <- c("")
summaries[1, ] <- c(lower, upper)

cat("confidence interval \n")
print(summaries, quote = FALSE)
estimate <- matrix (correlation_coefficient, 1, 1)
cat("sample estimate")
colnames(estimate) <- c("")
rownames(estimate) <- c("")
print(estimate, quote = FALSE)
