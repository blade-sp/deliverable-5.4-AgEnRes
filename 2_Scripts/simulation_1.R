library(MASS)

set.seed(12345)

### Monte Carlo simulation of profit variance in R

# Profit: pi = p * Y - s * N

draws <- 10000  # number of Monte Carlo draws

# Parameters 
p_mean <- 365    # mean output price [from historical data]
y_mean <- 8.89    # mean yield [Just & Pope]
s_mean <- 1.25     # mean fertilizer price [from historical data]

p_sd <- 10       # std dev of output price 
y_sd <- 1.2      # std dev of yield [Just & Pope]
s_sd <- 2        # std dev of fertilizer price

corr_p_y <- -0.25  # correlation between price and yield (natural hedge)
corr_p_s <- 0.0  # correlation between wheat price and fertilizer price
corr_y_s <- 0.0  # correlation between yield and fertilizer price

N <- 100          # fertilizer quantity (deterministic)


# Build covariance matrix for p and Y
cov_p_y <- corr_p_y * p_sd * y_sd
cov_matrix_py <- matrix(c(p_sd^2, cov_p_y, cov_p_y, y_sd^2), nrow = 2)

# Simulate (p, Y) jointly
py_draws <- mvrnorm(n = draws, mu = c(p_mean, y_mean), Sigma = cov_matrix_py)
p_sim <- py_draws[,1]
y_sim <- py_draws[,2]

# Simulate s independently
s_sim <- rnorm(draws, mean = s_mean, sd = s_sd)

# Compute profit
pi_sim <- p_sim * y_sim - s_sim * N

# Monte Carlo mean and variance
mc_mean <- mean(pi_sim)
mc_var  <- var(pi_sim)

# Sample moments for delta approximation
p_bar <- mean(p_sim); y_bar <- mean(y_sim); s_bar <- mean(s_sim)
var_p <- var(p_sim); var_y <- var(y_sim); var_s <- var(s_sim)
cov_p_y_sample <- cov(p_sim, y_sim)
cov_p_s_sample <- cov(p_sim, s_sim)
cov_y_s_sample <- cov(y_sim, s_sim)

# Linear (delta) approximation
delta_var <- (y_bar^2) * var_p +
  (p_bar^2) * var_y +
  (N^2)     * var_s +
  2 * p_bar * y_bar * cov_p_y_sample -
  2 * y_bar * N * cov_p_s_sample -
  2 * p_bar * N * cov_y_s_sample

# Exact formula recomposed from sample moments
var_pY   <- var(p_sim * y_sim)
cov_pY_s <- cov(p_sim * y_sim, s_sim)
exact_var <- var_pY + N^2 * var_s - 2 * N * cov_pY_s

# Display results
results <- data.frame(
  measure = c("MC_mean_pi", "MC_var_pi", "delta_approx_var",
              "var_pY", "exact_var_from_formula"),
  value = c(mc_mean, mc_var, delta_var, var_pY, exact_var)
)

print(results, digits = 5)


# ---- Plot profit distribution ----
hist(pi_sim, breaks = 100, probability = TRUE,
     main = "Simulated Profit Distribution",
     xlab = "Profit", col = "lightblue", border = "white")

lines(density(pi_sim), col = "red", lwd = 2)
abline(v = mc_mean, col = "darkgreen", lwd = 2, lty = 2)
legend("topright", legend = c("Density", "Mean profit"),
       col = c("red", "darkgreen"), lwd = 2, lty = c(1,2))


